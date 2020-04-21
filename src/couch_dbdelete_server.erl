% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_dbdelete_server).


-behaviour(gen_server).


-export([
    start_link/0,
    process_loop/0
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-include("couch_dbupdates.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    process_flag(trap_exit, true),
    proc_lib:spawn_link(?MODULE, process_loop, []),
    couch_jobs:set_type_timeout(?DB_DELETE_JOB_TYPE, 6),
    St = #{
        workers => #{},
        max_workers => max_workers()
    },
    {ok, spawn_workers(St)}.


terminate(_, _St) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'EXIT', Pid, Reason}, St) ->
    #{workers := Workers} = St,
    case maps:is_key(Pid, Workers) of
        true ->
            if Reason == normal -> ok; true ->
                LogMsg = "~p : indexer process ~p exited with ~p",
                couch_log:error(LogMsg, [?MODULE, Pid, Reason])
            end,
            NewWorkers = maps:remove(Pid, Workers),
            {noreply, spawn_workers(St#{workers := NewWorkers})};
        false ->
            LogMsg = "~p : unknown process ~p exited with ~p",
            couch_log:error(LogMsg, [?MODULE, Pid, Reason]),
            {stop, {unknown_pid_exit, Pid}, St}
    end;

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


process_loop() ->
    case is_enabled() of
        true ->
            process_delete();
        false ->
            ok
    end,
    SleepSec = sleep_sec(),
    timer:sleep(SleepSec * 1000),
    process_loop().


process_delete() ->
    {ok, Infos} = fabric2_db:list_deleted_dbs_info(),
    lists:foreach(fun(Info) ->
        DbName = proplists:get_value(db_name, Info),
        DeletedWhen = proplists:get_value(timestamp, Info),
        Now = now_sec(),
        Retention = retention_sec(),
        Since = Now - Retention,
        case Since > timestamp_to_sec(DeletedWhen)  of
            true ->
                JobId = job_id(DbName, DeletedWhen),
                JobData = job_data(DbName, DeletedWhen),
                ok = couch_jobs:add(
                    undefined,
                    ?DB_DELETE_JOB_TYPE,
                    JobId,
                    JobData
                );
            false ->
                ok
        end
    end, Infos).


job_id(DbName, Timestamp) ->
    <<DbName/binary, "-", Timestamp/binary>>.


job_data(DbName, Timestamp) ->
    #{
        db_name => DbName,
        timestamp => Timestamp
    }.


now_sec() ->
    Now = os:timestamp(),
    Nowish = calendar:now_to_universal_time(Now),
    calendar:datetime_to_gregorian_seconds(Nowish).


timestamp_to_sec(TimeStamp) ->
    <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary,
        "T",
        Hour:2/binary, ":", Minutes:2/binary, ":", Second:2/binary,
        "Z">> = TimeStamp,

    calendar:datetime_to_gregorian_seconds(
        {{?btoi(Year), ?btoi(Month), ?btoi(Day)},
        {?btoi(Hour), ?btoi(Minutes), ?btoi(Second)}}
    ).


spawn_workers(St) ->
    #{
        workers := Workers,
        max_workers := MaxWorkers
    } = St,
    case maps:size(Workers) < MaxWorkers of
        true ->
            Pid = couch_dbdelete_worker:spawn_link(),
            NewSt = St#{workers := Workers#{Pid => true}},
            spawn_workers(NewSt);
        false ->
            St
    end.


max_workers() ->
    config:get_integer("couch", "max_db_delete_workers", ?MAX_WORKERS).


is_enabled() ->
    config:get_boolean("couch", "db_delete_enabled", true).


retention_sec() ->
    config:get_integer("couch", "db_delete_retention_sec",
        ?DEFAULT_RETENTION_SEC).


sleep_sec() ->
    config:get_integer("couch", "db_delete_sleep_sec",
        ?DEFAULT_SLEEP_SEC).
