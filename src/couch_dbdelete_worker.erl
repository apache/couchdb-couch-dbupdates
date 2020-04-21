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

-module(couch_dbdelete_worker).

-export([
    spawn_link/0
]).


-export([
    init/0
]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-include("couch_dbupdates.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("fabric/include/fabric2.hrl").


spawn_link() ->
    proc_lib:spawn_link(?MODULE, init, []).


init() ->
    {ok, Job, JobData} = couch_jobs:accept(?DB_DELETE_JOB_TYPE, #{}),
    #{
        <<"db_name">> := DbName,
        <<"timestamp">> := TimeStamp
    } = JobData,

    ok = fabric2_db:delete(DbName, [{deleted_at, TimeStamp}]),

    couch_jobs:finish(undefined, Job, JobData#{
        message => db_deleted,
        reason => "Database was deleted"
    }),
    exit(normal).
