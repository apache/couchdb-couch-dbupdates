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


-module(couch_dbdelete_sup).


-behaviour(supervisor).


-export([
    start_link/0
]).


-export([
    init/1
]).


-include("couch_dbupdates.hrl").


start_link() ->
    Arg = case fabric2_node_types:is_type(background_db_deletion) of
          true -> normal;
          false -> deletion_disabled
        end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, Arg).


init(normal) ->
    Children = [
        #{
            id => couch_dbdelete_server,
            start => {couch_dbdelete_server, start_link, []}
        }
    ],
    {ok, {flags(), Children}};

init(deletion_disabled) ->
    couch_log:notice("~p : database background deletion disabled", [?MODULE]),
    couch_jobs:set_type_timeout(?DB_DELETE_JOB_TYPE, 6),
    {ok, {flags(), []}}.


flags() ->
    #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    }.
