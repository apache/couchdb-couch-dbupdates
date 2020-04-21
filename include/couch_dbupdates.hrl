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

% jobs api
-define(DB_DELETE_JOB_TYPE, <<"dbdelete">>).

% number of worker for db background deletion
-define(MAX_WORKERS, 10).

% settings for background deletion interval
-define(DEFAULT_RETENTION_SEC, 172800). % 48 hours
-define(DEFAULT_SLEEP_SEC, 3600). % 1 hour

% type conversion
-define(btoi(V), binary_to_integer(V)).
