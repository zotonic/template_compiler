%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2023 Marc Worrell
%% @doc Supervisor for the template compiler and administration.
%% @end

%% Copyright 2016-2023 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(template_compiler_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Children = [
        {template_compiler_admin, {template_compiler_admin, start_link, []},
         permanent, 5000, worker, [template_compiler_admin]}
    ],
    {ok, { {one_for_all, 1000, 10}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
