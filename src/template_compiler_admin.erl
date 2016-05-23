%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Administrate all compiled templates and compilers in flight.

%% Copyright 2016 Marc Worrell
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

-module(template_compiler_admin).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(gen_server).

-export([
    lookup/3,
    changed/1
    ]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).


-include("template_compiler.hrl").

-record(state, {
        compiled :: ets:tab(),
        compiling = [] :: list({binary(), pid()}),
        waiting = [] :: list({binary(), any()})
    }).



%%% ---------------------- API ----------------------


%% @doc Find a template, start a compilation if not found
-spec lookup(binary(), template_compiler:options(), any()) -> {ok, atom()} | {error, term()}.
lookup(Template, Options, Context) ->
    case ets:lookup(?MODULE, Template) of
        [#tpl{module=Mod}] ->
            {ok, Mod};
        [] ->
            case gen_server:call(?MODULE, {compile, Template, Options, Context}) of
                {ok, {compile, Key}} ->
                    % Unknown, compile the template
                    Result = template_compiler:compile_file(Template, Options, Context),
                    gen_server:call(?MODULE, {compile_ready, Result, Key});
                {ok, compiling} ->
                    gen_server:call(?MODULE, {wait, Template, Options, Context});
                {ok, Module} ->
                    {ok, Module}
            end
    end.

%% @doc Ping that a template has been changed
-spec changed(binary()) -> ok.
changed(Template) ->
    gen_server:cast(?MODULE, {changed, Template}). 


-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%% ---------------------- Callbacks ----------------------

init([]) ->
    Compiled = ets:new(?MODULE, [set, {keypos, #tpl.template}, named_table, protected]),
    #state{
        compiled = Compiled,
        compiling = [],
        waiting = []
    }.

handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

code_change(_FromVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% ---------------------- Internal  ----------------------

