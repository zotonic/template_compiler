%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2020 Marc Worrell
%% @doc Administrate all compiled templates and compilers in flight.

%% Copyright 2016-2020 Marc Worrell
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
    flush/0,
    flush_file/1,
    flush_context_name/1
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

-include_lib("kernel/include/logger.hrl").
-include("template_compiler.hrl").
-include("template_compiler_internal.hrl").

-type gen_server_from() :: {pid(),term()}.

-record(tpl, {
        key :: template_compiler:template_key(),
        module :: atom()
    }).

-record(state, {
        compiled :: ets:tab(),
        compiling = [] :: list({reference(), template_compiler:template_key(), gen_server_from()}),
        waiting = [] :: list({template_compiler:template_key(), gen_server_from()}),
        filename_keys = [] :: list({file:filename_all(), template_compiler:template_key()})
    }).


%%% ---------------------- API ----------------------


%% @doc Find a template, start a compilation if not found
-spec lookup(file:filename_all(), template_compiler:options(), any()) -> {ok, atom()} | {error, term()}.
lookup(Filename, Options, Context) ->
    Runtime = template_compiler:get_option(runtime, Options),
    ContextName = Runtime:get_context_name(Context),
    TplKey = {ContextName, Runtime, Filename},
    case ets:lookup(?MODULE, TplKey) of
        [#tpl{module=Module}] ->
            case Runtime:is_modified(Filename, Module:mtime(), Context) of
                true -> compile_file(Filename, TplKey, Options, Context);
                false -> {ok, Module}
            end;
        [] ->
            compile_file(Filename, TplKey, Options, Context)
    end.

compile_file(Filename, TplKey, Options, Context) ->
    case gen_server:call(?MODULE, {compile_request, TplKey, Filename}, infinity) of
        {ok, {compile, TplKey}} ->
            % Unknown, compile the template
            Result = try
                        template_compiler:compile_file(Filename, Options, Context)
                     catch
                        What:Error:Stack ->
                            % io:format("Error compiling template ~p: ~p:~n~p at~n ~p~n",
                            %           [Filename, What, Error, Stack]),
                            ?LOG_ERROR(
                                "Error compiling template ~p: ~p: ~p",
                                [Filename, What, Error],
                                #{
                                    template => Filename,
                                    stack => Stack
                                }),
                            {error, Error}
                     end,
            ok = gen_server:call(?MODULE, {compile_done, Result, TplKey}, infinity),
            Result;
        {ok, Module} when is_atom(Module) ->
            {ok, Module};
        {error, _} = Error ->
            Error
    end.

%% @doc Flush all template mappings
-spec flush() -> ok.
flush() ->
    gen_server:cast(?MODULE, flush).

%% @doc Ping that a template has been changed
-spec flush_file(file:filename_all()) -> ok.
flush_file(Filename) ->
    gen_server:cast(?MODULE, {flush_file, Filename}).

%% @doc Ping that a context has changed
-spec flush_context_name(ContextName::term()) -> ok.
flush_context_name(ContextName) ->
    gen_server:cast(?MODULE, {flush_context_name, ContextName}).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%% ---------------------- Callbacks ----------------------

init([]) ->
    Compiled = ets:new(?MODULE, [set, {keypos, #tpl.key}, named_table, protected]),
    {ok, #state{
        compiled = Compiled,
        compiling = [],
        waiting = [],
        filename_keys = []
    }}.

handle_call({compile_request, TplKey, Filename}, From, State) ->
    case is_compiling(TplKey, State) of
        true ->
            {noreply, wait_for_compile(TplKey, From, State)};
        false ->
            FTpl = {Filename, TplKey},
            State1 = case lists:member(FTpl, State#state.filename_keys) of
                        false -> 
                            State#state{
                                filename_keys=[ FTpl | State#state.filename_keys ]
                            };
                        true ->
                            State
                    end,
            {noreply, start_compile(TplKey, From, State1)}
    end;
handle_call({compile_done, Result, TplKey}, _From, State) ->
    State1 = case lists:keytake(TplKey, 2, State#state.compiling) of
                {value, {MRef, _TplKey, _CFrom}, Compiling1} ->
                    erlang:demonitor(MRef),
                    State#state{compiling=Compiling1};
                false ->
                    State
             end,
    case Result of
        {ok, Module} ->
            ets:insert(?MODULE, #tpl{key=TplKey, module=Module});
        {error, _} ->
            ok
    end,
    {Waiters, State2} = split_waiters(TplKey, State1),
    lists:foreach(
            fun({_TplKey, Waiter}) ->
                gen_server:reply(Waiter, Result)
            end,
            Waiters),
    {reply, ok, State2};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast(flush, State) ->
    true = ets:delete_all_objects(?MODULE),
    State1 = State#state{
        filename_keys = []
    },
    {noreply, State1};
handle_cast({flush_file, Filename}, State) ->
    {ChangedKeys, FnKeys} = lists:partition(
                                fun ({F,_}) ->
                                    F =:= Filename
                                end,
                                State#state.filename_keys),
    lists:foreach(
            fun({_,TplKey}) ->
                ets:delete(?MODULE, TplKey)
            end,
            ChangedKeys),
    {noreply, State#state{filename_keys=FnKeys}};
handle_cast({flush_context_name, ContextName}, State) ->
    Matched = ets:foldl(
                    fun
                        (#tpl{key={Ctx, _, _}} = Tpl, Acc) when Ctx =:= ContextName ->
                            [Tpl|Acc];
                        (_, Acc) ->
                            Acc
                    end,
                    [],
                    ?MODULE),
    % TODO: add module to list of modules that might be purged
    lists:foreach(
            fun(#tpl{key=Key}) ->
                ets:delete(?MODULE, Key)
            end,
            Matched),
    FilenameKeys = lists:filter(
                        fun({_Fn, K}) -> 
                            K =/= ContextName
                        end,
                        State#state.filename_keys),
    {noreply, State#state{filename_keys=FilenameKeys}};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info({'DOWN', MRef, process, _Pid, Reason}, State) ->
    case lists:keytake(MRef, 1, State#state.compiling) of
        {value, {_MRef, TplKey, _From}, Compiling1} ->
            ?LOG_ERROR("[template_compiler] Process compiling ~p down (reason ~p), restarting other waiter",
                        [TplKey, Reason]),
            {noreply, restart_compile(TplKey, State#state{compiling=Compiling1})};
        false ->
            {noreply, State}
    end;
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

code_change(_FromVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% ---------------------- Internal  ----------------------

is_compiling(TplKey, State) ->
    lists:keymember(TplKey, 1, State#state.waiting)
    orelse lists:keymember(TplKey, 2, State#state.compiling).

wait_for_compile(TplKey, From, State) ->
    State#state{waiting=[{TplKey,From} | State#state.waiting]}.

start_compile(TplKey, From, State) ->
    State1 = wait_for_compile(TplKey, From, State),
    restart_compile(TplKey, State1).

restart_compile(TplKey, State) ->
    case lists:keytake(TplKey, 1, State#state.waiting) of
        {value, {_,{Pid, _} = From}, Waiting} ->
            MRef = erlang:monitor(process, Pid),
            gen_server:reply(From, {ok, {compile, TplKey}}),
            State#state{waiting=Waiting, compiling=[{MRef,TplKey,From}|State#state.compiling]};
        false ->
            % Drop this request, no one is waiting anymore
            State
    end.

split_waiters(TplKey, State) ->
    {Ready,Waiting} = lists:partition(
                                fun({K,_From}) ->
                                    K =:= TplKey
                                end,
                                State#state.waiting),
    {Ready, State#state{waiting=Waiting}}.

