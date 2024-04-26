%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2023 Marc Worrell
%% @doc Callback routines for compiled templates.
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

-module(template_compiler_runtime_internal).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    forloop/9,
    with_vars/3,
    block_call/6,
    block_inherit/7,
    include/9,
    call/4,
    print/1,
    unique/0
    ]).

-include_lib("kernel/include/logger.hrl").


%% @doc Runtime implementation of a forloop.
-spec forloop(IsForloopVar :: boolean(), ListExpr :: term(), LoopVars :: [atom()],
              LoopBody :: fun(), EmptyPart :: fun(),
              Runtime :: atom(), IsContextVars :: boolean(),
              Vars :: map(), Context :: term()) -> term().
forloop(IsLoopVar, ListExpr, Idents, BodyFun, EmptyFun, Runtime, IsContextVars, Vars, Context) ->
    case forloop_to_list(ListExpr, length(Idents), Runtime, Context) of
        [] ->
            EmptyFun();
        List when IsLoopVar ->
            forloop_fold(List, Idents, BodyFun, Runtime, IsContextVars, Vars, Context);
        List when not IsLoopVar ->
            forloop_map(List, Idents, BodyFun, Runtime, IsContextVars, Vars, Context)
    end.

forloop_to_list(undefined, _, _Runtime, _Context) ->
    [];
forloop_to_list(V, 1, _Runtime, _Context)
    when is_map(V);
         is_number(V);
         is_atom(V);
         is_binary(V) ->
    [ V ];
forloop_to_list(ListExpr, _NVars, Runtime, Context) ->
    Runtime:to_list(ListExpr, Context).


% For loop with a forloop variable in the body, use a fold with a forloop state
% variable.
forloop_fold(List, Idents, Fun, Runtime, IsContextVars, Vars, Context) ->
    Len = length(List),
    {Result, _} = lists:foldl(
            fun(Val, {Acc, Counter}) ->
                Forloop = #{
                    counter => Counter,
                    counter0 => Counter-1,
                    revcounter => Len - Counter + 1,
                    revcounter0 => Len - Counter,
                    first => Counter =:= 1,
                    last => Counter =:= Len,
                    parentloop => maps:get(forloop, Vars, undefined)
                },
                Vars1 = assign_vars(Idents, Val, Vars#{forloop => Forloop}),
                Context1 = case IsContextVars of
                                true -> Runtime:set_context_vars(Vars1, Context);
                                false -> Context
                           end,
                {[Fun(Vars1, Context1) | Acc], Counter+1}
            end,
            {[], 1},
            List),
    lists:reverse(Result).

% For loop without any forloop variable, use a direct map
forloop_map(List, Idents, Fun, Runtime, true, Vars, Context) ->
    [
        begin
            Vars1 = assign_vars(Idents, Val, Vars),
            Fun(Vars1, Runtime:set_context_vars(Vars1, Context))
        end || Val <- List
    ];
forloop_map(List, Idents, Fun, _Runtime, false, Vars, Context) ->
    [ Fun(assign_vars(Idents, Val, Vars), Context) || Val <- List ].


%% @doc Used with forloops, assign variables from an expression value
assign_vars([V], E, Vars) ->
    Vars#{V => E};
assign_vars(Vs, L, Vars) when is_list(L) ->
    assign_vars_list(Vs, L, Vars);
assign_vars(Vs, T, Vars) when is_tuple(T) ->
    assign_vars_tuple(Vs, T, Vars);
assign_vars([V|Vs], E, Vars) ->
    assign_vars_list(Vs, [], Vars#{V => E}).

assign_vars_list([], _, Vars) ->
    Vars;
assign_vars_list([V|Vs], [], Vars) ->
    assign_vars_list(Vs, [], Vars#{ V => undefined });
assign_vars_list([V|Vs], [E|Es], Vars) ->
    assign_vars_list(Vs, Es, Vars#{V => E}).

assign_vars_tuple([V1], {E1}, Vars) ->
    Vars#{V1 => E1};
assign_vars_tuple([V1,V2], {E1,E2}, Vars) ->
    Vars#{V1 => E1, V2 => E2};
assign_vars_tuple([V1,V2,V3], {E1,E2,E3}, Vars) ->
    Vars#{V1 => E1, V2 => E2, V3 => E3};
assign_vars_tuple([V1,V2,V3,V4], {E1,E2,E3,E4}, Vars) ->
    Vars#{V1 => E1, V2 => E2, V3 => E3, V4 => E4};
assign_vars_tuple(Vs, Es, Vars) ->
    assign_vars_list(Vs, tuple_to_list(Es), Vars).


%% @doc Assign variables from a with statement. Care has to be taken for unpacking tuples and lists.
-spec with_vars([atom()], [term()], map()) -> map().
with_vars([_,_|_] = Vs, [E], Vars) ->
    assign_vars(Vs, E, Vars);
with_vars(Vs, Es, Vars) ->
    assign_vars_list(Vs, Es, Vars).


%% @doc Call the block function, lookup the function in the BlockMap to find
%%      the correct module.
-spec block_call({binary(), integer(), integer()}, atom(), map(), map(), atom(), term()) -> term().
block_call(SrcPos, Block, Vars, BlockMap, Runtime, Context) ->
    case maps:find(Block, BlockMap) of
        {ok, [Module|_]} when is_atom(Module) ->
            case Runtime:trace_block(SrcPos, Block, Module, Context) of
                ok ->
                    Module:render_block(Block, Vars, BlockMap, Context);
                {ok, Before, After} ->
                    [
                        Before,
                        Module:render_block(Block, Vars, BlockMap, Context),
                        After
                    ]
            end;
        error ->
            % No such block, return empty data.
            <<>>
    end.

%% @doc Call the block function of the template the current module extends.
-spec block_inherit({binary(), integer(), integer()}, atom(), atom(), map(), map(), atom(), term()) -> term().
block_inherit(SrcPos, Module, Block, Vars, BlockMap, Runtime, Context) ->
    case maps:find(Block, BlockMap) of
        {ok, Modules} ->
            case lists:dropwhile(fun(M) -> M =/= Module end, Modules) of
                [Module, Next|_] ->
                    case Runtime:trace_block(SrcPos, Block, Next, Context) of
                        ok ->
                            Next:render_block(Block, Vars, BlockMap, Context);
                        {ok, Before, After} ->
                            [
                                Before,
                                Next:render_block(Block, Vars, BlockMap, Context),
                                After
                            ]
                    end;
                _ ->
                    <<>>
            end;
        error ->
            % No such block, return empty data.
            <<>>
    end.


%% @doc Include a template.
-spec include({File::binary(), Line::integer(), Col::integer()}, normal|optional|all,
        template_compiler:template(), list({atom(),term()}), atom(), list(binary()), boolean(), map(), term()) ->
        template_compiler:render_result().
include(SrcPos, Method, Template, Args, Runtime, ContextVars, IsContextVars, Vars, Context) ->
    Vars1 = lists:foldl(
                fun
                    ({'$cat', [Cat|_] = E}, Acc) when is_atom(Cat); is_binary(Cat); is_list(Cat) ->
                        Acc#{ '$cat' => E };
                    ({'$cat', E}, Acc) ->
                        Acc#{
                            'id' => E,
                            '$cat' => E
                        };
                    ({V,E}, Acc) ->
                        Acc#{ V => E }
                end,
                Vars,
                Args),
    Context1 = case IsContextVars of
        true -> Runtime:set_context_vars(Args, Context);
        false -> Context
    end,
    include_1(SrcPos, Method, Template, Runtime, ContextVars, Vars1, Context1).

include_1(SrcPos, all, Template, Runtime, ContextVars, Vars1, Context) ->
    Templates = Runtime:map_template_all(Template, Vars1, Context),
    lists:map(
            fun(Tpl) ->
                include_1(SrcPos, optional, Tpl, Runtime, ContextVars, Vars1, Context)
            end,
            Templates);
include_1(SrcPos, Method, Template, Runtime, ContextVars, Vars1, Context) ->
    {SrcFile, SrcLine, _SrcCol} = SrcPos,
    Options = [
        {runtime, Runtime},
        {trace_position, SrcPos},
        {context_vars, ContextVars}
    ],
    case template_compiler:render(Template, Vars1, Options, Context) of
        {ok, Result} ->
            Result;
        {error, enoent} when Method =:= normal ->
            ?LOG_ERROR(#{
                text => <<"Included template not found">>,
                template => Template,
                srcpos => SrcPos,
                result => error,
                reason => enoent,
                at => SrcFile,
                line => SrcLine
            }),
            <<>>;
        {error, enoent} ->
            <<>>;
        {error, Err} when is_map(Err) ->
            ?LOG_ERROR(Err),
            <<>>;
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => <<"Template render error">>,
                template => Template,
                srcpos => SrcPos,
                result => error,
                reason => Reason,
                at => SrcFile,
                line => SrcLine
            }),
            <<>>
    end.


%% @doc Call a module's render function.
-spec call(Module::atom(), Args::map(), Vars::map(), Context::term()) -> template_compiler:render_result().
call(Module, Args, Vars, Context) ->
    case Module:render(Args, Vars, Context) of
        {ok, Result} -> Result;
        {error, _} -> <<>>
    end.


%% @doc Echo the HTML escape value within &lt;pre&gt; tags.
-spec print(term()) -> iolist().
print(Expr) ->
    V = io_lib:format("~p", [Expr]),
    [
        <<"<pre>">>,
        z_html:escape(iolist_to_binary(V)),
        <<"</pre>">>
    ].


%% @doc Make an unique string (about 11 characters). Used for expanding unique args in templates. The string only
%%      consists of the characters A-Z and 0-9 and is safe to use as HTML element id.
-spec unique() -> binary().
unique() ->
    <<"u", (integer_to_binary(rand_uniform(100000000000000000), 36))/binary>>.

-spec rand_uniform( pos_integer() ) -> non_neg_integer().
rand_uniform(N) ->
    rand:uniform(N) - 1.
