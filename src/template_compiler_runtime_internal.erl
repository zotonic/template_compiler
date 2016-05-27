%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Callback routines for compiled templates.

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

-module(template_compiler_runtime_internal).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    forloop/8,
    with_vars/3,
    block_call/4,
    block_inherit/5,
    include/6,
    unique/0
    ]).


%% @doc Runtime implementation of a forloop. Two variations: one with 
-spec forloop(IsForloopVar :: boolean(), ListExpr :: term(), LoopVars :: [atom()],
              LoopBody :: fun(), EmptyPart :: fun(),
              Runtime :: atom(), Vars :: #{}, Context :: term()) -> term().
forloop(IsLoopVar, ListExpr, Idents, BodyFun, EmptyFun, Runtime, Vars, Context) ->
    case Runtime:to_list(ListExpr, Context) of
        [] ->
            EmptyFun();
        List when IsLoopVar ->
            forloop_fold(List, Idents, BodyFun, Vars);
        List when not IsLoopVar ->
            forloop_map(List, Idents, BodyFun, Vars)
    end.

% For loop with a forloop variable in the body, use a fold with a forloop state
% variable.
forloop_fold(List, Idents, Fun, Vars) ->
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
                {[Fun(Vars1) | Acc], Counter+1}
            end,
            {[], 1},
            List),
    lists:reverse(Result).

% For loop without any forloop variable, use a direct map
forloop_map(List, Idents, Fun, Vars) ->
    [ Fun(assign_vars(Idents, Val, Vars)) || Val <- List ].


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
-spec with_vars([atom()], [term()], #{}) -> #{}.
with_vars([_,_|_] = Vs, [E], Vars) ->
    assign_vars(Vs, E, Vars);
with_vars(Vs, Es, Vars) ->
    assign_vars_list(Vs, Es, Vars).


%% @doc Call the block function, lookup the function in the BlockMap to find
%%      the correct module.
-spec block_call(atom(), #{}, #{}, term()) -> term().
block_call(Block, Vars, BlockMap, Context) ->
    case maps:find(Block, BlockMap) of
        {ok, [Module|_]} when is_atom(Module) ->
            Module:render_block(Block, Vars, BlockMap, Context);
        error ->
            % No such block, return empty data.
            <<>>
    end.

%% @doc Call the block function of the template the current module extends.
-spec block_inherit(atom(), atom(), #{}, #{}, term()) -> term().
block_inherit(Module, Block, Vars, BlockMap, Context) ->
    case maps:find(Block, BlockMap) of
        {ok, Modules} ->
            case lists:dropwhile(fun(M) -> M =/= Module end, Modules) of
                [Module, Next|_] ->
                    Next:render_block(Block, Vars, BlockMap, Context);
                _ ->
                    <<>>
            end;
        error ->
            % No such block, return empty data.
            <<>>
    end.


%% @doc Include a template.
%% @todo Add support for 'all' option
-spec include(normal|optional|all, template_compiler:template(), list({atom(),term()}), atom(), #{}, term()) -> 
        template_compiler:render_result().
include(Method, Template, Args, Runtime, Vars, Context) ->
    Vars1 = lists:foldl(
                fun({V,E}, Acc) ->
                    Acc#{V => E}
                end,
                Vars,
                Args),
    case template_compiler:render(Template, Vars1, [{runtime, Runtime}], Context) of
        {ok, Result} ->
            Result;
        {error, notfound} when Method =/= optional ->
            lager:error("Missing included template ~p", [Template]),
            <<>>;
        {error, _} ->
            <<>>
    end.


%% @doc Make an unique string (about 11 characters). Used for expanding unique args in templates. The string only 
%%      consists of the characters a-z and 0-9 and is safe to use as HTML element id.
-spec unique() -> string().
unique() ->
    list_to_binary([ $u | string:to_lower(integer_to_list(crypto:rand_uniform(0,100000000000000000), 36)) ]).
