%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Compile expressions to erl_syntax trees.

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

-module(template_compiler_expr).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    compile/3
    ]).

-include_lib("syntax_tools/include/merl.hrl").
-include("template_compiler.hrl").


-spec compile(element(), #cs{}, #ws{}) -> {#ws{}, erl_syntax:syntaxTree()}.
compile(true, _CState, Ws) ->
    {Ws, erl_syntax:atom(true)};
compile(false, _CState, Ws) ->
    {Ws, erl_syntax:atom(true)};
compile(undefined, _CState, Ws) ->
    {Ws, erl_syntax:atom(undefined)};
compile({string_literal, _SrcPos, Text}, _CState, Ws) when is_binary(Text) ->
    {Ws, erl_syntax:abstract(Text)};
compile({trans_literal, _SrcPos, {trans, _} = Tr}, #cs{runtime=Runtime} = CState, Ws) ->
    Ast = erl_syntax:application(
            erl_syntax:atom(Runtime),
            erl_syntax:atom(lookup_translation),
            [
                erl_syntax:abstract(Tr),
                erl_syntax:variable(CState#cs.vars_var),
                erl_syntax:variable(CState#cs.context_var)
            ]),
    {Ws, Ast};
compile({number_literal, _SrcPos, Nr}, _CState, Ws) ->
    {Ws, erl_syntax:abstract(z_convert:to_integer(Nr))};
compile({atom_literal, _SrcPos, Atom}, _CState, Ws) ->
    {Ws, erl_syntax:abstract(template_compiler_utils:to_atom(Atom))};
compile({find_value, LookupList}, CState, Ws) ->
    find_value_lookup(LookupList, CState, Ws);
compile({auto_id, {{identifier, _, Name}, {identifier, _, Var}}}, #cs{runtime=Runtime} = CState, Ws) ->
    VarName = erl_syntax:atom(template_compiler_utils:to_atom(Var)),
    Ast = ?Q(["[ ",
                "maps:get('$autoid', _@vars),",
                "$-, _@Name@,",
                "$-, z_convert:to_binary(",
                        "'@Runtime@':find_value(_@VarName, _@vars, _@vars, _@context)"
                    ")"
              "]"],
              [
                {context, erl_syntax:variable(CState#cs.context_var)},
                {vars, erl_syntax:variable(CState#cs.vars_var)}
              ]),
    {Ws#ws{is_autoid_var=true}, Ast};
compile({auto_id, {identifier, _, Name}}, #cs{vars_var=Vars}, Ws) ->
    VarsAst = erl_syntax:variable(Vars),
    Ast = ?Q("[ maps:get('$autoid', _@VarsAst), $-, _@Name@ ]"),
    {Ws#ws{is_autoid_var=true}, Ast};
compile({tuple_value, {identifier, _, Name}, Args}, Cs, Ws) ->
    TupleName = erl_syntax:atom(template_compiler_utils:to_atom(Name)),
    {WsProps, PropsAst} = proplist_ast(Args, Cs, Ws),
    Ast = ?Q("{ _@TupleName, _@PropsAst }"),
    {WsProps, Ast};
compile({list_value, Exprs}, Cs, Ws) ->
    list_ast(Exprs, Cs, Ws);
compile({expr, Op, Arg}, #cs{runtime=Runtime} = CState, Ws) when is_atom(Op) ->
    {Ws1, ArgAst} = compile(Arg, CState, Ws),
    Ast = ?Q("template_compiler_operators:'@Op@'(_@ArgAst, _@runtime, _@context)",
             [
                {context, erl_syntax:variable(CState#cs.context_var)},
                {runtime, erl_syntax:atom(Runtime)}
            ]),
    {Ws1, Ast};
compile({expr, Op, Arg1, Arg2}, #cs{runtime=Runtime} = CState, Ws) when is_atom(Op) ->
    {Ws1, Arg1Ast} = compile(Arg1, CState, Ws),
    {Ws2, Arg2Ast} = compile(Arg2, CState, Ws1),
    Ast = ?Q("template_compiler_operators:'@Op@'(_@Arg1Ast, _@Arg2Ast, _@runtime, _@context)",
             [
                {context, erl_syntax:variable(CState#cs.context_var)},
                {runtime, erl_syntax:atom(Runtime)}
            ]),
    {Ws2, Ast};
compile({apply_filter, Expr, {filter, {identifier, _, <<"default">>}, [Arg]}}, CState, Ws) ->
    filter_default(Expr, Arg, CState, Ws);
compile({apply_filter, Expr, {filter, {identifier, _, <<"default_if_none">>}, [Arg]}}, CState, Ws) ->
    filter_default_if_none(Expr, Arg, CState, Ws);
compile({apply_filter, Expr, {filter, {identifier, _, <<"default_if_undefined">>}, [Arg]}}, CState, Ws) ->
    filter_default_if_none(Expr, Arg, CState, Ws);
compile({apply_filter, Expr, {filter, {identifier, _, Filter}, FilterArgs}}, CState, Ws) ->
    FilterName = template_compiler_utils:to_atom(Filter),
    FilterModule = template_compiler_utils:to_atom(<<"filter_", Filter/binary>>),
    {Ws1, ExprAst} = compile(Expr, CState, Ws),
    {Ws2, AstList} = list_1(FilterArgs, CState, Ws1, []),
    Args = [ExprAst | AstList ] ++ [erl_syntax:variable(CState#cs.context_var)],
    Ast = erl_syntax:application(
                         erl_syntax:atom(FilterModule),
                         erl_syntax:atom(FilterName),
                         Args),
    {Ws2, Ast}.


find_value_lookup([{identifier, _SrcPos, <<"now">>}], _CState, Ws) ->
    Ast = ?Q("erlang:universaltime()"),
    {Ws, Ast};
find_value_lookup([{identifier, _SrcPos, Var} = Idn], #cs{runtime=Runtime, vars_var=Vars} = CState, Ws) ->
    VarName = template_compiler_utils:to_atom(Var),
    Ast = ?Q("'@Runtime@':find_value(_@VarName@, _@vars, _@vars, _@context)",
            [
                {context, erl_syntax:variable(CState#cs.context_var)},
                {vars, erl_syntax:variable(Vars)}
            ]),
    {maybe_forloop_var(Ws, Idn), Ast};
find_value_lookup(ValueLookup, #cs{runtime=Runtime, vars_var=Vars} = CState, Ws) ->
    {Ws1, ValueLookupAsts} = value_lookup_asts(ValueLookup, CState, Ws, []),
    ListAst = erl_syntax:list(ValueLookupAsts),
    Ast = ?Q("'@Runtime@':find_nested_value(_@ListAst, _@vars, _@context)",
            [
                {context, erl_syntax:variable(CState#cs.context_var)},
                {vars, erl_syntax:variable(Vars)}
            ]),
    {maybe_forloop_var(Ws1, hd(ValueLookup)), Ast}.

maybe_forloop_var(Ws, {identifier, _, <<"forloop">>}) ->
    Ws#ws{is_forloop_var=true};
maybe_forloop_var(Ws, _) ->
    Ws.

value_lookup_asts([], _CState, Ws, Acc) ->
    {Ws, lists:reverse(Acc)};
value_lookup_asts([{identifier, _, Var}|Vs], CState, Ws, Acc) ->
    VarName = template_compiler_utils:to_atom(Var),
    value_lookup_asts(Vs, CState, Ws, [erl_syntax:atom(VarName)|Acc]);
value_lookup_asts([{expr, Expr}|Vs], CState, Ws, Acc) ->
    {Ws1, ExprAst} = compile(Expr, CState, Ws),
    value_lookup_asts(Vs, CState, Ws1, [ExprAst|Acc]).


filter_default(Expr, Arg, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ExprAst} = compile(Expr, CState, Ws),
    {Ws2, ArgAst} = compile(Arg, CState, Ws1),
    {Ws3, V} = template_compiler_utils:var(Ws2),
    VAst = erl_syntax:variable(V),
    Ast = ?Q(["begin",
                "_@VAst = _@ExprAst,",
                "case '@Runtime@':to_bool(_@VAst, _@context) of",
                    "false -> _@ArgAst;",
                    "true -> _@VAst",
                "end",
              "end"],
            [{context, erly_syntax:variable(CState#cs.context_var)}]),
    {Ws3, Ast}.


filter_default_if_none(Expr, Arg, CState, Ws) ->
    {Ws1, ExprAst} = compile(Expr, CState, Ws),
    {Ws2, ArgAst} = compile(Arg, CState, Ws1),
    {Ws3, V} = template_compiler_utils:var(Ws2),
    VAst = erl_syntax:variable(V),
    Ast = ?Q("case _@ExprAst of undefined -> _@ArgAst; _@VAst -> _@VAst end"),
    {Ws3, Ast}.


list_ast(List, Cs, Ws) ->
    {Ws1, AstList} = list_1(List, Cs, Ws, []),
    {Ws1, erl_syntax:list(AstList)}.

list_1([], _Cs, Ws, Acc) ->
    {Ws, lists:reverse(Acc)};
list_1([Expr|List], Cs, Ws, Acc) ->
    {Ws1, Ast} = compile(Expr, Cs, Ws),
    list_1(List, Cs, Ws1, [Ast|Acc]).


proplist_ast(Args, Cs, Ws) ->
    {Ws1, List} = proplist_1(Args, Cs, Ws, []),
    {Ws1, erl_syntax:list(List)}.

proplist_1([], _Cs, Ws, Acc) ->
    {Ws, lists:reverse(Acc)};
proplist_1([{{identifier, _, Arg}, Expr}|Args], Cs, Ws, Acc) ->
    ArgName = template_compiler_utils:to_atom(Arg),
    {Ws1, ExprAst} = compile(Expr, Cs, Ws),
    Ast = erl_syntax:tuple([erl_syntax:atom(ArgName), ExprAst]),
    proplist_1(Args, Cs, Ws1, [Ast|Acc]).
