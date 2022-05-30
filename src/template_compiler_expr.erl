%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2020 Marc Worrell
%% @doc Compile expressions to erl_syntax trees.

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

-module(template_compiler_expr).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    compile/3
    ]).

-include_lib("syntax_tools/include/merl.hrl").
-include("template_compiler_internal.hrl").


-spec compile(element(), #cs{}, #ws{}) -> {#ws{}, erl_syntax:syntaxTree()}.
compile({ast, Ast}, _CState, Ws) ->
    {Ws, Ast};
compile(true, _CState, Ws) ->
    {Ws, erl_syntax:atom(true)};
compile(false, _CState, Ws) ->
    {Ws, erl_syntax:atom(true)};
compile(undefined, _CState, Ws) ->
    {Ws, erl_syntax:atom(undefined)};
compile({string_literal, SrcPos, Text}, _CState, Ws) when is_binary(Text) ->
    {Ws, template_compiler_utils:set_pos(SrcPos, erl_syntax:abstract(Text))};
compile({trans_literal, SrcPos, {trans, _} = Tr}, #cs{runtime=Runtime} = CState, Ws) ->
    Ast = erl_syntax:application(
            erl_syntax:atom(Runtime),
            erl_syntax:atom(lookup_translation),
            [
                erl_syntax:abstract(Tr),
                erl_syntax:variable(CState#cs.vars_var),
                erl_syntax:variable(CState#cs.context_var)
            ]),
    {Ws, template_compiler_utils:set_pos(SrcPos, Ast)};
compile({number_literal, _SrcPos, Nr}, _CState, Ws) ->
    {Ws, erl_syntax:abstract(z_convert:to_integer(Nr))};
compile({atom_literal, _SrcPos, Atom}, _CState, Ws) ->
    {Ws, erl_syntax:abstract(template_compiler_utils:to_atom(Atom))};
compile({find_value, LookupList}, CState, Ws) ->
    find_value_lookup(LookupList, CState, Ws);
compile({auto_id, {{identifier, SrcPos, Name}, {identifier, _, Var}}}, #cs{runtime=Runtime} = CState, Ws) ->
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "iolist_to_binary([ "
                "maps:get('$autoid', _@vars),"
                "$-, _@name,"
                "$-, z_convert:to_binary("
                        "_@runtime:find_value(_@varname, _@vars, _@vars, _@context)"
                    ")"
            "])",
            [
                {runtime, erl_syntax:atom(Runtime)},
                {name, erl_syntax:abstract(Name)},
                {varname, erl_syntax:atom(template_compiler_utils:to_atom(Var))},
                {context, erl_syntax:variable(CState#cs.context_var)},
                {vars, erl_syntax:variable(CState#cs.vars_var)}
            ]),
    {Ws#ws{is_autoid_var=true}, Ast};
compile({auto_id, {identifier, SrcPos, Name}}, #cs{vars_var=Vars}, Ws) ->
    Ast = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "iolist_to_binary([ maps:get('$autoid', _@vars), $-, _@name ])",
                [
                    {name, erl_syntax:abstract(Name)},
                    {vars, erl_syntax:variable(Vars)}
                ]),
    {Ws#ws{is_autoid_var=true}, Ast};
compile({map_value, Args}, Cs, Ws) ->
    {WsProps, PropsAst} = mapfields_ast(Args, Cs, Ws),
    Ast = erl_syntax:map_expr(PropsAst),
    {WsProps, Ast};
compile({tuple_value, {identifier, _, Name}, Args}, Cs, Ws) ->
    TupleName = erl_syntax:atom(template_compiler_utils:to_atom(Name)),
    {WsProps, PropsAst} = proplist_ast(Args, Cs, Ws),
    Ast = ?Q("{ _@TupleName, _@PropsAst }"),
    {WsProps, Ast};
compile({value_list, Exprs}, Cs, Ws) ->
    list_ast(Exprs, Cs, Ws);
compile({expr, {Op, {_, SrcPos, _}}, Arg}, #cs{runtime=Runtime} = CState, Ws) when is_atom(Op) ->
    {Ws1, ArgAst} = compile(Arg, CState, Ws),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "template_compiler_operators:_@op(_@args, _@runtime, _@context)",
            [
                {op, erl_syntax:abstract(Op)},
                {args, ArgAst},
                {context, erl_syntax:variable(CState#cs.context_var)},
                {runtime, erl_syntax:atom(Runtime)}
            ]),
    {Ws1, Ast};
compile({expr, {'or', {_, SrcPos, _}}, Arg1, Arg2}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, Arg1Ast} = compile(Arg1, CState, Ws),
    {Ws2, Arg2Ast} = compile(Arg2, CState, Ws1),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "case _@runtime:to_bool(_@runtime:to_simple_value(_@arg1, _@context), _@context) of "
            "  true -> true; "
            "  false -> _@runtime:to_bool(_@runtime:to_simple_value(_@arg2, _@context), _@context) "
            "end",
            [
                {arg1, Arg1Ast},
                {arg2, Arg2Ast},
                {context, erl_syntax:variable(CState#cs.context_var)},
                {runtime, erl_syntax:atom(Runtime)}
            ]),
    {Ws2, Ast};
compile({expr, {'and', {_, SrcPos, _}}, Arg1, Arg2}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, Arg1Ast} = compile(Arg1, CState, Ws),
    {Ws2, Arg2Ast} = compile(Arg2, CState, Ws1),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "case _@runtime:to_bool(_@runtime:to_simple_value(_@arg1, _@context), _@context) of "
            "  true -> _@runtime:to_bool(_@runtime:to_simple_value(_@arg2, _@context), _@context); "
            "  false -> false "
            "end",
            [
                {arg1, Arg1Ast},
                {arg2, Arg2Ast},
                {context, erl_syntax:variable(CState#cs.context_var)},
                {runtime, erl_syntax:atom(Runtime)}
            ]),
    {Ws2, Ast};
compile({expr, {Op, {_, SrcPos, _}}, Arg1, Arg2}, #cs{runtime=Runtime} = CState, Ws) when is_atom(Op) ->
    {Ws1, Arg1Ast} = compile(Arg1, CState, Ws),
    {Ws2, Arg2Ast} = compile(Arg2, CState, Ws1),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "template_compiler_operators:_@op(_@arg1, _@arg2, _@runtime, _@context)",
            [
                {op, erl_syntax:abstract(Op)},
                {arg1, Arg1Ast},
                {arg2, Arg2Ast},
                {context, erl_syntax:variable(CState#cs.context_var)},
                {runtime, erl_syntax:atom(Runtime)}
            ]),
    {Ws2, Ast};
compile({apply_filter, Expr, {filter, {identifier, SrcPos, <<"default">>}, [Arg]}}, CState, Ws) ->
    filter_default(Expr, Arg, SrcPos, CState, Ws);
compile({apply_filter, Expr, {filter, {identifier, SrcPos, <<"default_if_none">>}, [Arg]}}, CState, Ws) ->
    filter_default_if_none(Expr, Arg, SrcPos, CState, Ws);
compile({apply_filter, Expr, {filter, {identifier, SrcPos, <<"default_if_undefined">>}, [Arg]}}, CState, Ws) ->
    filter_default_if_none(Expr, Arg, SrcPos, CState, Ws);
compile({apply_filter, Expr, {filter, {identifier, SrcPos, Filter}, FilterArgs}}, CState, Ws) ->
    FilterName = template_compiler_utils:to_atom(Filter),
    FilterModule = template_compiler_utils:to_atom(<<"filter_", Filter/binary>>),
    {Ws1, ExprAst} = compile(Expr, CState, Ws),
    {Ws2, AstList} = list_1(FilterArgs, CState, Ws1, []),
    Args = [ExprAst | AstList ] ++ [erl_syntax:variable(CState#cs.context_var)],
    Ast = template_compiler_utils:set_pos(
                SrcPos,
                erl_syntax:application(
                         erl_syntax:atom(FilterModule),
                         erl_syntax:atom(FilterName),
                         Args)),
    {Ws2, Ast};
compile({model, [{identifier, SrcPos, Model} | Path ], OptPayload}, #cs{runtime=Runtime, vars_var=Vars} = CState, Ws) ->
    {Ws1, PathAsts} = value_lookup_asts(Path, CState, Ws, []),
    {Ws2, PayloadAst} = case OptPayload of
        none -> {Ws1, erl_syntax:abstract(undefined)};
        Expr -> compile(Expr, CState, Ws1)
    end,
    {Ws3, V1} = template_compiler_utils:var(Ws2),
    {Ws4, V2} = template_compiler_utils:var(Ws3),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "case _@runtime:model_call(_@model, _@path, _@payload, _@context) of "
            "  {ok, {_@v1, []}} -> _@v1;"
            "  {ok, {_@v1, _@v2}} when is_list(_@v2) -> "
            "       _@runtime:find_nested_value(_@v1, _@v2, _@vars, _@context); "
            "  {error, _} -> undefined "
            "end",
            [
                {model, erl_syntax:atom(binary_to_atom(Model, 'utf8'))},
                {path, erl_syntax:list(PathAsts)},
                {payload, PayloadAst},
                {v1, erl_syntax:variable(V1)},
                {v2, erl_syntax:variable(V2)},
                {vars, erl_syntax:variable(Vars)},
                {context, erl_syntax:variable(CState#cs.context_var)},
                {runtime, erl_syntax:atom(Runtime)}
            ]),
    {Ws4, Ast}.


find_value_lookup([{identifier, SrcPos, <<"now">>}], _CState, Ws) ->
    Ast = template_compiler_utils:set_pos(
            SrcPos,
            erl_syntax:application(
                 erl_syntax:atom(erlang),
                 erl_syntax:atom(universaltime),
                 [])),
    {Ws, Ast};
find_value_lookup([{identifier, SrcPos, <<"true">>}], _CState, Ws) ->
    {Ws, template_compiler_utils:set_pos(SrcPos, erl_syntax:atom(true))};
find_value_lookup([{identifier, SrcPos, <<"false">>}], _CState, Ws) ->
    {Ws, template_compiler_utils:set_pos(SrcPos, erl_syntax:atom(false))};
find_value_lookup([{identifier, SrcPos, <<"undefined">>}], _CState, Ws) ->
    {Ws, template_compiler_utils:set_pos(SrcPos, erl_syntax:atom(undefined))};
find_value_lookup([
            {identifier, SrcPos, <<"forloop">>},
            {identifier, _, Key}
        ], #cs{runtime=Runtime, vars_var=Vars} = CState, Ws) ->
    Ws1 = Ws#ws{is_forloop_var=true},
    ValueLookupAsts = [
        erl_syntax:atom(forloop),
        erl_syntax:atom(binary_to_atom(Key, utf8))
    ],
    Ast = merl:qquote(
            erl_syntax:get_pos(SrcPos),
            "_@runtime:find_nested_value(_@list, _@vars, _@context)",
            [
                {runtime, erl_syntax:atom(Runtime)},
                {list, erl_syntax:list(ValueLookupAsts)},
                {vars, erl_syntax:variable(Vars)},
                {context, erl_syntax:variable(CState#cs.context_var)}
            ]),
    {Ws1, Ast};
find_value_lookup([{_, SrcPos, _}|_] = ValueLookup, #cs{runtime=Runtime, vars_var=Vars} = CState, Ws) ->
    case Runtime:compile_map_nested_value(ValueLookup, CState#cs.context_var, CState#cs.context) of
        [{mfa, M, F, As}] ->
            {Ws1, ValueLookupAsts} = value_lookup_asts(As, CState, Ws, []),
            {Ws2, V1} = template_compiler_utils:var(Ws1),
            {Ws3, V2} = template_compiler_utils:var(Ws2),
            Ast = merl:qquote(
                    template_compiler_utils:pos(SrcPos),
                    "case _@module:_@func(_@mfargs, _@context) of "
                    "  {ok, {_@v1, []}} -> _@v1;"
                    "  {ok, {_@v1, _@v2}} when is_list(_@v2) -> "
                    "       _@runtime:find_nested_value(_@v1, _@v2, _@vars, _@context); "
                    "  {error, _} -> undefined "
                    "end",
                    [
                        {module, erl_syntax:atom(M)},
                        {func, erl_syntax:atom(F)},
                        {mfargs, erl_syntax:list(ValueLookupAsts)},
                        {v1, erl_syntax:variable(V1)},
                        {v2, erl_syntax:variable(V2)},
                        {runtime, erl_syntax:atom(Runtime)},
                        {vars, erl_syntax:variable(Vars)},
                        {context, erl_syntax:variable(CState#cs.context_var)}
                    ]),
            {Ws3, Ast};
        [{mfa2, M, F, As, ExtraArg}] ->
            {Ws1, ValueLookupAsts} = value_lookup_asts(As, CState, Ws, []),
            {Ws2, V1} = template_compiler_utils:var(Ws1),
            {Ws3, V2} = template_compiler_utils:var(Ws2),
            Ast = merl:qquote(
                    template_compiler_utils:pos(SrcPos),
                    "case _@module:_@func(_@mfargs, _@extra, _@context) of "
                    "  {ok, {_@v1, []}} -> _@v1;"
                    "  {ok, {_@v1, _@v2}} when is_list(_@v2) -> "
                    "       _@runtime:find_nested_value(_@v1, _@v2, _@vars, _@context); "
                    "  {error, _} -> undefined "
                    "end",
                    [
                        {module, erl_syntax:atom(M)},
                        {func, erl_syntax:atom(F)},
                        {mfargs, erl_syntax:list(ValueLookupAsts)},
                        {extra, erl_syntax:abstract(ExtraArg)},
                        {v1, erl_syntax:variable(V1)},
                        {v2, erl_syntax:variable(V2)},
                        {runtime, erl_syntax:atom(Runtime)},
                        {vars, erl_syntax:variable(Vars)},
                        {context, erl_syntax:variable(CState#cs.context_var)}
                    ]),
            {Ws3, Ast};
        [{ast, Ast}] ->
            {Ws, Ast};
        [{ast, Ast} | ValueLookup1 ] ->
            {Ws1, ValueLookupAsts} = value_lookup_asts(ValueLookup1, CState, Ws, []),
            Ast1 = merl:qquote(
                    erl_syntax:get_pos(hd(ValueLookupAsts)),
                    "_@runtime:find_nested_value(_@list, _@vars, _@context)",
                    [
                        {runtime, erl_syntax:atom(Runtime)},
                        {list, erl_syntax:list(ValueLookupAsts)},
                        {vars, Ast},
                        {context, erl_syntax:variable(CState#cs.context_var)}
                    ]),
            {Ws1, Ast1};
        [{identifier, SrcPosIdn, Var}] ->
            VarName = template_compiler_utils:to_atom(Var),
            Ast = merl:qquote(
                    template_compiler_utils:pos(SrcPosIdn),
                    "_@runtime:find_value(_@varname, _@vars, _@vars, _@context)",
                    [
                        {runtime, erl_syntax:atom(Runtime)},
                        {varname, erl_syntax:atom(VarName)},
                        {vars, erl_syntax:variable(Vars)},
                        {context, erl_syntax:variable(CState#cs.context_var)}
                    ]),
            {Ws, Ast};
        ValueLookup1 ->
            {Ws1, ValueLookupAsts} = value_lookup_asts(ValueLookup1, CState, Ws, []),
            Ast = merl:qquote(
                    erl_syntax:get_pos(hd(ValueLookupAsts)),
                    "_@runtime:find_nested_value(_@list, _@vars, _@context)",
                    [
                        {runtime, erl_syntax:atom(Runtime)},
                        {list, erl_syntax:list(ValueLookupAsts)},
                        {vars, erl_syntax:variable(Vars)},
                        {context, erl_syntax:variable(CState#cs.context_var)}
                    ]),
            {Ws1, Ast}
    end.

value_lookup_asts([], _CState, Ws, Acc) ->
    {Ws, lists:reverse(Acc)};
value_lookup_asts([{identifier, _, Var}|Vs], CState, Ws, Acc) ->
    value_lookup_asts(Vs, CState, Ws, [erl_syntax:abstract(Var)|Acc]);
value_lookup_asts([{ast, Ast}|Vs], CState, Ws, Acc) ->
    value_lookup_asts(Vs, CState, Ws, [Ast|Acc]);
value_lookup_asts([{expr, Expr}|Vs], CState, Ws, Acc) ->
    {Ws1, ExprAst} = compile(Expr, CState, Ws),
    value_lookup_asts(Vs, CState, Ws1, [ExprAst|Acc]).


filter_default(Expr, Arg, SrcPos, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ExprAst} = compile(Expr, CState, Ws),
    {Ws2, ArgAst} = compile(Arg, CState, Ws1),
    {Ws3, V} = template_compiler_utils:var(Ws2),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "begin "
                "_@v = _@expr,"
                "case _@runtime:to_bool(_@v, _@context) of "
                    "false -> _@arg;"
                    "true -> _@v "
                "end "
            "end",
            [
                {v, erl_syntax:variable(V)},
                {expr, ExprAst},
                {arg, ArgAst},
                {runtime, erl_syntax:atom(Runtime)},
                {context, erl_syntax:variable(CState#cs.context_var)}
            ]),
    {Ws3, Ast}.


filter_default_if_none(Expr, Arg, SrcPos, CState, Ws) ->
    {Ws1, ExprAst} = compile(Expr, CState, Ws),
    {Ws2, ArgAst} = compile(Arg, CState, Ws1),
    {Ws3, V} = template_compiler_utils:var(Ws2),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "case _@expr of undefined -> _@arg; _@v -> _@v end",
            [
                {v, erl_syntax:variable(V)},
                {expr, ExprAst},
                {arg, ArgAst}
            ]),
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


mapfields_ast(Args, Cs, Ws) ->
    mapfields_1(Args, Cs, Ws, []).

mapfields_1([], _Cs, Ws, Acc) ->
    {Ws, lists:reverse(Acc)};
mapfields_1([{{identifier, _, Arg}, Expr}|Args], Cs, Ws, Acc) ->
    {Ws1, ExprAst} = compile(Expr, Cs, Ws),
    Ast = erl_syntax:map_field_assoc(erl_syntax:abstract(Arg), ExprAst),
    mapfields_1(Args, Cs, Ws1, [Ast|Acc]);
mapfields_1([{{string_literal, _, Text}, Expr}|Args], Cs, Ws, Acc) ->
    {Ws1, ExprAst} = compile(Expr, Cs, Ws),
    Ast = erl_syntax:map_field_assoc(erl_syntax:abstract(Text), ExprAst),
    mapfields_1(Args, Cs, Ws1, [Ast|Acc]).
