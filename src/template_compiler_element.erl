%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Compile main block elements to erl_syntax trees.

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

-module(template_compiler_element).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    compile/3
    ]).

-include_lib("syntax_tools/include/merl.hrl").
-include("template_compiler.hrl").

compile([], _CState, Ws) ->
    {Ws, erl_syntax:abstract(<<>>)};
compile(L, CState, Ws) when is_list(L) ->
    {Ws1, Asts} = lists:foldr(
                    fun(Elt, {WsAcc, AstAcc}) ->
                        {WsAcc1, Ast} = compile(Elt, CState, WsAcc),
                        {WsAcc1, [Ast|AstAcc]}
                    end,
                    {Ws,[]},
                    L),
    {Ws1, erl_syntax:list(Asts)};
compile({text, _Pos, Text}, _CState, Ws) ->
    {Ws, erl_syntax:abstract(Text)};
compile({trans_text, _Pos, Tr}, #cs{runtime=Runtime} = CState, Ws) ->
    Ast = erl_syntax:application(
            erl_syntax:atom(Runtime),
            erl_syntax:atom(lookup_translation),
            [
                erl_syntax:abstract(Tr),
                erl_syntax:variable(CState#cs.vars_var),
                erl_syntax:variable(CState#cs.context_var)
            ]),
    {Ws, Ast};
compile({trans_ext, {string_literal, Pos, Text}, Args}, CState, Ws) ->
    Trans = lists:map(
            fun({{identifier,_,Lang}, {string_literal,_,String}}) ->
                {template_compiler_utils:to_atom(Lang), String}
            end,
            Args),
    Trans1 = {trans, lists:sort([{en,Text} | Trans])},
    compile({trans_text, Pos, Trans1}, CState, Ws);
compile({value, Expr, _With}, #cs{runtime=Runtime} = CState, Ws) ->
    % TODO: handle optional With
    {Ws1, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws),
    Ast = ?Q("'@Runtime@':to_iolist(_@ExprAst, _@vars, _@context)",
            [
                {context, erl_syntax:variable(CState#cs.context_var)},
                {vars, erl_syntax:variable(CState#cs.vars_var)}
            ]),
    {Ws1, Ast};
compile({date, now, {string_literal, _Pos, Format}}, CState, Ws) ->
    FormatAst = erl_syntax:abstract(Format),
    Ast = ?Q("filter_date:date(erlang:universaltime(), _@FormatAst, _@context)",
            [{context, erl_syntax:variable(CState#cs.context_var)}]),
    {Ws, Ast};
compile({load, Names}, _CState, Ws) ->
    CustomTags = [ Name || {identifier, _, Name} <- Names ],
    {Ws#ws{custom_tags=CustomTags ++ Ws#ws.custom_tags}, <<>>};
compile({block, {identifier, _Pos, Name}, _Elts}, CState, Ws) ->
    Ast = erl_syntax:application(
            erl_syntax:atom(template_compiler_runtime_internal),
            erl_syntax:atom(block_call),
            [
                erl_syntax:atom(template_compiler_utils:to_atom(Name)),
                erl_syntax:variable(CState#cs.vars_var),
                erl_syntax:variable("Blocks"),
                erl_syntax:variable(CState#cs.context_var)
            ]),
    {Ws, Ast};
compile(inherit, #cs{block=undefined}, Ws) ->
    {Ws, erl_syntax:abstract(<<>>)};
compile(inherit, #cs{block=Block, module=Module} = CState, Ws) ->
    Ast = erl_syntax:application(
            erl_syntax:atom(template_compiler_runtime_internal),
            erl_syntax:atom(block_inherit),
            [
                erl_syntax:atom(Module),
                erl_syntax:atom(Block),
                erl_syntax:variable(CState#cs.vars_var),
                erl_syntax:variable("Blocks"),
                erl_syntax:variable(CState#cs.context_var)
            ]),
    {Ws, Ast};
compile({'if', {'as', Expr, undefined}, IfElts, ElseElts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws),
    {Ws2, IfClauseAst} = compile(IfElts, CState, Ws1),
    {Ws3, ElseClauseAst} = compile(ElseElts, CState, Ws2),
    Ast = ?Q([
        "case '@Runtime@':to_bool(_@ExprAst, _@context) of ",
         "true -> _@IfClauseAst;",
         "false -> _@ElseClauseAst",
        "end"],
        [
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]
    ),
    {Ws3, Ast};
compile({'if', {'as', Expr, {identifier, _Pos, Name}}, IfElts, ElseElts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, V} = template_compiler_utils:var(Ws),
    {CState1, Ws2} = template_compiler_utils:next_vars_var(CState, Ws1),
    {Ws3, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws2),
    {Ws4, IfClauseAst} = compile(IfElts, CState1, Ws3),
    {Ws5, ElseClauseAst} = compile(ElseElts, CState, Ws4),
    VAst = erl_syntax:variable(V),
    Ast = ?Q([
        "begin",
          "_@VAst = _@ExprAst,",
          "case '@Runtime@':to_bool(_@VAst, _@context) of ",
            "true -> _@vars1 = _@vars#{ _@name => _@VAst }, _@IfClauseAst;",
            "false -> _@ElseClauseAst",
          "end",
        "end"],
       [
            {name, template_compiler_utils:to_atom(Name)},
            {context, erl_syntax:variable(CState#cs.context_var)},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {vars1, erl_syntax:variable(CState1#cs.vars_var)}
       ]),
    {Ws5, Ast};
compile({'for', {'in', Idents, ListExpr}, LoopElts, EmptyElts}, #cs{runtime=Runtime} = CState, Ws) ->
    {CsLoop, WsLoop} = template_compiler_utils:next_vars_var(CState, Ws#ws{is_forloop_var=false, is_include_inherit=false}),
    {WsLoop1, LoopAst} = compile(LoopElts, CsLoop, WsLoop),
    WsEmpty = WsLoop1#ws{ 
        is_forloop_var=Ws#ws.is_forloop_var,
        is_include_inherit=Ws#ws.is_include_inherit
    },
    {WsEmpty1, EmptyAst} = compile(EmptyElts, CState, WsEmpty),
    {WsExpr, ExprAst} = template_compiler_expr:compile(ListExpr, CState, WsEmpty1),
    LoopVarsAst = erl_syntax:abstract(idents_as_atoms(Idents)),
    Ast = ?Q([
            "template_compiler_runtime_internal:forloop(",
                "_@isforloopvar,"
                "_@ExprAst,"
                "_@LoopVarsAst,",
                "fun(_@varsloop) -> _@LoopAst end,"
                "fun() -> _@EmptyAst end,",
                "_@Runtime@,",
                "_@vars,",
                "_@context"
            ")"
        ],
        [
            {isforloopvar, erl_syntax:atom(WsLoop1#ws.is_forloop_var or WsLoop1#ws.is_include_inherit)},
            {varsloop, erl_syntax:variable(CsLoop#cs.vars_var)},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {WsExpr, Ast}.



idents_as_atoms(Idents) ->
    [ ident_as_atom(Ident) || Ident <- Idents ].

ident_as_atom({identifier, _SrcPos, Ident}) ->
    template_compiler_utils:to_atom(Ident).
