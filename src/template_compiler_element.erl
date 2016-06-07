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
-include("template_compiler_internal.hrl").

-spec compile(element()|elements(), #cs{}, #ws{}) -> {#ws{}, erl_syntax:syntaxTree()}.
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
compile({value, Expr, []}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws),
    Ast = ?Q("'@Runtime@':to_render_result(_@ExprAst, _@vars, _@context)",
            [
                {context, erl_syntax:variable(CState#cs.context_var)},
                {vars, erl_syntax:variable(CState#cs.vars_var)}
            ]),
    case CState#cs.is_autoescape of
        true ->
            Ast2 = ?Q("'@Runtime@':escape(_@Ast, _@context)",
                      [
                        {context, erl_syntax:variable(CState#cs.context_var)}
                      ]),
            {Ws1, Ast2};
        false ->
            {Ws1, Ast}
    end;
compile({value, Expr, With}, CState, Ws) ->
    {Ws1, WithExprAsts} = with_args(With, CState, Ws, false),
    {CState1, Ws2} = template_compiler_utils:next_vars_var(CState, Ws1),
    MapAst = erl_syntax:map_expr(
                erl_syntax:variable(CState#cs.vars_var),
                [
                    erl_syntax:map_field_assoc(WName, WExpr)
                    || {WName, WExpr} <- WithExprAsts
                ]),
    {Ws3, WithAst} = compile({value, Expr, []}, CState1, Ws2),
    Ast = ?Q([
        "begin",
            "_@vars = _@MapAst,",
            "_@WithAst",
        "end"],
        [
            {vars, erl_syntax:variable(CState1#cs.vars_var)}
        ]),
    {Ws3, Ast};
compile({date, now, {string_literal, _Pos, Format}}, CState, Ws) ->
    FormatAst = erl_syntax:abstract(Format),
    Ast = ?Q("filter_date:date(erlang:universaltime(), _@FormatAst, _@context)",
            [{context, erl_syntax:variable(CState#cs.context_var)}]),
    {Ws, Ast};
compile({load, Names}, _CState, Ws) ->
    % We don't do anything with this. Present for compatibility only.
    CustomTags = [ Name || {identifier, _, Name} <- Names ],
    {Ws#ws{custom_tags=CustomTags ++ Ws#ws.custom_tags}, <<>>};
compile({block, {identifier, SrcPos, Name}, _Elts}, CState, Ws) ->
    BlockName = template_compiler_utils:to_atom(Name),
    Ast = erl_syntax:application(
            erl_syntax:atom(template_compiler_runtime_internal),
            erl_syntax:atom(block_call),
            [
                erl_syntax:abstract(SrcPos),
                erl_syntax:atom(BlockName),
                erl_syntax:variable(CState#cs.vars_var),
                erl_syntax:variable("Blocks"),
                erl_syntax:atom(CState#cs.runtime),
                erl_syntax:variable(CState#cs.context_var)
            ]),
    {value, {BlockName, _Tree, BlockWs}} = lists:keysearch(BlockName, 1, CState#cs.blocks), 
    Ws1 = Ws#ws{is_forloop_var = Ws#ws.is_forloop_var or BlockWs#ws.is_forloop_var}, 
    {Ws1, Ast};
compile({inherit, {_, _SrcPos, _}}, #cs{block=undefined}, Ws) ->
    {Ws, erl_syntax:abstract(<<>>)};
compile({inherit, {_, SrcPos, _}}, #cs{block=Block, module=Module} = CState, Ws) ->
    Ast = erl_syntax:application(
            erl_syntax:atom(template_compiler_runtime_internal),
            erl_syntax:atom(block_inherit),
            [
                erl_syntax:abstract(SrcPos),
                erl_syntax:atom(Module),
                erl_syntax:atom(Block),
                erl_syntax:variable(CState#cs.vars_var),
                erl_syntax:variable("Blocks"),
                erl_syntax:atom(CState#cs.runtime),
                erl_syntax:variable(CState#cs.context_var)
            ]),
    {Ws#ws{is_forloop_var=true}, Ast};
compile({'include', TagPos, Method, Template, Args}, CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    include(TagPos, Method, Template, ArgsList, CState, Ws1);
compile({'catinclude', TagPos, Method, Template, IdExpr, Args}, CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    {Ws2, IdAst} = template_compiler_expr:compile(IdExpr, CState, Ws1),
    catinclude(TagPos, Method, Template, IdAst, ArgsList, CState, Ws2);
compile({'call', {identifier, _, Name}, Args}, CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    Module = template_compiler_utils:to_atom(Name),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    Ast = ?Q([
            "template_compiler_runtime_internal:call(",
                    "_@Module@,",
                    "_@ArgsListAst,",
                    "_@vars,",
                    "_@context)"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast};
compile({'call_with', {identifier, _, Name}, Expr}, CState, Ws) ->
    {Ws1, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws),
    Module = template_compiler_utils:to_atom(Name),
    Ast = ?Q([
            "template_compiler_runtime_internal:call(",
                    "_@Module@,",
                    "[{with, _@ExprAst}],",
                    "_@vars,",
                    "_@context)"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast};
compile({custom_tag, {identifier, _, Name}, Args}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, true),
    TagName = template_compiler_utils:to_atom(Name),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    Ast = ?Q([
            "'@Runtime@':custom_tag(",
                    "_@TagName@,",
                    "_@ArgsListAst,",
                    "_@vars,",
                    "_@context)"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast};
compile({Tag, Expr, Args}, #cs{runtime=Runtime} = CState, Ws) when Tag =:= image; Tag =:= image_url; Tag =:= media ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    {Ws2, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws1),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    Ast = ?Q([
            "'@Runtime@':builtin_tag(",
                    "_@Tag@,",
                    "_@ExprAst,",
                    "_@ArgsListAst,",
                    "_@vars,",
                    "_@context)"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws2, Ast};
compile({url, Expr, Args}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    {Ws2, DispatchRuleAst} = case Expr of
        {find_value, [{identifier, _, Name}]} ->
            {Ws1, erl_syntax:atom(template_compiler_utils:to_atom(Name))};
        _ ->
            template_compiler_expr:compile(Expr, CState, Ws1)
    end,
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    Ast = ?Q([
            "'@Runtime@':builtin_tag(",
                    "url,",
                    "_@DispatchRuleAst,",
                    "_@ArgsListAst,",
                    "_@vars,",
                    "_@context)"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws2, Ast};
compile({lib, LibList, Args}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    LibFilenames = lists:map(fun({string_literal, _, Filename}) -> Filename end, LibList),
    Ast = ?Q([
            "'@Runtime@':builtin_tag(",
                    "lib,",
                    "_@LibFilenames@,",
                    "_@ArgsListAst,",
                    "_@vars,",
                    "_@context)"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast};
compile({print, Expr}, CState, Ws) ->
    {Ws1, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws),
    Ast = ?Q("template_compiler_runtime_internal:print(_@ExprAst)"),
    {Ws1, Ast};
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
            {name, erl_syntax:atom(template_compiler_utils:to_atom(Name))},
            {context, erl_syntax:variable(CState#cs.context_var)},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {vars1, erl_syntax:variable(CState1#cs.vars_var)}
       ]),
    {Ws5, Ast};
compile({'for', {'in', Idents, ListExpr}, LoopElts, EmptyElts}, #cs{runtime=Runtime} = CState, Ws) ->
    {CsLoop, WsLoop} = template_compiler_utils:next_vars_var(CState, Ws#ws{is_forloop_var=false}),
    {WsLoop1, LoopAst} = compile(LoopElts, CsLoop, WsLoop),
    WsEmpty = WsLoop1#ws{ 
        is_forloop_var=Ws#ws.is_forloop_var
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
            {isforloopvar, erl_syntax:atom(WsLoop1#ws.is_forloop_var)},
            {varsloop, erl_syntax:variable(CsLoop#cs.vars_var)},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {WsExpr, Ast};
compile({'with', {Exprs, Idents}, Elts}, CState, Ws) ->
    {Ws1, ExprAsts} = expr_list(Exprs, CState, Ws),
    VarsAsts = erl_syntax:abstract(idents_as_atoms(Idents)),
    ExprListAst = erl_syntax:list(ExprAsts),
    {CsWith, Ws2} = template_compiler_utils:next_vars_var(CState, Ws1),
    {Ws3, BodyAst} = compile(Elts, CsWith, Ws2),
    Ast = ?Q([
            "begin",
                "_@vars1 = template_compiler_runtime_internal:with_vars(",
                    "_@VarsAsts,",
                    "_@ExprListAst,",
                    "_@vars),",
                "_@BodyAst",
            "end"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {vars1, erl_syntax:variable(CsWith#cs.vars_var)}
        ]),
    {Ws3, Ast};
compile({cache, {CacheTime, Args}, Elts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    {Ws2, CacheTimeAst} = template_compiler_expr:compile(CacheTime, CState, Ws1),
    {CsBody, Ws3} = template_compiler_utils:next_vars_var(CState, Ws2),
    {CsBody1, Ws4} = template_compiler_utils:next_vars_var(CsBody, Ws3),
    {Ws5, BodyAst} = compile(Elts, CsBody1, Ws4),
    Unique = template_compiler_runtime_internal:unique(),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    Ast = ?Q([
            "'@Runtime@':cache_tag(",
                    "_@CacheTimeAst,",
                    "_@Unique@,"
                    "_@ArgsListAst,",
                    "fun(_@varsbody, _@contextbody) -> _@BodyAst end,",
                    "_@vars,",
                    "_@context)"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)},
            {varsbody, erl_syntax:variable(CsBody1#cs.vars_var)},
            {contextbody, erl_syntax:variable(CsBody1#cs.context_var)}
        ]),
    {Ws5, Ast};
compile({javascript, Elts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, BodyAst} = compile(Elts, CState, Ws),
    Ast = ?Q([
            "'@Runtime@':javascript_tag(",
                    "_@BodyAst,",
                    "_@vars,",
                    "_@context)"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast};
compile({filter, Filters, Elts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, BodyAst} = compile(Elts, CState, Ws),
    Expr = lists:foldl(
                fun({filter, Name, Args}, Acc) ->
                    {apply_filter, Acc, {filter, Name, Args}}
                end,
                {ast, BodyAst},
                Filters),
    {Ws2, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws1),
    Ast = ?Q("'@Runtime@':to_render_result(_@ExprAst, _@vars, _@context)",
            [
                {context, erl_syntax:variable(CState#cs.context_var)},
                {vars, erl_syntax:variable(CState#cs.vars_var)}
            ]),
    {Ws2, Ast};
compile({spaceless, Elts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, BodyAst} = compile(Elts, CState, Ws),
    Ast = ?Q("'@Runtime@':spaceless_tag(_@BodyAst, _@vars, _@context)",
            [
                {context, erl_syntax:variable(CState#cs.context_var)},
                {vars, erl_syntax:variable(CState#cs.vars_var)}
            ]),
    {Ws1, Ast};
compile({autoescape, {identifier, _, <<"on">>}, Elts}, CState, Ws) ->
    compile(Elts, CState#cs{is_autoescape = true}, Ws);
compile({autoescape, {identifier, _, <<"off">>}, Elts}, CState, Ws) ->
    compile(Elts, CState#cs{is_autoescape = false}, Ws);
compile({autoescape, {identifier, _, OnOff}, Elts}, CState, Ws) ->
    compile(Elts, CState#cs{is_autoescape = z_convert:to_bool(OnOff)}, Ws);
compile({cycle_compat, Names}, CState, Ws) ->
    Exprs = [{string_literal, Pos, Name} || {identifier, Pos, Name} <- Names ],
    compile({cycle, Exprs}, CState, Ws);
compile({cycle, []}, _CState, Ws) ->
    {Ws, erl_syntax:list([])};
compile({cycle, Exprs}, CState, Ws) ->
    {Ws1, Var} = template_compiler_utils:var(Ws),
    {Ws2, ExprList} = expr_list(Exprs, CState, Ws1),
    N = length(Exprs),
    Clauses = lists:zip(lists:seq(0,N-1), ExprList),
    ClauseAsts = [ erl_syntax:clause([erl_syntax:integer(Nr)], none, [Expr]) || {Nr,Expr} <- Clauses ],
    ValueAst = ?Q("maps:get(counter0, _@v) rem _@N@", 
                  [ {v, erl_syntax:variable(Var)} ]),
    CaseAst = erl_syntax:case_expr(ValueAst, ClauseAsts),
    Ast = ?Q([
                "case maps:get(forloop, _@vars, undefined) of",
                    "undefined -> _@first;",
                    "_@v -> _@CaseAst",
                "end"
            ],
            [
                {first, hd(ExprList)},
                {v, erl_syntax:variable(Var)},
                {vars, erl_syntax:variable(CState#cs.vars_var)}
            ]),
    compile({value, {ast, Ast}, []}, CState, Ws2#ws{is_forloop_var=true}).


include({_, SrcPos, _}, Method, Template, ArgsList, #cs{runtime=Runtime} = CState, Ws) when is_atom(Method) ->
    {Ws1, TemplateAst} = template_compiler_expr:compile(Template, CState, Ws),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    Ast = ?Q([
            "template_compiler_runtime_internal:include(",
                    "_@SrcPos@,",
                    "_@Method@,",
                    "_@TemplateAst,",
                    "_@ArgsListAst,",
                    "_@Runtime@,",
                    "_@vars,",
                    "_@context)"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast}.


catinclude({_, SrcPos, _}, Method, Template, IdAst, ArgsList, #cs{runtime=Runtime} = CState, Ws) when is_atom(Method) ->
    {Ws1, TemplateAst} = template_compiler_expr:compile(Template, CState, Ws),
    ArgsList1 = [ {erl_syntax:atom(id),IdAst} | ArgsList ],
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList1 ]),
    Ast = ?Q([
            "template_compiler_runtime_internal:include(",
                    "_@SrcPos@,",
                    "_@Method@,",
                    "{cat, _@TemplateAst},",
                    "_@ArgsListAst,",
                    "_@Runtime@,",
                    "_@vars,",
                    "_@context)"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast}.



expr_list(ExprList, CState, Ws) ->
    lists:foldr(
        fun(E, {WsAcc, ExprAcc}) ->
            {WsAcc1, EAst} = template_compiler_expr:compile(E, CState, WsAcc),
            {WsAcc1, [EAst|ExprAcc]}
        end,
        {Ws, []},
        ExprList).


with_args(With, CState, Ws, IsPostback) ->
    lists:foldl(
            fun
                ({Ident, true}, {WsAcc, Acc}) ->
                    VarAst = erl_syntax:atom(ident_as_atom(Ident)),
                    {WsAcc, [{VarAst, erl_syntax:atom(true)}|Acc]};
                ({{identifier, _, <<"postback">>}, {string_literal, _, Postback}}, {WsAcc, Acc}) when IsPostback ->
                    ExprAst = erl_syntax:atom(template_compiler_utils:to_atom(Postback)),
                    VarAst = erl_syntax:atom(postback),
                    {WsAcc, [{VarAst, ExprAst}|Acc]};
                ({Ident, Expr}, {WsAcc, Acc}) ->
                    {Ws1, ExprAst} = template_compiler_expr:compile(Expr, CState, WsAcc),
                    VarAst = erl_syntax:atom(ident_as_atom(Ident)),
                    {Ws1, [{VarAst, ExprAst}|Acc]}
            end,
            {Ws, []},
            With).

-spec idents_as_atoms([identifier_token()]) -> [ atom() ].
idents_as_atoms(Idents) ->
    [ ident_as_atom(Ident) || Ident <- Idents ].

ident_as_atom({identifier, _SrcPos, Ident}) ->
    template_compiler_utils:to_atom(Ident).
