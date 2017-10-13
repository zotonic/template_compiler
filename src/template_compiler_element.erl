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
compile({text, SrcPos, Text}, _CState, Ws) ->
    {Ws, template_compiler_utils:set_pos(SrcPos, erl_syntax:abstract(Text))};
compile({trans_text, SrcPos, Tr}, #cs{runtime=Runtime} = CState, Ws) ->
    Ast = erl_syntax:application(
            erl_syntax:atom(Runtime),
            erl_syntax:atom(lookup_translation),
            [
                erl_syntax:abstract(Tr),
                erl_syntax:variable(CState#cs.vars_var),
                erl_syntax:variable(CState#cs.context_var)
            ]),
    {Ws, template_compiler_utils:set_pos(SrcPos, Ast)};
compile({trans_ext, Tr, Args}, CState, Ws) ->
    trans_ext(Tr, Args, CState, Ws);
compile({value, {_, SrcPos, _}, Expr, []}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "_@runtime:to_render_result(_@expr, _@vars, _@context)",
            [
                {expr, ExprAst},
                {runtime, erl_syntax:atom(Runtime)},
                {context, erl_syntax:variable(CState#cs.context_var)},
                {vars, erl_syntax:variable(CState#cs.vars_var)}
            ]),
    case CState#cs.is_autoescape of
        true ->
            Ast2 = merl:qquote(
                    template_compiler_utils:pos(SrcPos),
                    "_@runtime:escape(_@ast, _@context)",
                    [
                        {ast, Ast},
                        {runtime, erl_syntax:atom(Runtime)},
                        {context, erl_syntax:variable(CState#cs.context_var)}
                    ]),
            {Ws1, Ast2};
        false ->
            {Ws1, Ast}
    end;
compile({value, {_, SrcPos, _} = TagSrc, Expr, With}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, WithExprAsts} = with_args(With, CState, Ws, false),
    {CState1, Ws2} = template_compiler_utils:next_vars_var(CState, Ws1),
    MapAst = template_compiler_utils:set_pos(
                SrcPos,
                erl_syntax:map_expr(
                    erl_syntax:variable(CState#cs.vars_var),
                    [
                        erl_syntax:map_field_assoc(WName, WExpr)
                        || {WName, WExpr} <- WithExprAsts
                    ])),
    case is_context_vars_arg(With, CState) of
        true ->
            {CState2, Ws3} = template_compiler_utils:next_context_var(CState1, Ws2),
            {Ws4, WithAst} = compile({value, TagSrc, Expr, []}, CState2, Ws3),
            Ast = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "begin "
                    "_@vars = _@map,"
                    "_@context1 = _@runtime:set_context_vars(_@vars, _@context),"
                    "_@with "
                "end",
                [
                    {vars, erl_syntax:variable(CState1#cs.vars_var)},
                    {map, MapAst},
                    {with, WithAst},
                    {runtime, erl_syntax:atom(Runtime)},
                    {context, erl_syntax:variable(CState#cs.context_var)},
                    {context1, erl_syntax:variable(CState2#cs.context_var)}
                ]),
            {Ws4, Ast};
        false ->
            {Ws3, WithAst} = compile({value, TagSrc, Expr, []}, CState1, Ws2),
            Ast = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "begin "
                    "_@vars = _@map,"
                    "_@with "
                "end",
                [
                    {vars, erl_syntax:variable(CState1#cs.vars_var)},
                    {map, MapAst},
                    {with, WithAst}
                ]),
            {Ws3, Ast}
    end;
compile({date, now, {_, SrcPos, _}, {string_literal, _SrcPos, Format}}, CState, Ws) ->
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "filter_date:date(erlang:universaltime(), _@format, _@context)",
            [
                {format, erl_syntax:abstract(Format)},
                {context, erl_syntax:variable(CState#cs.context_var)}
            ]),
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
    {Ws1, template_compiler_utils:set_pos(SrcPos, Ast)};
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
    {Ws#ws{is_forloop_var=true}, template_compiler_utils:set_pos(SrcPos, Ast)};
compile({'include', TagPos, Method, Template, Args}, CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    IsContextVar = is_context_vars_arg(Args, CState),
    include(TagPos, Method, Template, ArgsList, IsContextVar, CState, Ws1);
compile({'catinclude', TagPos, Method, Template, IdExpr, Args}, CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    {Ws2, IdAst} = template_compiler_expr:compile(IdExpr, CState, Ws1),
    IsContextVar = is_context_vars_arg(Args, CState),
    catinclude(TagPos, Method, Template, IdAst, ArgsList, IsContextVar, CState, Ws2);
compile({'call', {identifier, SrcPos, Name}, Args}, CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    Module = template_compiler_utils:to_atom(Name),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    Ast = merl:qquote(
        template_compiler_utils:pos(SrcPos),
        "template_compiler_runtime_internal:call("
                "_@module,"
                "_@args,"
                "_@vars,"
                "_@context)",
        [
            {module, erl_syntax:atom(Module)},
            {args, ArgsListAst},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast};
compile({'call_with', {identifier, SrcPos, Name}, Expr}, CState, Ws) ->
    {Ws1, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws),
    Module = template_compiler_utils:to_atom(Name),
    Ast = merl:qquote(
        template_compiler_utils:pos(SrcPos),
        "template_compiler_runtime_internal:call("
                "_@module,"
                "[{with, _@expr}],"
                "_@vars,"
                "_@context)",
        [
            {module, erl_syntax:atom(Module)},
            {expr, ExprAst},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast};
compile({custom_tag, {identifier, SrcPos, Name}, Args}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, true),
    TagName = template_compiler_utils:to_atom(Name),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    Ast = merl:qquote(
        template_compiler_utils:pos(SrcPos),
        "_@runtime:custom_tag("
                "_@tagname,"
                "_@args,"
                "_@vars,"
                "_@context)",
        [
            {runtime, erl_syntax:atom(Runtime)},
            {tagname, erl_syntax:atom(TagName)},
            {args, ArgsListAst},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast};
compile({Tag, {_, SrcPos, _}, Expr, Args}, #cs{runtime=Runtime} = CState, Ws) when Tag =:= image; Tag =:= image_url; Tag =:= media ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    {Ws2, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws1),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    case is_context_vars_arg(Args, CState) of
        true ->
            {Ws3, ArgsVar} = template_compiler_utils:var(Ws2),
            {CsCtx, Ws4} = template_compiler_utils:next_context_var(CState, Ws3),
            Ast = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "begin "
                    "_@argsvar = _@argslist,"
                    "_@context1 = _@runtime:set_context_vars(_@args, _@context),"
                    "_@runtime:builtin_tag("
                            "_@tag,"
                            "_@expr,"
                            "_@argsvar,"
                            "_@vars,"
                            "_@context1) "
                "end",
                [
                    {argslist, ArgsListAst},
                    {argsvar, erl_syntax:variable(ArgsVar)},
                    {runtime, erl_syntax:atom(Runtime)},
                    {tag, erl_syntax:abstract(Tag)},
                    {expr, ExprAst},
                    {vars, erl_syntax:variable(CState#cs.vars_var)},
                    {context, erl_syntax:variable(CState#cs.context_var)},
                    {context1, erl_syntax:variable(CsCtx#cs.context_var)}
                ]),
            {Ws4, Ast};
        false ->
            Ast = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "_@runtime:builtin_tag("
                        "_@tag,"
                        "_@expr,"
                        "_@argslist,"
                        "_@vars,"
                        "_@context)",
                [
                    {runtime, erl_syntax:atom(Runtime)},
                    {tag, erl_syntax:abstract(Tag)},
                    {expr, ExprAst},
                    {argslist, ArgsListAst},
                    {vars, erl_syntax:variable(CState#cs.vars_var)},
                    {context, erl_syntax:variable(CState#cs.context_var)}
                ]),
            {Ws2, Ast}
    end;
compile({url, {_, SrcPos, _}, Expr, Args}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    {Ws2, DispatchRuleAst} = case Expr of
        {find_value, [{identifier, _, Name}]} ->
            {Ws1, erl_syntax:atom(template_compiler_utils:to_atom(Name))};
        _ ->
            template_compiler_expr:compile(Expr, CState, Ws1)
    end,
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    case is_context_vars_arg(Args, CState) of
        true ->
            {Ws3, ArgsVar} = template_compiler_utils:var(Ws2),
            {CsCtx, Ws4} = template_compiler_utils:next_context_var(CState, Ws3),
            Ast = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "begin "
                    "_@args = _@argslist,"
                    "_@context1 = _@runtime:set_context_vars(_@args, _@context),"
                    "_@runtime:builtin_tag("
                            "url,"
                            "_@dispatchrule,"
                            "_@args,"
                            "_@vars,"
                            "_@context1) "
                "end",
                [
                    {runtime, erl_syntax:atom(Runtime)},
                    {args, erl_syntax:variable(ArgsVar)},
                    {argslist, ArgsListAst},
                    {dispatchrule, DispatchRuleAst},
                    {vars, erl_syntax:variable(CState#cs.vars_var)},
                    {context, erl_syntax:variable(CState#cs.context_var)},
                    {context1, erl_syntax:variable(CsCtx#cs.context_var)}
                ]),
            {Ws4, Ast};
        false ->
            Ast = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "_@runtime:builtin_tag("
                        "url,"
                        "_@dispatchrule,"
                        "_@argslist,"
                        "_@vars,"
                        "_@context)",
                [
                    {runtime, erl_syntax:atom(Runtime)},
                    {argslist, ArgsListAst},
                    {dispatchrule, DispatchRuleAst},
                    {vars, erl_syntax:variable(CState#cs.vars_var)},
                    {context, erl_syntax:variable(CState#cs.context_var)}
                ]),
            {Ws2, Ast}
    end;
compile({lib, {_, SrcPos, _}, LibList, Args}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    LibFilenames = lists:map(fun({string_literal, _, Filename}) -> Filename end, LibList),
    Ast = merl:qquote(
        template_compiler_utils:pos(SrcPos),
        "_@runtime:builtin_tag("
                "lib,"
                "_@libfilenames,"
                "_@argslist,"
                "_@vars,"
                "_@context)",
        [
            {runtime, erl_syntax:atom(Runtime)},
            {libfilenames, erl_syntax:abstract(LibFilenames)},
            {argslist, ArgsListAst},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast};
compile({print, {_, SrcPos, _}, Expr}, CState, Ws) ->
    {Ws1, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws),
    Ast = merl:qquote(
        template_compiler_utils:pos(SrcPos),
        "template_compiler_runtime_internal:print(_@expr)",
        [
            {expr, ExprAst}
        ]),
    {Ws1, Ast};
compile({'if', {'as', {_, SrcPos, _}, Expr, undefined}, IfElts, ElseElts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws),
    {Ws2, IfClauseAst} = compile(IfElts, CState, Ws1),
    {Ws3, ElseClauseAst} = compile(ElseElts, CState, Ws2),
    Ast = merl:qquote(
        template_compiler_utils:pos(SrcPos),
        "case _@runtime:to_bool(_@expr, _@context) of "
         "true -> _@ifclause;"
         "false -> _@elseclause "
        "end",
        [
            {runtime, erl_syntax:atom(Runtime)},
            {expr, ExprAst},
            {ifclause, IfClauseAst},
            {elseclause, ElseClauseAst},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]
    ),
    {Ws3, Ast};
compile({'if', {'as', {_, SrcPos, _}, Expr, {identifier, _Pos, Name} = Ident}, IfElts, ElseElts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, V} = template_compiler_utils:var(Ws),
    VAst = erl_syntax:variable(V),
    {CState1, Ws2} = template_compiler_utils:next_vars_var(CState, Ws1),
    {Ws3, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws2),
    {Ws4, ElseClauseAst} = compile(ElseElts, CState, Ws3),
    case is_context_vars_ident(Ident, CState) of
        true ->
            {CsCtx, Ws5} = template_compiler_utils:next_context_var(CState1, Ws4),
            {Ws6, IfClauseAst} = compile(IfElts, CsCtx, Ws5),
            Ast = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "begin "
                  "_@v = _@expr,"
                  "case _@runtime:to_bool(_@v, _@context) of "
                    "true -> "
                        "_@vars1 = _@vars#{ _@name => _@v },"
                        "_@context1 = _@runtime:set_context_vars(_@vars1),"
                        "_@ifclause;",
                    "false -> _@elseclause "
                  "end "
                "end",
                [
                    {runtime, erl_syntax:atom(Runtime)},
                    {v, VAst},
                    {expr, ExprAst},
                    {name, erl_syntax:atom(template_compiler_utils:to_atom(Name))},
                    {ifclause, IfClauseAst},
                    {elseclause, ElseClauseAst},
                    {context, erl_syntax:variable(CState#cs.context_var)},
                    {context1, erl_syntax:variable(CsCtx#cs.context_var)},
                    {vars, erl_syntax:variable(CState#cs.vars_var)},
                    {vars1, erl_syntax:variable(CState1#cs.vars_var)}
                ]),
            {Ws6, Ast};
        false ->
            {Ws5, IfClauseAst} = compile(IfElts, CState1, Ws4),
            Ast = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "begin "
                  "_@v = _@expr,"
                  "case _@runtime:to_bool(_@v, _@context) of "
                    "true -> _@vars1 = _@vars#{ _@name => _@v }, _@ifclause;"
                    "false -> _@elseclause "
                  "end "
                "end",
                [
                    {runtime, erl_syntax:atom(Runtime)},
                    {v, VAst},
                    {expr, ExprAst},
                    {name, erl_syntax:atom(template_compiler_utils:to_atom(Name))},
                    {ifclause, IfClauseAst},
                    {elseclause, ElseClauseAst},
                    {context, erl_syntax:variable(CState#cs.context_var)},
                    {vars, erl_syntax:variable(CState#cs.vars_var)},
                    {vars1, erl_syntax:variable(CState1#cs.vars_var)}
                ]),
            {Ws5, Ast}
    end;
compile({'for', {'in', {_, SrcPos, _}, Idents, ListExpr}, LoopElts, EmptyElts}, #cs{runtime=Runtime} = CState, Ws) ->
    {CsLoop0, WsLoop0} = template_compiler_utils:next_vars_var(CState, Ws#ws{is_forloop_var=false}),
    {CsLoop, WsLoop} = template_compiler_utils:next_context_var(CsLoop0, WsLoop0),
    {WsLoop1, LoopAst} = compile(LoopElts, CsLoop, WsLoop),
    WsEmpty = WsLoop1#ws{
        is_forloop_var=Ws#ws.is_forloop_var
    },
    {WsEmpty1, EmptyAst} = compile(EmptyElts, CState, WsEmpty),
    {WsExpr, ExprAst} = template_compiler_expr:compile(ListExpr, CState, WsEmpty1),
    LoopVarsAst = erl_syntax:abstract(idents_as_atoms(Idents)),
    IsContextVars = is_context_vars_ident(Idents, CState),
    Ast = merl:qquote(
        template_compiler_utils:pos(SrcPos),
        "template_compiler_runtime_internal:forloop("
            "_@isforloopvar,"
            "_@expr,"
            "_@loopvars,"
            "fun(_@varsloop, _@contextloop) -> _@loop end,"
            "fun() -> _@empty end,"
            "_@runtime,"
            "_@is_context_vars,"
            "_@vars,"
            "_@context"
        ")",
        [
            {runtime, erl_syntax:atom(Runtime)},
            {expr, ExprAst},
            {loopvars, LoopVarsAst},
            {isforloopvar, erl_syntax:atom(WsLoop1#ws.is_forloop_var)},
            {varsloop, erl_syntax:variable(CsLoop#cs.vars_var)},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {is_context_vars, erl_syntax:abstract(IsContextVars)},
            {loop, LoopAst},
            {empty, EmptyAst},
            {context, erl_syntax:variable(CState#cs.context_var)},
            {contextloop, erl_syntax:variable(CsLoop#cs.context_var)}
        ]),
    {WsExpr, Ast};
compile({'with', {{_, SrcPos, _}, Exprs, Idents}, Elts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ExprAsts} = expr_list(Exprs, CState, Ws),
    VarsAsts = erl_syntax:abstract(idents_as_atoms(Idents)),
    ExprListAst = erl_syntax:list(ExprAsts),
    {CsWith, Ws2} = template_compiler_utils:next_vars_var(CState, Ws1),
    case is_context_vars_ident(Idents, CState) of
        true ->
            {CsCtx, Ws3} = template_compiler_utils:next_context_var(CsWith, Ws2),
            {Ws4, BodyAst} = compile(Elts, CsCtx, Ws3),
            Ast = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "begin "
                    "_@vars1 = template_compiler_runtime_internal:with_vars("
                        "_@varsasts,"
                        "_@exprlist,"
                        "_@vars),"
                    "_@context1 = _@runtime:set_context_vars(_@vars1, _@context),"
                    "_@body "
                "end",
                [
                    {runtime, erl_syntax:atom(Runtime)},
                    {varsasts, VarsAsts},
                    {exprlist, ExprListAst},
                    {body, BodyAst},
                    {vars, erl_syntax:variable(CState#cs.vars_var)},
                    {vars1, erl_syntax:variable(CsCtx#cs.vars_var)},
                    {context, erl_syntax:variable(CState#cs.context_var)},
                    {context1, erl_syntax:variable(CsCtx#cs.context_var)}
                ]),
            {Ws4, Ast};
        false ->
            {Ws3, BodyAst} = compile(Elts, CsWith, Ws2),
            Ast = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "begin "
                    "_@vars1 = template_compiler_runtime_internal:with_vars("
                        "_@varsasts,"
                        "_@exprlist,"
                        "_@vars),"
                    "_@body "
                "end",
                [
                    {varsasts, VarsAsts},
                    {exprlist, ExprListAst},
                    {body, BodyAst},
                    {vars, erl_syntax:variable(CState#cs.vars_var)},
                    {vars1, erl_syntax:variable(CsWith#cs.vars_var)}
                ]),
            {Ws3, Ast}
    end;
compile({cache, {{_, SrcPos, _}, CacheTime, Args}, Elts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, ArgsList} = with_args(Args, CState, Ws, false),
    {Ws2, CacheTimeAst} = template_compiler_expr:compile(CacheTime, CState, Ws1),
    {CsBody, Ws3} = template_compiler_utils:next_vars_var(CState, Ws2),
    {CsBody1, Ws4} = template_compiler_utils:next_vars_var(CsBody, Ws3),
    {Ws5, BodyAst} = compile(Elts, CsBody1, Ws4),
    Unique = template_compiler_runtime_internal:unique(),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    Ast = merl:qquote(
        template_compiler_utils:pos(SrcPos),
        "_@runtime:cache_tag("
                "_@cachetime,"
                "_@unique,"
                "_@argslist,"
                "fun(_@varsbody, _@contextbody) -> _@body end,"
                "_@vars,"
                "_@context)",
        [
            {runtime, erl_syntax:atom(Runtime)},
            {unique, erl_syntax:abstract(Unique)},
            {cachetime, CacheTimeAst},
            {argslist, ArgsListAst},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {body, BodyAst},
            {context, erl_syntax:variable(CState#cs.context_var)},
            {varsbody, erl_syntax:variable(CsBody1#cs.vars_var)},
            {contextbody, erl_syntax:variable(CsBody1#cs.context_var)}
        ]),
    {Ws5, Ast};
compile({javascript, {_, SrcPos, _}, Elts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, BodyAst} = compile(Elts, CState, Ws),
    Ast = merl:qquote(
        template_compiler_utils:pos(SrcPos),
        "_@runtime:javascript_tag("
                    "_@body,"
                    "_@vars,"
                    "_@context)",
        [
            {runtime, erl_syntax:atom(Runtime)},
            {body, BodyAst},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)}
        ]),
    {Ws1, Ast};
compile({filter, {{_, SrcPos, _}, Filters}, Elts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, BodyAst} = compile(Elts, CState, Ws),
    Expr = lists:foldl(
                fun({filter, Name, Args}, Acc) ->
                    {apply_filter, Acc, {filter, Name, Args}}
                end,
                {ast, BodyAst},
                Filters),
    {Ws2, ExprAst} = template_compiler_expr:compile(Expr, CState, Ws1),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "_@runtime:to_render_result(_@expr, _@vars, _@context)",
            [
                {runtime, erl_syntax:atom(Runtime)},
                {expr, ExprAst},
                {context, erl_syntax:variable(CState#cs.context_var)},
                {vars, erl_syntax:variable(CState#cs.vars_var)}
            ]),
    {Ws2, Ast};
compile({spaceless, {_, SrcPos, _}, Elts}, #cs{runtime=Runtime} = CState, Ws) ->
    {Ws1, BodyAst} = compile(Elts, CState, Ws),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "_@runtime:spaceless_tag(_@body, _@vars, _@context)",
            [
                {runtime, erl_syntax:atom(Runtime)},
                {body, BodyAst},
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
compile({cycle_compat, TagPos, Names}, CState, Ws) ->
    Exprs = [{string_literal, Pos, Name} || {identifier, Pos, Name} <- Names ],
    compile({cycle, TagPos, Exprs}, CState, Ws);
compile({cycle, _TagPos, []}, _CState, Ws) ->
    {Ws, erl_syntax:list([])};
compile({cycle, {_, SrcPos, _} = TagSrc, Exprs}, CState, Ws) ->
    {Ws1, Var} = template_compiler_utils:var(Ws),
    {Ws2, ExprList} = expr_list(Exprs, CState, Ws1),
    N = length(Exprs),
    Clauses = lists:zip(lists:seq(0,N-1), ExprList),
    ClauseAsts = [ erl_syntax:clause([erl_syntax:integer(Nr)], none, [Expr]) || {Nr,Expr} <- Clauses ],
    ValueAst = merl:qquote(
                template_compiler_utils:pos(SrcPos),
                "maps:get(counter0, _@v) rem _@n",
                [
                    {v, erl_syntax:variable(Var)},
                    {n, erl_syntax:integer(N)}
                ]),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "case maps:get(forloop, _@vars, undefined) of "
                "undefined -> _@first; "
                "_@v -> _@caseast "
            "end",
            [
                {first, hd(ExprList)},
                {v, erl_syntax:variable(Var)},
                {vars, erl_syntax:variable(CState#cs.vars_var)},
                {caseast, erl_syntax:case_expr(ValueAst, ClauseAsts)}
            ]),
    io:format("~n~n~p~n~n", [Ast]),
    compile({value, TagSrc, {ast, Ast}, []}, CState, Ws2#ws{is_forloop_var=true}).


include({_, SrcPos, _}, Method, Template, ArgsList, IsContextVars, #cs{runtime=Runtime} = CState, Ws) when is_atom(Method) ->
    {Ws1, TemplateAst} = template_compiler_expr:compile(Template, CState, Ws),
    ArgsListAst = erl_syntax:list([ erl_syntax:tuple([A,B]) || {A,B} <- ArgsList ]),
    Ast = merl:qquote(
        template_compiler_utils:pos(SrcPos),
        "template_compiler_runtime_internal:include("
            "_@srcpos,"
            "_@method,"
            "_@template,"
            "_@args,"
            "_@runtime,"
            "_@context_vars,"
            "_@is_context_vars,"
            "_@vars,"
            "_@context)",
        [
            {srcpos, erl_syntax:abstract(SrcPos)},
            {method, erl_syntax:abstract(Method)},
            {template, TemplateAst},
            {args, ArgsListAst},
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {runtime, erl_syntax:atom(Runtime)},
            {context, erl_syntax:variable(CState#cs.context_var)},
            {context_vars, erl_syntax:abstract(CState#cs.context_vars)},
            {is_context_vars, erl_syntax:abstract(IsContextVars)}
        ]),
    {Ws1, Ast}.


catinclude({_, SrcPos, _}, Method, Template, IdAst, ArgsList, IsContextVars, #cs{runtime=Runtime} = CState, Ws) when is_atom(Method) ->
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
                    "_@context_vars,",
                    "_@IsContextVars@,",
                    "_@vars,",
                    "_@context)"
        ],
        [
            {vars, erl_syntax:variable(CState#cs.vars_var)},
            {context, erl_syntax:variable(CState#cs.context_var)},
            {context_vars, erl_syntax:abstract(CState#cs.context_vars)}
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


is_context_vars_arg({{identifier, _, Ident}, _Val}, CState) ->
    lists:member(Ident, CState#cs.context_vars);
is_context_vars_arg(Args, CState) when is_list(Args) ->
    lists:any(
            fun
                ({{identifier, _, Ident}, _Val}) ->
                    lists:member(Ident, CState#cs.context_vars)
            end,
            Args).

is_context_vars_ident({identifier, _, Ident}, CState) ->
    lists:member(Ident, CState#cs.context_vars);
is_context_vars_ident(Idents, CState) when is_list(Idents) ->
    lists:any(
            fun
                ({identifier, _, Ident}) ->
                    lists:member(Ident, CState#cs.context_vars)
            end,
            Idents).

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

trans_ext({string_literal, SrcPos, Text}, Args, CState, Ws) ->
    Unescaped = template_compiler_utils:unescape_string_literal(Text),
    trans_ext_1({trans, [{en, Unescaped}]}, Args, SrcPos, CState, Ws);
trans_ext({trans_literal, SrcPos, Tr}, Args, CState, Ws) ->
    trans_ext_1(Tr, Args, SrcPos, CState, Ws).

trans_ext_1({trans, Tr}, Args, SrcPos, #cs{runtime=Runtime} = CState, Ws) ->
    Split = [ {Lang, split_string(Txt, <<>>, [])} || {Lang, Txt} <- Tr ],
    {FunAsts, Ws1} = lists:foldl(
                    fun({Lang, Parts}, {FAcc, WsAcc}) ->
                        {WsAcc1, Fun} = trans_ext_fun(Parts, Args, CState, WsAcc),
                        {[{Lang, Fun}|FAcc], WsAcc1}
                    end,
                    {[], Ws},
                    Split),
    FunListAst = erl_syntax:list(
                    lists:map(
                        fun ({Lng,FunAst}) ->
                            erl_syntax:tuple([
                                    erl_syntax:atom(Lng),
                                    FunAst
                                ])
                        end,
                        FunAsts)),
    Ast = merl:qquote(
            template_compiler_utils:pos(SrcPos),
            "(_@runtime:lookup_translation({trans, _@funlist}, _@vars, _@context))()",
            [
                {runtime, erl_syntax:atom(Runtime)},
                {funlist, FunListAst},
                {context, erl_syntax:variable(CState#cs.context_var)},
                {vars, erl_syntax:variable(CState#cs.vars_var)}
            ]),
    {Ws1, Ast}.

trans_ext_fun(Parts, Args, CState, Ws) ->
    Parts1 = [ P || P <- Parts, P =/= <<>> ],
    Args1 = [{Ident, {TagSrc, ArgExpr}} || {{identifier, _, Ident} = TagSrc, ArgExpr} <- Args],
    {Asts,Ws1} = lists:foldr(
                    fun
                        (B, {Acc, WsAcc}) when is_binary(B) ->
                            {[erl_syntax:abstract(B)|Acc], WsAcc};
                        ({var, Name}, {Acc, WsAcc}) ->
                            case proplists:get_value(Name, Args1) of
                                undefined ->
                                    {Acc, WsAcc};
                                {TagSrc, Expr} ->
                                    {WsAcc1, ExprAst} = compile({value, TagSrc, Expr, []}, CState, WsAcc),
                                    {[ExprAst|Acc], WsAcc1}
                            end
                    end,
                    {[], Ws},
                    Parts1),
    {Ws1, ?Q("fun() -> _@list end", [{list, erl_syntax:list(Asts)}])}.

split_string(<<>>, Acc, Parts) ->
    lists:reverse([Acc|Parts]);
split_string(<<"{{", T/binary>>, Acc, Parts) ->
    split_string(T, <<Acc/binary, ${>>, Parts);
split_string(<<"}}", T/binary>>, Acc, Parts) ->
    split_string(T, <<Acc/binary, $}>>, Parts);
split_string(<<"{", T/binary>>, Acc, Parts) ->
    split_string_name(T, <<>>, [Acc|Parts]);
split_string(<<C/utf8, T/binary>>, Acc, Parts) ->
    split_string(T, <<Acc/binary, C/utf8>>, Parts).

split_string_name(<<>>, Acc, Parts) ->
    lists:reverse([Acc|Parts]);
split_string_name(<<"}", T/binary>>, Acc, Parts) ->
    split_string(T, <<>>, [{var, z_string:trim(Acc)}|Parts]);
split_string_name(<<C/utf8, T/binary>>, Acc, Parts) ->
    split_string_name(T, <<Acc/binary, C/utf8>>, Parts).
