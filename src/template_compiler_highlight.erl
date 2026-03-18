%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Generate syntax highlighted HTML from template parse trees.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(template_compiler_highlight).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    highlight_file/1,
    highlight_binary/1,
    highlight_binary/2,
    highlight_module/1
]).

-define(DUMMY_TEMPLATE_FILENAME, <<"template.tpl">>).

-define(STYLE_PRE, <<"background:#f8fafc;color:#0f172a;padding:1rem 1.25rem;border:1px solid #e2e8f0;border-radius:8px;overflow:auto;white-space:pre-wrap;font-family:Menlo,Consolas,monospace;font-size:12px;line-height:1.5;">>).
-define(STYLE_LINE, <<"display:block;padding-left:4.5em;position:relative;min-height:1.5em;">>).
-define(STYLE_LINE_NO, <<"position:absolute;left:0;width:3.5em;text-align:right;color:#94a3b8;user-select:none;">>).
-define(STYLE_CHECKBOX, <<"display:inline-flex;align-items:center;vertical-align:middle;margin-right:0.45rem;">>).
-define(STYLE_TEXT, <<"color:#334155;">>).
-define(STYLE_DELIM, <<"color:#64748b;">>).
-define(STYLE_KEYWORD, <<"color:#0f766e;font-weight:600;">>).
-define(STYLE_STRING, <<"color:#b45309;">>).
-define(STYLE_IDENT, <<"color:#1d4ed8;">>).
-define(STYLE_LITERAL, <<"color:#7c3aed;">>).
-define(STYLE_OPERATOR, <<"color:#be123c;font-weight:600;">>).
-define(STYLE_COMMENT, <<"color:#94a3b8;font-style:italic;">>).

%% @doc Return syntax highlighted HTML for a template source file.
-spec highlight_file(file:filename_all()) -> {ok, binary()} | {error, term()}.
highlight_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            highlight_binary(Bin, Filename, []);
        {error, _} = Error ->
            Error
    end.

%% @doc Return syntax highlighted HTML for a compiled template module
%%      and add checkbox inputs for its debug points.
-spec highlight_module(module()) -> {ok, binary()} | {error, term()}.
highlight_module(Module) when is_atom(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case template_compiler:is_template_module(Module)
                andalso erlang:function_exported(Module, filename, 0)
                andalso erlang:function_exported(Module, debug_points, 0)
            of
                true ->
                    highlight_file(Module:filename(), Module:debug_points());
                false ->
                    {error, bad_template_module}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Return syntax highlighted HTML for an in-memory template source.
-spec highlight_binary(binary()) -> {ok, binary()} | {error, term()}.
highlight_binary(Bin) when is_binary(Bin) ->
    highlight_binary(Bin, ?DUMMY_TEMPLATE_FILENAME).

%% @doc Return syntax highlighted HTML for an in-memory template source.
-spec highlight_binary(binary(), file:filename_all()) -> {ok, binary()} | {error, term()}.
highlight_binary(Bin, Filename) when is_binary(Bin) ->
    highlight_binary(Bin, Filename, []).

highlight_file(Filename, DebugPoints) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            highlight_binary(Bin, Filename, DebugPoints);
        {error, _} = Error ->
            Error
    end.

highlight_binary(Bin, Filename, DebugPoints) when is_binary(Bin) ->
    case template_compiler_scanner:scan(Filename, Bin) of
        {ok, Tokens} ->
            Tokens1 = normalize_tokens(Tokens),
            case template_compiler_parser:parse(Tokens1) of
                {ok, Tree} ->
                    {ok, iolist_to_binary(render_document(Tree, debug_points_map(DebugPoints)))};
                {error, _} = Error ->
                    case maybe_parse_trans_tag(Tokens1) of
                        {ok, Tree} ->
                            {ok, iolist_to_binary(render_document(Tree, debug_points_map(DebugPoints)))};
                        error ->
                            Error
                    end
            end;
        {error, _} = Error ->
            Error
    end.

render_document(Tree, DebugPointMap) ->
    Content = iolist_to_binary(render_template(Tree, DebugPointMap)),
    [
        <<"<pre class=\"template-compiler-highlight\" style=\"">>, ?STYLE_PRE, <<"\"><code>">>,
        render_lines(Content),
        <<"</code></pre>">>
    ].

render_template({base, Elements}, DebugPointMap) ->
    render_elements(Elements, DebugPointMap);
render_template({extends, Template, Elements}, DebugPointMap) ->
    [
        render_tag([keyword(<<"extends">>), space(), render_expr(Template)]),
        maybe_newline(Elements),
        render_elements(Elements, DebugPointMap)
    ];
render_template({overrules, Elements}, DebugPointMap) ->
    [
        render_tag([keyword(<<"overrules">>)]),
        maybe_newline(Elements),
        render_elements(Elements, DebugPointMap)
    ].

render_elements(Elements, DebugPointMap) when is_list(Elements) ->
    [ render_element(E, DebugPointMap) || E <- Elements ].

render_element(Element, DebugPointMap) ->
    [
        render_debug_checkbox(element_src_pos(Element), DebugPointMap),
        render_element_content(Element, DebugPointMap)
    ].

render_element_content({text, _SrcPos, Text}, _DebugPointMap) ->
    span(?STYLE_TEXT, Text);
render_element_content({value, _OpenVar, Expr, []}, _DebugPointMap) ->
    render_var(render_expr(Expr));
render_element_content({value, _OpenVar, Expr, With}, _DebugPointMap) ->
    render_var([render_expr(Expr), space(), keyword(<<"with">>), space(), render_args(With)]);
render_element_content({trans_ext, Tr, Args}, _DebugPointMap) ->
    render_tag([keyword(<<"trans">>), space(), render_expr({trans_literal, undefined, Tr}) | render_args_tail(Args)]);
render_element_content({date, now, _Tag, Format}, _DebugPointMap) ->
    render_tag([keyword(<<"now">>), space(), render_expr(Format)]);
render_element_content({load, Names}, _DebugPointMap) ->
    render_tag([keyword(<<"load">>) | render_load_names(Names)]);
render_element_content({block, {identifier, _Pos, Name}, Elts}, DebugPointMap) ->
    [
        render_tag([keyword(<<"block">>), space(), ident(Name)]),
        render_elements(Elts, DebugPointMap),
        render_tag([keyword(<<"endblock">>)])
    ];
render_element_content({inherit, _Tag}, _DebugPointMap) ->
    render_tag([keyword(<<"inherit">>)]);
render_element_content({include, _Tag, Method, Template, Args}, _DebugPointMap) ->
    render_include(<<"include">>, Method, Template, undefined, Args);
render_element_content({catinclude, _Tag, Method, Template, IdExpr, Args}, _DebugPointMap) ->
    render_include(<<"catinclude">>, Method, Template, IdExpr, Args);
render_element_content({compose, {_Tag, Template, Args}, Blocks}, DebugPointMap) ->
    render_compose(<<"compose">>, Template, undefined, Args, Blocks, DebugPointMap);
render_element_content({catcompose, {_Tag, Template, IdExpr, Args}, Blocks}, DebugPointMap) ->
    render_compose(<<"catcompose">>, Template, IdExpr, Args, Blocks, DebugPointMap);
render_element_content({'call', {identifier, _Pos, Name}, Args}, _DebugPointMap) ->
    render_tag([keyword(<<"call">>), space(), ident(Name) | render_args_tail(Args)]);
render_element_content({call_with, {identifier, _Pos, Name}, Expr}, _DebugPointMap) ->
    render_tag([keyword(<<"call">>), space(), ident(Name), space(), keyword(<<"with">>), space(), render_expr(Expr)]);
render_element_content({custom_tag, {identifier, _Pos, Name}, Args}, _DebugPointMap) ->
    render_tag([ident(Name) | render_args_tail(Args)]);
render_element_content({Tag, _TagPos, Expr, Args}, _DebugPointMap) when Tag =:= image; Tag =:= image_url; Tag =:= image_data_url; Tag =:= media; Tag =:= url ->
    render_tag([keyword(atom_to_binary(Tag, utf8)), space(), render_expr(Expr) | render_args_tail(Args)]);
render_element_content({LibTag, _TagPos, LibList, Args}, _DebugPointMap) when LibTag =:= lib; LibTag =:= lib_url ->
    render_tag([keyword(atom_to_binary(LibTag, utf8)) | render_libs_and_args(LibList, Args)]);
render_element_content({print, _TagPos, Expr}, _DebugPointMap) ->
    render_tag([keyword(<<"print">>), space(), render_expr(Expr)]);
render_element_content({'if', Cond, IfElts, ElseElts}, DebugPointMap) ->
    render_if(Cond, IfElts, ElseElts, DebugPointMap);
render_element_content({'ifequal', {Tag, Expr1, Expr2}, IfElts, ElseElts}, DebugPointMap) ->
    render_if_compare(<<"ifequal">>, Tag, Expr1, Expr2, IfElts, ElseElts, DebugPointMap);
render_element_content({'ifnotequal', {Tag, Expr1, Expr2}, IfElts, ElseElts}, DebugPointMap) ->
    render_if_compare(<<"ifnotequal">>, Tag, Expr1, Expr2, IfElts, ElseElts, DebugPointMap);
render_element_content({for, {'in', _Tag, Idents, ListExpr}, LoopElts, EmptyElts}, DebugPointMap) ->
    [
        render_tag([keyword(<<"for">>), space(), render_ident_list(Idents), space(), keyword(<<"in">>), space(), render_expr(ListExpr)]),
        render_elements(LoopElts, DebugPointMap),
        render_empty_part(EmptyElts, DebugPointMap),
        render_tag([keyword(<<"endfor">>)])
    ];
render_element_content({with, {_Tag, Exprs, Idents}, Elts}, DebugPointMap) ->
    [
        render_tag([keyword(<<"with">>), space(), render_expr_list(Exprs), space(), keyword(<<"as">>), space(), render_ident_list(Idents)]),
        render_elements(Elts, DebugPointMap),
        render_tag([keyword(<<"endwith">>)])
    ];
render_element_content({cache, {CacheTime, Args, _Tag}, Elts}, DebugPointMap) ->
    [
        render_tag([keyword(<<"cache">>) | render_cache_open(CacheTime, Args)]),
        render_elements(Elts, DebugPointMap),
        render_tag([keyword(<<"endcache">>)])
    ];
render_element_content({javascript, _TagPos, Elts}, DebugPointMap) ->
    [
        render_tag([keyword(<<"javascript">>)]),
        render_elements(Elts, DebugPointMap),
        render_tag([keyword(<<"endjavascript">>)])
    ];
render_element_content({filter, {_Tag, Filters}, Elts}, DebugPointMap) ->
    [
        render_tag([keyword(<<"filter">>), space(), render_filters(Filters)]),
        render_elements(Elts, DebugPointMap),
        render_tag([keyword(<<"endfilter">>)])
    ];
render_element_content({spaceless, _TagPos, Elts}, DebugPointMap) ->
    [
        render_tag([keyword(<<"spaceless">>)]),
        render_elements(Elts, DebugPointMap),
        render_tag([keyword(<<"endspaceless">>)])
    ];
render_element_content({autoescape, {identifier, _Pos, OnOff}, Elts}, DebugPointMap) ->
    [
        render_tag([keyword(<<"autoescape">>), space(), ident(OnOff)]),
        render_elements(Elts, DebugPointMap),
        render_tag([keyword(<<"endautoescape">>)])
    ];
render_element_content({cycle, _TagPos, Exprs}, _DebugPointMap) ->
    render_tag([keyword(<<"cycle">>), space(), render_expr_list(Exprs)]);
render_element_content(Other, _DebugPointMap) ->
    span(?STYLE_COMMENT, iolist_to_binary(io_lib:format("~tp", [Other]))).

render_if({'as', _Tag, Expr, undefined}, IfElts, ElseElts, DebugPointMap) ->
    [
        render_tag([keyword(<<"if">>), space(), render_expr(Expr)]),
        render_elements(IfElts, DebugPointMap),
        render_else_part(ElseElts, DebugPointMap),
        render_tag([keyword(<<"endif">>)])
    ];
render_if({'as', _Tag, Expr, {identifier, _Pos, Name}}, IfElts, ElseElts, DebugPointMap) ->
    [
        render_tag([keyword(<<"if">>), space(), render_expr(Expr), space(), keyword(<<"as">>), space(), ident(Name)]),
        render_elements(IfElts, DebugPointMap),
        render_else_part(ElseElts, DebugPointMap),
        render_tag([keyword(<<"endif">>)])
    ].

render_if_compare(Name, _Tag, Expr1, Expr2, IfElts, ElseElts, DebugPointMap) ->
    [
        render_tag([keyword(Name), space(), render_expr(Expr1), space(), render_expr(Expr2)]),
        render_elements(IfElts, DebugPointMap),
        render_else_part(ElseElts, DebugPointMap),
        render_tag([keyword(<<"end", Name/binary>>)])
    ].

render_else_part([], _DebugPointMap) ->
    [];
render_else_part(ElseElts, DebugPointMap) ->
    [render_tag([keyword(<<"else">>)]), render_elements(ElseElts, DebugPointMap)].

render_empty_part([], _DebugPointMap) ->
    [];
render_empty_part(EmptyElts, DebugPointMap) ->
    [render_tag([keyword(<<"empty">>)]), render_elements(EmptyElts, DebugPointMap)].

render_include(Name, Method, Template, undefined, Args) ->
    render_tag(render_include_parts(Name, Method, [render_expr(Template)], Args));
render_include(Name, Method, Template, IdExpr, Args) ->
    render_tag(render_include_parts(Name, Method, [render_expr(Template), space(), render_expr(IdExpr)], Args)).

render_include_parts(Name, Method, Parts, Args) ->
    MethodParts = case Method of
        optional -> [keyword(<<"optional">>), space()];
        all -> [keyword(<<"all">>), space()];
        normal -> []
    end,
    MethodParts ++ [keyword(Name), space()] ++ Parts ++ render_args_tail(Args).

render_compose(Name, Template, undefined, Args, Blocks, DebugPointMap) ->
    [
        render_tag([keyword(Name), space(), render_expr(Template) | render_args_tail(Args)]),
        render_elements(Blocks, DebugPointMap),
        render_tag([keyword(<<"endcompose">>)])
    ];
render_compose(Name, Template, IdExpr, Args, Blocks, DebugPointMap) ->
    [
        render_tag([keyword(Name), space(), render_expr(Template), space(), render_expr(IdExpr) | render_args_tail(Args)]),
        render_elements(Blocks, DebugPointMap),
        render_tag([keyword(<<"endcompose">>)])
    ].

render_expr({string_literal, _Pos, Text}) ->
    string(Text);
render_expr({trans_literal, _Pos, {trans, Tr}}) ->
    [operator(<<"_">>), string(proplists:get_value(en, Tr, <<>>))];
render_expr({number_literal, _Pos, Nr}) ->
    literal(Nr);
render_expr({atom_literal, _Pos, Atom}) ->
    [operator(<<"`">>), literal(Atom), operator(<<"`">>)];
render_expr({find_value, LookupList}) ->
    render_lookup(LookupList);
render_expr({auto_id, {Ident, Var}}) ->
    [operator(<<"#">>), render_lookup_ident(Ident), operator(<<"-">>), render_lookup_ident(Var)];
render_expr({auto_id, Ident}) ->
    [operator(<<"#">>), render_lookup_ident(Ident)];
render_expr({map_value, Args}) ->
    [operator(<<"%{">>), render_args(Args), operator(<<"}">>)];
render_expr({tuple_value, {identifier, _Pos, Name}, Args}) ->
    [operator(<<"{">>), ident(Name), maybe_space_args(Args), render_args(Args), operator(<<"}">>)];
render_expr({value_list, Exprs}) ->
    [operator(<<"[">>), render_expr_list(Exprs), operator(<<"]">>)];
render_expr({expr, {Op, Token}, Arg}) ->
    render_unary_expr(Op, Token, Arg);
render_expr({expr, {Op, Token}, Arg1, Arg2}) ->
    render_binary_expr(Op, Token, Arg1, Arg2);
render_expr({apply_filter, Expr, {filter, {identifier, _Pos, Name}, Args}}) ->
    [render_expr(Expr), operator(<<"|">>), ident(Name), render_filter_args(Args)];
render_expr({model, Path, none}) ->
    [keyword(<<"m">>), render_model_path(Path)];
render_expr({model, Path, Payload}) ->
    [keyword(<<"m">>), render_model_path(Path), operator(<<"::">>), render_expr(Payload)];
render_expr(Value) when Value =:= true; Value =:= false; Value =:= undefined ->
    literal(atom_to_binary(Value, utf8));
render_expr(Value) when is_binary(Value) ->
    string(Value);
render_expr(Value) ->
    span(?STYLE_COMMENT, iolist_to_binary(io_lib:format("~tp", [Value]))).

render_unary_expr(_Op, {TokenName, _Pos, TokenText}, Arg) when TokenName =:= '-'; TokenName =:= not_keyword ->
    [operator(token_text(TokenText)), render_expr(Arg)];
render_unary_expr(Op, _Token, Arg) ->
    [operator(atom_to_binary(Op, utf8)), render_expr(Arg)].

render_binary_expr(_Op, {_TokenName, _Pos, TokenText}, Arg1, Arg2) ->
    [render_expr(Arg1), space(), operator(token_text(TokenText)), space(), render_expr(Arg2)].

render_lookup([Part]) ->
    render_lookup_ident(Part);
render_lookup([Part|Rest]) ->
    [render_lookup_ident(Part), operator(<<".">>), render_lookup(Rest)].

render_lookup_ident({identifier, _Pos, Name}) ->
    ident(Name);
render_lookup_ident({number_literal, _Pos, Nr}) ->
    literal(Nr);
render_lookup_ident({string_literal, _Pos, Text}) ->
    string(Text);
render_lookup_ident(Part) ->
    span(?STYLE_COMMENT, iolist_to_binary(io_lib:format("~tp", [Part]))).

render_model_path([]) ->
    [];
render_model_path([{identifier, _Pos, Name} | Rest]) ->
    [operator(<<".">>), ident(Name), render_model_path(Rest)];
render_model_path([{expr, Expr} | Rest]) ->
    [operator(<<"[">>), render_expr(Expr), operator(<<"]">>), render_model_path(Rest)];
render_model_path([Part|Rest]) ->
    [operator(<<"[">>), render_expr(Part), operator(<<"]">>), render_model_path(Rest)].

render_expr_list([]) ->
    [];
render_expr_list([Expr]) ->
    render_expr(Expr);
render_expr_list([Expr|Rest]) ->
    [render_expr(Expr), operator(<<",">>), space(), render_expr_list(Rest)].

render_ident_list([Ident]) ->
    render_lookup_ident(Ident);
render_ident_list([Ident|Rest]) ->
    [render_lookup_ident(Ident), operator(<<",">>), space(), render_ident_list(Rest)].

render_args([]) ->
    [];
render_args([{Ident, true}]) ->
    render_lookup_ident(Ident);
render_args([{Ident, true}|Rest]) ->
    [render_lookup_ident(Ident), space(), render_args(Rest)];
render_args([{Ident, Expr}]) ->
    [render_lookup_ident(Ident), operator(<<"=">>), render_expr(Expr)];
render_args([{Ident, Expr}|Rest]) ->
    [render_lookup_ident(Ident), operator(<<"=">>), render_expr(Expr), space(), render_args(Rest)].

render_args_tail([]) ->
    [];
render_args_tail(Args) ->
    [space(), render_args(Args)].

render_filter_args([]) ->
    [];
render_filter_args(Args) ->
    [operator(<<":">>), render_expr_list(Args)].

render_filters([]) ->
    [];
render_filters([{filter, {identifier, _Pos, Name}, Args}]) ->
    [ident(Name), render_filter_args(Args)];
render_filters([{filter, {identifier, _Pos, Name}, Args}|Rest]) ->
    [ident(Name), render_filter_args(Args), space(), render_filters(Rest)].

render_load_names([]) ->
    [];
render_load_names([Name]) ->
    [space(), render_lookup_ident(Name)];
render_load_names([Name|Rest]) ->
    [space(), render_lookup_ident(Name), render_load_names(Rest)].

render_libs_and_args(LibList, Args) ->
    LibParts = case LibList of
        [] -> [];
        _ -> [space(), render_expr_list(LibList)]
    end,
    LibParts ++ render_args_tail(Args).

render_cache_open(undefined, Args) ->
    render_args_tail(Args);
render_cache_open(CacheTime, Args) ->
    [space(), render_expr(CacheTime)] ++ render_args_tail(Args).

render_var(Content) ->
    [delim(<<"{{ ">>), Content, delim(<<" }}">>)].

render_tag(Content) ->
    [delim(<<"{% ">>), Content, delim(<<" %}">>)].

maybe_newline([]) ->
    [];
maybe_newline(_Elements) ->
    <<"\n">>.

maybe_space_args([]) ->
    [];
maybe_space_args(_Args) ->
    space().

space() ->
    <<" ">>.

keyword(Text) ->
    span(?STYLE_KEYWORD, Text).

string(Text) ->
    [operator(<<"\"">>), span(?STYLE_STRING, escape_text(Text)), operator(<<"\"">>)].

ident(Text) ->
    span(?STYLE_IDENT, Text).

literal(Text) ->
    span(?STYLE_LITERAL, z_convert:to_binary(Text)).

operator(Text) ->
    span(?STYLE_OPERATOR, Text).

delim(Text) ->
    span(?STYLE_DELIM, Text).

span(Style, Text) ->
    span_lines(Style, escape_text(Text)).

escape_text(Text) when is_binary(Text) ->
    z_html:escape(Text);
escape_text(Text) when is_list(Text) ->
    z_html:escape(iolist_to_binary(Text));
escape_text(Text) ->
    z_html:escape(z_convert:to_binary(Text)).

normalize_tokens(Tokens) ->
    normalize_tokens(Tokens, []).

normalize_tokens([], Acc) ->
    lists:reverse(Acc);
normalize_tokens([{trans_keyword, _, _} = Trans, {string_literal, SrcPos, Text} | Ts], Acc) ->
    Acc1 = [{trans_literal, SrcPos, unescape_trim(Text)}, Trans | Acc],
    normalize_tokens(Ts, Acc1);
normalize_tokens([{trans_text, SrcPos, Text} | Ts], Acc) ->
    Acc1 = [{trans_text, SrcPos, unescape_trim(Text)} | Acc],
    normalize_tokens(Ts, Acc1);
normalize_tokens([{trans_literal, SrcPos, Text} | Ts], Acc) ->
    Acc1 = [{trans_literal, SrcPos, unescape_trim(Text)} | Acc],
    normalize_tokens(Ts, Acc1);
normalize_tokens([{string_literal, SrcPos, Text} | Ts], Acc) ->
    Acc1 = [{string_literal, SrcPos, template_compiler_utils:unescape_string_literal(Text)} | Acc],
    normalize_tokens(Ts, Acc1);
normalize_tokens([T | Ts], Acc) ->
    normalize_tokens(Ts, [T | Acc]).

unescape_trim(Text) ->
    Unescaped = template_compiler_utils:unescape_string_literal(Text),
    z_string:trim(Unescaped).

maybe_parse_trans_tag([
    {open_tag, _OpenPos, _Open},
    {trans_keyword, _TransPos, _Keyword},
    {trans_literal, _LiteralPos, Text}
    | Rest
]) ->
    case split_close_tag(Rest) of
        {ok, ArgTokens, _CloseTag} ->
            case parse_trans_args(ArgTokens) of
                {ok, Args} ->
                    {ok, {base, [{trans_ext, normalize_trans_text(Text), Args}]}};
                error ->
                    error
            end;
        error ->
            error
    end;
maybe_parse_trans_tag(_) ->
    error.

normalize_trans_text({trans, _} = Tr) ->
    Tr;
normalize_trans_text(Text) when is_binary(Text) ->
    {trans, [{en, Text}]}.

split_close_tag(Tokens) ->
    case lists:reverse(Tokens) of
        [{close_tag, _ClosePos, _Close} = CloseTag | RevArgs] ->
            {ok, lists:reverse(RevArgs), CloseTag};
        _ ->
            error
    end.

parse_trans_args([]) ->
    {ok, []};
parse_trans_args([{identifier, _Pos, _Name} = Ident | Rest]) ->
    case Rest of
        [] ->
            {ok, [{Ident, true}]};
        [{identifier, _NextPos, _NextName} | _] ->
            case parse_trans_args(Rest) of
                {ok, Args} -> {ok, [{Ident, true} | Args]};
                error -> error
            end;
        [{equal, _EqPos, _Eq} | ExprTokens] ->
            case split_trans_arg_expr(ExprTokens) of
                {ok, Expr, Rest1} ->
                    case parse_trans_args(Rest1) of
                        {ok, Args} -> {ok, [{Ident, Expr} | Args]};
                        error -> error
                    end;
                error ->
                    error
            end;
        _ ->
            error
    end;
parse_trans_args(_) ->
    error.

split_trans_arg_expr(Tokens) ->
    split_trans_arg_expr(Tokens, length(Tokens)).

split_trans_arg_expr(_Tokens, 0) ->
    error;
split_trans_arg_expr(Tokens, N) ->
    Prefix = lists:sublist(Tokens, N),
    Suffix = lists:nthtail(N, Tokens),
    case is_valid_trans_arg_suffix(Suffix) of
        false ->
            split_trans_arg_expr(Tokens, N - 1);
        true ->
            case parse_expr_tokens(Prefix) of
                {ok, Expr} ->
                    {ok, Expr, Suffix};
                error ->
                    split_trans_arg_expr(Tokens, N - 1)
            end
    end.

is_valid_trans_arg_suffix([]) ->
    true;
is_valid_trans_arg_suffix([{identifier, _Pos, _Name} | _]) ->
    true;
is_valid_trans_arg_suffix(_) ->
    false.

parse_expr_tokens(Tokens) ->
    Open = {open_var, {<<"highlight-trans">>, 1, 1}, <<"{{">>},
    Close = {close_var, {<<"highlight-trans">>, 1, 1}, <<"}}">>},
    case template_compiler_parser:parse([Open | Tokens] ++ [Close]) of
        {ok, {base, [{value, _OpenVar, Expr, []}]}} ->
            {ok, Expr};
        _ ->
            error
    end.

render_lines(Content) ->
    render_lines(binary:split(Content, <<"\n">>, [global]), 1).

render_lines([], _LineNo) ->
    [];
render_lines([Line], LineNo) ->
    [render_line(LineNo, Line)];
render_lines([Line|Rest], LineNo) ->
    [render_line(LineNo, Line), <<"\n">>, render_lines(Rest, LineNo + 1)].

render_line(LineNo, Content) ->
    [
        <<"<span class=\"template-compiler-line\" style=\"">>, ?STYLE_LINE, <<"\" data-line=\"">>, integer_to_binary(LineNo), <<"\">">>,
        <<"<span class=\"template-compiler-line-number\" style=\"">>, ?STYLE_LINE_NO, <<"\">">>, integer_to_binary(LineNo), <<"</span>">>,
        Content,
        <<"</span>">>
    ].

span_lines(_Style, <<>>) ->
    [];
span_lines(Style, EscapedText) ->
    span_line_parts(binary:split(EscapedText, <<"\n">>, [global]), Style).

span_line_parts([], _Style) ->
    [];
span_line_parts([Part], Style) ->
    span_single(Style, Part);
span_line_parts([Part|Rest], Style) ->
    [span_single(Style, Part), <<"\n">>, span_line_parts(Rest, Style)].

span_single(_Style, <<>>) ->
    [];
span_single(Style, Text) ->
    [
        <<"<span style=\"">>, Style, <<"\">">>,
        Text,
        <<"</span>">>
    ].

token_text(Text) when is_binary(Text) ->
    Text;
token_text(Text) when is_list(Text) ->
    unicode:characters_to_binary(Text);
token_text(Text) when is_atom(Text) ->
    atom_to_binary(Text, utf8).

debug_points_map(DebugPoints) when is_list(DebugPoints) ->
    maps:from_keys(DebugPoints, true);
debug_points_map(DebugPoints) when is_map(DebugPoints) ->
    DebugPoints;
debug_points_map(_) ->
    #{}.

render_debug_checkbox({Filename, Line, Col} = SrcPos, DebugPointMap) ->
    case maps:is_key(SrcPos, DebugPointMap) of
        true ->
            Value = iolist_to_binary([integer_to_binary(Line), <<":">>, integer_to_binary(Col)]),
            AriaLabel = iolist_to_binary([
                <<"Toggle debug at ">>, Filename, <<" ">>,
                integer_to_binary(Line), <<":">>, integer_to_binary(Col)
            ]),
            [
                <<"<label class=\"template-compiler-debug-point\" style=\"">>, ?STYLE_CHECKBOX, <<"\">">>,
                <<"<input type=\"checkbox\" value=\"">>, escape_attr(Value),
                <<"\" aria-label=\"">>, escape_attr(AriaLabel),
                <<"\" data-template=\"">>, escape_attr(Filename),
                <<"\" data-line=\"">>, integer_to_binary(Line), <<"\" data-column=\"">>, integer_to_binary(Col), <<"\">">>,
                <<"</label>">>
            ];
        false ->
            []
    end;
render_debug_checkbox(_, _DebugPointMap) ->
    [].

element_src_pos({text, SrcPos, _Text}) ->
    SrcPos;
element_src_pos({value, OpenVar, _Expr, _With}) ->
    tag_src_pos(OpenVar);
element_src_pos({date, now, Tag, _Format}) ->
    tag_src_pos(Tag);
element_src_pos({block, {identifier, SrcPos, _Name}, _Elts}) ->
    SrcPos;
element_src_pos({inherit, Tag}) ->
    tag_src_pos(Tag);
element_src_pos({include, Tag, _Method, _Template, _Args}) ->
    tag_src_pos(Tag);
element_src_pos({catinclude, Tag, _Method, _Template, _IdExpr, _Args}) ->
    tag_src_pos(Tag);
element_src_pos({compose, {Tag, _Template, _Args}, _Blocks}) ->
    tag_src_pos(Tag);
element_src_pos({catcompose, {Tag, _Template, _IdExpr, _Args}, _Blocks}) ->
    tag_src_pos(Tag);
element_src_pos({'call', {identifier, SrcPos, _Name}, _Args}) ->
    SrcPos;
element_src_pos({call_with, {identifier, SrcPos, _Name}, _Expr}) ->
    SrcPos;
element_src_pos({custom_tag, {identifier, SrcPos, _Name}, _Args}) ->
    SrcPos;
element_src_pos({Tag, TagPos, _Expr, _Args}) when Tag =:= image; Tag =:= image_url; Tag =:= image_data_url; Tag =:= media; Tag =:= url ->
    tag_src_pos(TagPos);
element_src_pos({LibTag, TagPos, _LibList, _Args}) when LibTag =:= lib; LibTag =:= lib_url ->
    tag_src_pos(TagPos);
element_src_pos({print, TagPos, _Expr}) ->
    tag_src_pos(TagPos);
element_src_pos({'if', {'as', Tag, _Expr, _AsVar}, _IfElts, _ElseElts}) ->
    tag_src_pos(Tag);
element_src_pos({'ifequal', {Tag, _Expr1, _Expr2}, _IfElts, _ElseElts}) ->
    tag_src_pos(Tag);
element_src_pos({'ifnotequal', {Tag, _Expr1, _Expr2}, _IfElts, _ElseElts}) ->
    tag_src_pos(Tag);
element_src_pos({for, {'in', Tag, _Idents, _ListExpr}, _LoopElts, _EmptyElts}) ->
    tag_src_pos(Tag);
element_src_pos({with, {Tag, _Exprs, _Idents}, _Elts}) ->
    tag_src_pos(Tag);
element_src_pos({cache, {_CacheTime, _Args, Tag}, _Elts}) ->
    tag_src_pos(Tag);
element_src_pos({javascript, TagPos, _Elts}) ->
    tag_src_pos(TagPos);
element_src_pos({filter, {Tag, _Filters}, _Elts}) ->
    tag_src_pos(Tag);
element_src_pos({spaceless, TagPos, _Elts}) ->
    tag_src_pos(TagPos);
element_src_pos({autoescape, {identifier, SrcPos, _OnOff}, _Elts}) ->
    SrcPos;
element_src_pos({cycle, TagPos, _Exprs}) ->
    tag_src_pos(TagPos);
element_src_pos(_) ->
    undefined.

tag_src_pos({_Token, SrcPos, _Text}) when is_tuple(SrcPos), tuple_size(SrcPos) =:= 3 ->
    SrcPos;
tag_src_pos({_Token, SrcPos}) when is_tuple(SrcPos), tuple_size(SrcPos) =:= 3 ->
    SrcPos;
tag_src_pos(SrcPos) when is_tuple(SrcPos), tuple_size(SrcPos) =:= 3 ->
    SrcPos;
tag_src_pos(_) ->
    undefined.

escape_attr(Text) ->
    z_html:escape(z_convert:to_binary(Text)).
