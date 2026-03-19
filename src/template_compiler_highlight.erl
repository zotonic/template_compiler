%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Generate syntax highlighted HTML from template source plus parser annotations.
%%      The highlighter scans and parses the template, derives source-position
%%      annotations from the tokens and parsed constructs, adds debug-point
%%      annotations, and then merges those annotations back into the original
%%      template text to emit spans, checkboxes, and line numbers without
%%      reconstructing the source.
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
        {ok, RawTokens} ->
            Tokens = normalize_tokens(RawTokens),
            case ensure_parse_tree(Tokens) of
                {ok, _Tree} ->
                    SourceIndex = source_index(Bin),
                    Annotations = annotations(Bin, RawTokens, Tokens, SourceIndex),
                    {ok, iolist_to_binary(render_document(Bin, SourceIndex, Annotations, debug_points_map(DebugPoints)))};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

ensure_parse_tree(Tokens) ->
    case template_compiler_parser:parse(Tokens) of
        {ok, _} = Ok ->
            Ok;
        {error, _} = Error ->
            case maybe_parse_trans_tag(Tokens) of
                {ok, _} = Ok ->
                    Ok;
                error ->
                    Error
            end
    end.

annotations(Bin, RawTokens, _Tokens, SourceIndex) ->
    {TokenAnnotations, _Cursor} = token_annotations(Bin, RawTokens, SourceIndex, 0, []),
    split_annotations(TokenAnnotations, SourceIndex).

token_annotations(_Bin, [], _SourceIndex, Cursor, Acc) ->
    {lists:reverse(Acc), Cursor};
token_annotations(Bin, [Token | Rest], SourceIndex, Cursor, Acc) ->
    {Annotations, NextCursor} = token_annotation(Token, Bin, SourceIndex, Cursor),
    token_annotations(Bin, Rest, SourceIndex, NextCursor, lists:reverse(Annotations, Acc)).

token_annotation({string_literal, _SrcPos, _Text}, Bin, _SourceIndex, Cursor) ->
    case locate_quoted_span(Bin, Cursor) of
        {Start, Stop} ->
            {[#{start => Start, stop => Stop, kind => string}], Stop};
        error ->
            {[], Cursor}
    end;
token_annotation({trans_literal, _SrcPos, _Text}, Bin, _SourceIndex, Cursor) ->
    case locate_trans_literal_span(Bin, Cursor) of
        {Start, Stop} ->
            {trans_literal_annnotations(Start, Stop), Stop};
        error ->
            {[], Cursor}
    end;
token_annotation({atom_literal, _SrcPos, _Text}, Bin, _SourceIndex, Cursor) ->
    case locate_quoted_span(Bin, Cursor) of
        {Start, Stop} ->
            {[#{start => Start, stop => Stop, kind => literal}], Stop};
        error ->
            {[], Cursor}
    end;
token_annotation({identifier, _SrcPos, Text}, Bin, _SourceIndex, Cursor) ->
    Kind = case Text of
        <<"true">> -> literal;
        <<"false">> -> literal;
        <<"undefined">> -> literal;
        _ -> ident
    end,
    locate_annotation(Bin, Cursor, Text, Kind);
token_annotation({Token, _SrcPos, Text}, Bin, _SourceIndex, Cursor) when is_atom(Token) ->
    case classify_token(Token) of
        undefined ->
            {[], Cursor};
        {Kind, token_text} ->
            locate_annotation(Bin, Cursor, token_text(Token, Text), Kind);
        {Kind, token_length, _Length} ->
            locate_annotation(Bin, Cursor, token_text(Token, Text), Kind)
    end.

classify_token(Token) ->
    case is_keyword_token(Token) of
        true ->
            {keyword, token_text};
        false ->
            case Token of
                open_map -> {operator, token_length, 2};
                colons -> {operator, token_length, 2};
                equal -> {operator, token_text};
                colon -> {operator, token_text};
                pipe -> {operator, token_text};
                comma -> {operator, token_text};
                dot -> {operator, token_text};
                hash -> {operator, token_text};
                open_bracket -> {operator, token_text};
                close_bracket -> {operator, token_text};
                open_curly -> {operator, token_text};
                close_curly -> {operator, token_text};
                '(' -> {operator, token_text};
                ')' -> {operator, token_text};
                '+' -> {operator, token_text};
                '-' -> {operator, token_text};
                '*' -> {operator, token_text};
                '/' -> {operator, token_text};
                '%' -> {operator, token_text};
                '<' -> {operator, token_text};
                '>' -> {operator, token_text};
                '=<'
                    -> {operator, token_text};
                '>='
                    -> {operator, token_text};
                '=='
                    -> {operator, token_text};
                '/='
                    -> {operator, token_text};
                '=/='
                    -> {operator, token_text};
                '=:='
                    -> {operator, token_text};
                '++'
                    -> {operator, token_text};
                '--'
                    -> {operator, token_text};
                _ ->
                    undefined
            end
    end.

is_keyword_token(Token) ->
    case atom_to_binary(Token, utf8) of
        <<_/binary>> = Name ->
            binary:longest_common_suffix([Name, <<"_keyword">>]) =:= 8
    end.

trans_literal_annnotations(Start, Stop) when Stop > Start + 1 ->
    [
        #{start => Start, stop => Start + 1, kind => operator},
        #{start => Start + 1, stop => Stop, kind => string}
    ];
trans_literal_annnotations(_, _) ->
    [].

quoted_span(Bin, Start) ->
    case byte_at(Bin, Start) of
        $" ->
            find_quote_end(Bin, Start + 1, $");
        $' ->
            find_quote_end(Bin, Start + 1, $');
        $` ->
            find_quote_end(Bin, Start + 1, $`);
        _ ->
            error
    end.

find_quote_end(Bin, Pos, _Quote) when Pos >= byte_size(Bin) ->
    {ok, byte_size(Bin)};
find_quote_end(Bin, Pos, Quote) ->
    case utf8_at(Bin, Pos) of
        {$\\, NextPos} ->
            find_quote_end(Bin, next_utf8_offset(Bin, NextPos), Quote);
        {Quote, NextPos} ->
            {ok, NextPos};
        {_Char, NextPos} ->
            find_quote_end(Bin, NextPos, Quote);
        error ->
            {ok, byte_size(Bin)}
    end.

split_annotations(Annotations, SourceIndex) ->
    lists:flatmap(
        fun(Ann) -> split_annotation(Ann, SourceIndex) end,
        Annotations).

split_annotation(#{start := Start, stop := Stop, kind := Kind}, #{lines := Lines, starts := Starts}) ->
    lists:foldl(
        fun({Line, LineStart}, Acc) ->
            LineEnd = LineStart + byte_size(Line),
            PieceStart = erlang:max(Start, LineStart),
            PieceEnd = erlang:min(Stop, LineEnd),
            case PieceStart < PieceEnd of
                true -> [#{start => PieceStart, stop => PieceEnd, kind => Kind} | Acc];
                false -> Acc
            end
        end,
        [],
        lists:zip(Lines, Starts)).

render_document(Bin, SourceIndex, Annotations, DebugPointMap) ->
    Events = build_events(Annotations, DebugPointMap, SourceIndex),
    [
        <<"<pre class=\"template-compiler-highlight\" style=\"">>, ?STYLE_PRE, <<"\"><code>">>,
        render_lines(Bin, SourceIndex, Events, 1),
        <<"</code></pre>">>
    ].

build_events(Annotations, DebugPointMap, SourceIndex) ->
    {OpenMap, CloseMap} = lists:foldl(
        fun(#{start := Start, stop := Stop, kind := Kind}, Acc) ->
            {OpenAcc, CloseAcc} = Acc,
            {
                map_prepend(Start, span_open(Kind), OpenAcc),
                map_prepend(Stop, <<"</span>">>, CloseAcc)
            }
        end,
        {#{}, #{}},
        Annotations),
    DebugEvents = build_debug_events(DebugPointMap, SourceIndex),
    #{
        open => OpenMap,
        close => CloseMap,
        checkbox => DebugEvents
    }.

build_debug_events(DebugPointMap, SourceIndex) when is_map(DebugPointMap) ->
    maps:fold(
        fun
            ({_Filename, _Line, _Col} = SrcPos, _Value, Acc) ->
                case safe_srcpos_to_offset(SourceIndex, SrcPos) of
                    {ok, Offset} ->
                        map_prepend(Offset, debug_checkbox(SrcPos), Acc);
                    error ->
                        Acc
                end;
            (_Key, _Value, Acc) ->
                Acc
        end,
        #{},
        DebugPointMap).

span_open(text) ->
    span_open_style(?STYLE_TEXT);
span_open(delim) ->
    span_open_style(?STYLE_DELIM);
span_open(keyword) ->
    span_open_style(?STYLE_KEYWORD);
span_open(string) ->
    span_open_style(?STYLE_STRING);
span_open(ident) ->
    span_open_style(?STYLE_IDENT);
span_open(literal) ->
    span_open_style(?STYLE_LITERAL);
span_open(operator) ->
    span_open_style(?STYLE_OPERATOR);
span_open(comment) ->
    span_open_style(?STYLE_COMMENT).

span_open_style(Style) ->
    [<<"<span style=\"">>, Style, <<"\">">>].

debug_checkbox({Filename, Line, Col}) ->
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
        <<"\" data-line=\"">>, integer_to_binary(Line),
        <<"\" data-column=\"">>, integer_to_binary(Col), <<"\">">>,
        <<"</label>">>
    ].

render_lines(_Bin, #{lines := [], starts := []}, _Events, _LineNo) ->
    [];
render_lines(Bin, #{lines := [Line], starts := [Start]}, Events, LineNo) ->
    render_line(Bin, LineNo, Start, Line, Events);
render_lines(Bin, #{lines := [Line | RestLines], starts := [Start | RestStarts]} = SourceIndex, Events, LineNo) ->
    [
        render_line(Bin, LineNo, Start, Line, Events),
        <<"\n">>,
        render_lines(Bin, SourceIndex#{lines := RestLines, starts := RestStarts}, Events, LineNo + 1)
    ].

render_line(_Bin, LineNo, Start, Line, Events) ->
    End = Start + byte_size(Line),
    [
        <<"<span class=\"template-compiler-line\" style=\"">>, ?STYLE_LINE, <<"\" data-line=\"">>, integer_to_binary(LineNo), <<"\">">>,
        <<"<span class=\"template-compiler-line-number\" style=\"">>, ?STYLE_LINE_NO, <<"\">">>, integer_to_binary(LineNo), <<"</span>">>,
        render_segment(Line, Start, End, Start, relevant_positions(Start, End, Events), Events),
        <<"</span>">>
    ].

render_segment(Line, _Start, End, Cursor, [], _Events) ->
    escape_text(binary:part(Line, {Cursor - (End - byte_size(Line)), End - Cursor}));
render_segment(Line, Start, End, Cursor, [Pos | Rest], Events) ->
    [
        escape_text(binary:part(Line, {Cursor - Start, Pos - Cursor})),
        emit_at(Pos, Events),
        render_segment(Line, Start, End, Pos, Rest, Events)
    ].

emit_at(Pos, #{open := OpenMap, close := CloseMap, checkbox := CheckboxMap}) ->
    [
        lists:reverse(maps:get(Pos, CloseMap, [])),
        lists:reverse(maps:get(Pos, CheckboxMap, [])),
        lists:reverse(maps:get(Pos, OpenMap, []))
    ].

relevant_positions(Start, End, #{open := OpenMap, close := CloseMap, checkbox := CheckboxMap}) ->
    lists:usort(
        [ Pos || Pos <- maps:keys(OpenMap), Start =< Pos, Pos =< End ] ++
        [ Pos || Pos <- maps:keys(CloseMap), Start =< Pos, Pos =< End ] ++
        [ Pos || Pos <- maps:keys(CheckboxMap), Start =< Pos, Pos =< End ]).

map_prepend(Key, Value, Map) ->
    Map#{ Key => [Value | maps:get(Key, Map, [])] }.

source_index(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global]),
    Starts = source_line_starts(Lines, 0, []),
    #{
        lines => Lines,
        starts => Starts
    }.

source_line_starts([], _Offset, Acc) ->
    lists:reverse(Acc);
source_line_starts([Line | Rest], Offset, Acc) ->
    NextOffset = Offset + byte_size(Line) + 1,
    source_line_starts(Rest, NextOffset, [Offset | Acc]).

srcpos_to_offset(#{lines := Lines, starts := Starts}, {_Filename, LineNo, Column}) ->
    Line = lists:nth(LineNo, Lines),
    Start = lists:nth(LineNo, Starts),
    Start + utf8_prefix_bytes(Line, Column - 1).

safe_srcpos_to_offset(SourceIndex, {_Filename, LineNo, Column} = SrcPos) when LineNo >= 1, Column >= 1 ->
    try
        {ok, srcpos_to_offset(SourceIndex, SrcPos)}
    catch
        _:_ -> error
    end;
safe_srcpos_to_offset(_SourceIndex, _SrcPos) ->
    error.

utf8_prefix_bytes(_Bin, 0) ->
    0;
utf8_prefix_bytes(Bin, Count) when Count > 0 ->
    utf8_prefix_bytes(Bin, Count, 0).

utf8_prefix_bytes(_Bin, 0, Bytes) ->
    Bytes;
utf8_prefix_bytes(<<>>, _Count, Bytes) ->
    Bytes;
utf8_prefix_bytes(Bin, Count, Bytes) ->
    <<Char/utf8, Rest/binary>> = Bin,
    utf8_prefix_bytes(Rest, Count - 1, Bytes + byte_size(<<Char/utf8>>)).

byte_at(Bin, Offset) when Offset >= 0, Offset < byte_size(Bin) ->
    binary:at(Bin, Offset);
byte_at(_Bin, _Offset) ->
    undefined.

escape_text(Text) when is_binary(Text) ->
    z_html:escape(Text).

escape_attr(Text) ->
    z_html:escape(z_convert:to_binary(Text)).

debug_points_map(DebugPoints) when is_list(DebugPoints) ->
    maps:from_keys(DebugPoints, true);
debug_points_map(DebugPoints) when is_map(DebugPoints) ->
    DebugPoints;
debug_points_map(_) ->
    #{}.

locate_annotation(Bin, Cursor, Text, Kind) ->
    case locate_text_span(Bin, Cursor, Text) of
        {Start, Stop} ->
            {[#{start => Start, stop => Stop, kind => Kind}], Stop};
        error ->
            {[], Cursor}
    end.

locate_text_span(_Bin, Cursor, <<>>) ->
    {Cursor, Cursor};
locate_text_span(Bin, Cursor, Text) when is_binary(Text) ->
    locate_text_span_1(Bin, Cursor, Text).

locate_text_span_1(Bin, Cursor, Text) when Cursor =< byte_size(Bin) ->
    case has_prefix(Bin, Cursor, Text) of
        true ->
            {Cursor, Cursor + byte_size(Text)};
        false when Cursor < byte_size(Bin) ->
            locate_text_span_1(Bin, next_utf8_offset(Bin, Cursor), Text);
        false ->
            error
    end.

locate_quoted_span(Bin, Cursor) ->
    case find_next_quote_start(Bin, Cursor) of
        {ok, Start} ->
            case quoted_span(Bin, Start) of
                {ok, Stop} ->
                    {Start, Stop};
                error ->
                    error
            end;
        error ->
            error
    end.

find_next_quote_start(Bin, Cursor) when Cursor < byte_size(Bin) ->
    find_next_quote_start_1(Bin, Cursor);
find_next_quote_start(_Bin, _Cursor) ->
    error.

find_next_quote_start_1(Bin, Cursor) when Cursor < byte_size(Bin) ->
    case utf8_at(Bin, Cursor) of
        {$", _NextPos} ->
            {ok, Cursor};
        {$', _NextPos} ->
            {ok, Cursor};
        {$`, _NextPos} ->
            {ok, Cursor};
        {_Char, NextPos} ->
            find_next_quote_start_1(Bin, NextPos);
        error ->
            error
    end;
find_next_quote_start_1(_Bin, _Cursor) ->
    error.

locate_trans_literal_span(Bin, Cursor) ->
    case find_next_codepoint(Bin, Cursor, $_) of
        {ok, Start} ->
            case quoted_span(Bin, Start + 1) of
                {ok, Stop} when Stop > Start + 1 ->
                    {Start, Stop};
                _ ->
                    locate_quoted_span(Bin, Cursor)
            end;
        error ->
            locate_quoted_span(Bin, Cursor)
    end.

find_next_codepoint(Bin, Cursor, Codepoint) when Cursor < byte_size(Bin) ->
    find_next_codepoint_1(Bin, Cursor, Codepoint);
find_next_codepoint(_Bin, _Cursor, _Codepoint) ->
    error.

find_next_codepoint_1(Bin, Cursor, Codepoint) when Cursor < byte_size(Bin) ->
    case utf8_at(Bin, Cursor) of
        {Codepoint, _NextPos} ->
            {ok, Cursor};
        {_Char, NextPos} ->
            find_next_codepoint_1(Bin, NextPos, Codepoint);
        error ->
            error
    end;
find_next_codepoint_1(_Bin, _Cursor, _Codepoint) ->
    error.

has_prefix(Bin, Offset, Prefix) ->
    Size = byte_size(Prefix),
    Offset + Size =< byte_size(Bin)
        andalso binary:part(Bin, {Offset, Size}) =:= Prefix.

next_utf8_offset(Bin, Offset) when Offset < byte_size(Bin) ->
    case utf8_at(Bin, Offset) of
        {_Char, NextPos} ->
            NextPos;
        error ->
            byte_size(Bin)
    end;
next_utf8_offset(_Bin, Offset) ->
    Offset.

utf8_at(Bin, Offset) when Offset >= 0, Offset < byte_size(Bin) ->
    case binary:part(Bin, {Offset, byte_size(Bin) - Offset}) of
        <<Char/utf8, _/binary>> = Rest ->
            CharBytes = byte_size(<<Char/utf8>>),
            _ = Rest,
            {Char, Offset + CharBytes};
        _ ->
            error
    end;
utf8_at(_Bin, _Offset) ->
    error.

token_text(open_map, _Text) ->
    <<"#{">>;
token_text(colons, _Text) ->
    <<"::">>;
token_text(_Token, Text) ->
    Text.

normalize_tokens(Tokens) ->
    normalize_tokens(Tokens, []).

normalize_tokens([], Acc) ->
    lists:reverse(Acc);
normalize_tokens([{trans_keyword, _, _} = Trans, {string_literal, SrcPos, Text} | Ts], Acc) ->
    NormalizedText = unescape_trim(Text),
    Acc1 = [{trans_literal, SrcPos, {trans, [{en, NormalizedText}]}}, Trans | Acc],
    normalize_tokens(Ts, Acc1);
normalize_tokens([{trans_text, SrcPos, Text} | Ts], Acc) ->
    Acc1 = [{trans_text, SrcPos, unescape_trim(Text)} | Acc],
    normalize_tokens(Ts, Acc1);
normalize_tokens([{trans_literal, SrcPos, Text} | Ts], Acc) ->
    NormalizedText = unescape_trim(Text),
    Acc1 = [{trans_literal, SrcPos, {trans, [{en, NormalizedText}]}} | Acc],
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
