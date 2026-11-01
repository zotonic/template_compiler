%% @doc Apply literal gettext message contexts before translation expansion.
%% Declarations are lexical: separately compiled templates never inherit them.
%% @copyright 2026 Marc Worrell
%% @end
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
-module(template_compiler_msgctxt).

-export([apply/1, apply/2, default/3, translations/4]).

%% @doc Annotate normalized translation tokens for both compilation and extraction.
-spec apply(list()) -> list().
apply(Tokens) ->
    ?MODULE:apply(Tokens, undefined).

%% @doc A template declaration overrides the default supplied by its owner.
-spec apply(list(), binary() | undefined) -> list().
apply(Tokens, OwnerDefault) when is_binary(OwnerDefault); OwnerDefault =:= undefined ->
    {Declared, Body} = header(drop_comments(Tokens), undefined, []),
    Default = case Declared of undefined -> OwnerDefault; _ -> Declared end,
    annotate(Body, Default, [], []).

%% @doc Ask the runtime for this file's default, never the caller's default.
-spec default(module(), file:filename_all(), term()) -> binary() | undefined.
default(Runtime, Filename, Context) ->
    {module, Runtime} = code:ensure_loaded(Runtime),
    case erlang:function_exported(Runtime, get_template_msgctxt, 2) of
        true -> Runtime:get_template_msgctxt(Filename, Context);
        false -> undefined
    end.

%% @doc Old runtimes use the contextless catalog; new callbacks supply the
%% contextual/contextless candidates for each language, including English.
-spec translations(module(), binary(), binary(), term()) -> {trans, list()}.
translations(Runtime, Text, MsgCtxt, Context) ->
    {module, Runtime} = code:ensure_loaded(Runtime),
    Result = case erlang:function_exported(Runtime, get_translations, 3) of
        true -> Runtime:get_translations(Text, MsgCtxt, Context);
        false -> Runtime:get_translations(Text, Context)
    end,
    case Result of
        {trans, Tr} ->
            case proplists:is_defined(en, Tr) of
                true -> Result;
                false -> {trans, [{en, Text}|Tr]}
            end;
        B when is_binary(B) -> {trans, [{en, B}]}
    end.

%% Ignore declarations and literals in comment blocks, including nested blocks.
drop_comments([{open_tag, _, _}, {comment_keyword, Pos, _}, {close_tag, _, _}|Rest]) ->
    drop_comments(skip_comment(Rest, 1, Pos));
drop_comments([T|Rest]) -> [T|drop_comments(Rest)];
drop_comments([]) -> [].

skip_comment([{open_tag, _, _}, {comment_keyword, _, _}, {close_tag, _, _}|Rest], Depth, Pos) ->
    skip_comment(Rest, Depth + 1, Pos);
skip_comment([{open_tag, _, _}, {endcomment_keyword, _, _}, {close_tag, _, _}|Rest], 1, _) -> Rest;
skip_comment([{open_tag, _, _}, {endcomment_keyword, _, _}, {close_tag, _, _}|Rest], Depth, Pos) ->
    skip_comment(Rest, Depth - 1, Pos);
skip_comment([_|Rest], Depth, Pos) -> skip_comment(Rest, Depth, Pos);
skip_comment([], _, Pos) -> fail(unclosed_comment, Pos).

%% One file default, declared before output/blocks (extends/overrules may precede it).
header([{text, _, Text}=T|Rest]=Tokens, Default, Acc) ->
    case z_string:trim(Text) of
        <<>> -> header(Rest, Default, [T|Acc]);
        _ -> {Default, lists:reverse(Acc, Tokens)}
    end;
header([{open_tag, _, _}, {identifier, Pos, <<"msgctxt">>}|Rest], Default, Acc) ->
    {Args, Tail} = tag_tail(Rest, Pos),
    case Default of
        undefined -> header(Tail, literal(Args, Pos), Acc);
        _ -> fail(duplicate_msgctxt, Pos)
    end;
header([{open_tag, _, _}=Open, {Kind, Pos, _}=Tag|Rest], Default, Acc)
        when Kind =:= extends_keyword; Kind =:= overrules_keyword ->
    {Args, Tail} = tag_tail(Rest, Pos),
    header(Tail, Default, lists:reverse([Open, Tag|Args], Acc));
header(Tokens, Default, Acc) -> {Default, lists:reverse(Acc, Tokens)}.

annotate([], _, [], Acc) -> lists:reverse(Acc);
annotate([], _, [{_, Pos}|_], _) -> fail(unclosed_with_msgctxt, Pos);
annotate([{open_tag, _, _}, {identifier, Pos, <<"msgctxt">>}|_], _, _, _) ->
    fail(msgctxt_must_be_template_header, Pos);
annotate([{open_tag, _, _}, {identifier, Pos, <<"with_msgctxt">>}|Rest], Current, Stack, Acc) ->
    {Args, Tail} = tag_tail(Rest, Pos),
    annotate(Tail, literal(Args, Pos), [{Current, Pos}|Stack], Acc);
annotate([{open_tag, _, _}, {identifier, Pos, <<"endwith_msgctxt">>}|Rest], _, Stack, Acc) ->
    case {tag_tail(Rest, Pos), Stack} of
        {{[{close_tag, _, _}], Tail}, [{Previous, _}|Parents]} ->
            annotate(Tail, Previous, Parents, Acc);
        _ -> fail(unexpected_endwith_msgctxt, Pos)
    end;
annotate([{open_tag, _, _}=Open, {trans_keyword, Pos, _}=Tag,
        {trans_literal, Src, Source}|Rest], Current, Stack, Acc) ->
    {Args, Tail} = tag_tail(Rest, Pos),
    {Override, Args1} = explicit(Args, Pos),
    Effective = case Override of undefined -> Current; _ -> Override end,
    % An explicit context applies to the message, not translated argument values.
    Tokens = [Open, Tag, contextual({trans_literal, Src, Source}, Effective)
        |[contextual(T, Current) || T <- Args1]],
    annotate(Tail, Current, Stack, lists:reverse(Tokens, Acc));
annotate([T|Rest], Current, Stack, Acc) ->
    annotate(Rest, Current, Stack, [contextual(T, Current)|Acc]).

contextual({Kind, Pos, Text}, MsgCtxt)
        when MsgCtxt =/= undefined, (Kind =:= trans_text orelse Kind =:= trans_literal) ->
    {Kind, Pos, {context, MsgCtxt, Text}};
contextual(T, _) -> T.

%% Use the expression parser to distinguish a tag argument from a variable,
%% property, filter argument, or nested tuple field also named msgctxt.
explicit(Tokens, Pos) ->
    Tag = [{open_tag, Pos, <<"{%">>}, {trans_keyword, Pos, <<"trans">>},
        {trans_literal, Pos, <<>>}|Tokens],
    case template_compiler_parser:parse(Tag) of
        {ok, {base, [{trans_ext, _, Args}]}} ->
            Contexts = [{KeyPos, Expr} || {{identifier, KeyPos, <<"msgctxt">>}, Expr} <- Args],
            case Contexts of
                [] -> {undefined, Tokens};
                [{KeyPos, {string_literal, _, Value}}] ->
                    {Value, remove_explicit(Tokens, KeyPos)};
                [{KeyPos, _}] -> fail(msgctxt_requires_literal, KeyPos);
                _ -> fail(duplicate_msgctxt, Pos)
            end;
        {error, _} -> fail(invalid_trans_arguments, Pos)
    end.

remove_explicit([{identifier, Pos, <<"msgctxt">>}, {equal, _, _}, {string_literal, _, _}|Rest], Pos) -> Rest;
remove_explicit([T|Rest], Pos) -> [T|remove_explicit(Rest, Pos)].

literal([{string_literal, _, Text}, {close_tag, _, _}], _) -> Text;
literal(_, Pos) -> fail(msgctxt_requires_literal, Pos).

tag_tail(Tokens, Pos) -> tag_tail(Tokens, Pos, []).
tag_tail([{close_tag, _, _}=Close|Rest], _, Acc) -> {lists:reverse([Close|Acc]), Rest};
tag_tail([T|Rest], Pos, Acc) -> tag_tail(Rest, Pos, [T|Acc]);
tag_tail([], Pos, _) -> fail(unclosed_msgctxt_tag, Pos).

fail(Reason, Pos) -> throw({error, {Reason, Pos}}).
