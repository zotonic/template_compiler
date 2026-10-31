%% @doc Restricted GNU gettext plural expressions and selection of plural forms.
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
-module(template_compiler_plural).

-export([parse/1, index/2, english/0, select/3, translations/4]).

-type rule() :: {pos_integer(), term()}.
-export_type([rule/0]).

%% @doc The English source language always has two forms.
-spec english() -> rule().
english() -> {2, {'!=', n, 1}}.

%% @doc Parse the value of a Plural-Forms header once, when loading a catalog.
%% Only integer expressions over n are accepted. Limits bound catalog input size.
-spec parse(Header) -> Result when
    Header :: binary(),
    Result :: {ok, rule()} | {error, invalid_plural_forms}.
parse(Header) when is_binary(Header), byte_size(Header) =< 4096 ->
    try
        {match, [Count, Expr]} = re:run(Header,
            <<"^\\s*nplurals\\s*=\\s*([0-9]+)\\s*;\\s*plural\\s*=\\s*([^;]+);?\\s*$">>,
            [{capture, all_but_first, binary}]),
        N = binary_to_integer(Count),
        true = N > 0 andalso N =< 100,
        {Ast, []} = conditional(tokens(binary_to_list(Expr))),
        {ok, {N, Ast}}
    catch
        _:_ -> {error, invalid_plural_forms}
    end;
parse(_) ->
    {error, invalid_plural_forms}.

%% @doc Evaluate a parsed rule, checking its zero-based result against nplurals.
-spec index(Rule, N) -> Result when
    Rule :: rule(),
    N :: non_neg_integer(),
    Result :: {ok, non_neg_integer()} | {error, invalid_plural_index}.
index({Size, Ast}, N) when is_integer(N), N >= 0 ->
    try eval(Ast, N) of
        I when is_integer(I), I >= 0, I < Size -> {ok, I};
        _ -> {error, invalid_plural_index}
    catch
        _:_ -> {error, invalid_plural_index}
    end;
index(_, _) ->
    {error, invalid_plural_index}.

%% @doc Select a form after language fallback. Missing/broken forms use the
%% English source pair and its rule, never the requested language's index.
-spec select(Translation, N, Fallback) -> term() when
    Translation :: term(),
    N :: non_neg_integer(),
    Fallback :: {rule(), tuple()}.
select(_, N, _) when not is_integer(N); N < 0 ->
    error({invalid_plural_count, N});
select({Rule, Forms}, N, Fallback) when is_tuple(Forms) ->
    case index(Rule, N) of
        {ok, I} when I < tuple_size(Forms) ->
            case element(I + 1, Forms) of
                undefined -> fallback(N, Fallback);
                <<>> -> fallback(N, Fallback);
                Form -> Form
            end;
        _ -> fallback(N, Fallback)
    end;
select(_, N, Fallback) ->
    fallback(N, Fallback).

fallback(N, {Rule, Forms}) ->
    {ok, I} = index(Rule, N),
    element(I + 1, Forms).

%% @doc Optional runtime callback; old runtimes can render the English sources.
-spec translations(Runtime, Singular, Plural, Context) -> {trans, list()} when
    Runtime :: module(),
    Singular :: binary(),
    Plural :: binary(),
    Context :: term().
translations(Runtime, Singular, Plural, Context) ->
    {module, Runtime} = code:ensure_loaded(Runtime),
    {trans, Tr} = case erlang:function_exported(Runtime, get_plural_translations, 3) of
        true -> Runtime:get_plural_translations(Singular, Plural, Context);
        false -> {trans, []}
    end,
    case proplists:is_defined(en, Tr) of
        true -> {trans, Tr};
        false -> {trans, [{en, {english(), {Singular, Plural}}}|Tr]}
    end.

tokens([]) -> [];
tokens([C|Rest]) when C =< 32 -> tokens(Rest);
tokens([C|_] = Input) when C >= $0, C =< $9 ->
    {Digits, Rest} = lists:splitwith(fun(D) -> D >= $0 andalso D =< $9 end, Input),
    [list_to_integer(Digits)|tokens(Rest)];
tokens([$n|Rest]) -> [n|tokens(Rest)];
tokens([$&, $&|Rest]) -> ['&&'|tokens(Rest)];
tokens([$|, $||Rest]) -> ['||'|tokens(Rest)];
tokens([$=, $=|Rest]) -> ['=='|tokens(Rest)];
tokens([$!, $=|Rest]) -> ['!='|tokens(Rest)];
tokens([$<, $=|Rest]) -> ['<='|tokens(Rest)];
tokens([$>, $=|Rest]) -> ['>='|tokens(Rest)];
tokens([C|Rest]) when C =:= $(; C =:= $); C =:= $?; C =:= $:;
        C =:= $+; C =:= $-; C =:= $*; C =:= $/; C =:= $%;
        C =:= $!; C =:= $<; C =:= $> ->
    [{char, C}|tokens(Rest)].

conditional(Tokens) ->
    case binary_expr(Tokens, 1) of
        {Condition, [{char, $?}|Rest]} ->
            {Yes, [{char, $:}|Rest1]} = conditional(Rest),
            {No, Rest2} = conditional(Rest1),
            {{conditional, Condition, Yes, No}, Rest2};
        Result -> Result
    end.

binary_expr(Tokens, Min) ->
    {Left, Rest} = unary(Tokens),
    binary_rest(Left, Rest, Min).

binary_rest(Left, [Op|Rest] = Tokens, Min) ->
    case precedence(Op) of
        P when P >= Min ->
            {Right, Rest1} = binary_expr(Rest, P + 1),
            binary_rest({Op, Left, Right}, Rest1, Min);
        _ -> {Left, Tokens}
    end;
binary_rest(Left, [], _) -> {Left, []}.

precedence('||') -> 1;
precedence('&&') -> 2;
precedence('==') -> 3;
precedence('!=') -> 3;
precedence({char, $<}) -> 4;
precedence({char, $>}) -> 4;
precedence('<=') -> 4;
precedence('>=') -> 4;
precedence({char, $+}) -> 5;
precedence({char, $-}) -> 5;
precedence({char, $*}) -> 6;
precedence({char, $/}) -> 6;
precedence({char, $%}) -> 6;
precedence(_) -> 0.

unary([{char, C}|Rest]) when C =:= $!; C =:= $+; C =:= $- ->
    {Expr, Rest1} = unary(Rest),
    {{unary, C, Expr}, Rest1};
unary([{char, $(}|Rest]) ->
    {Expr, [{char, $)}|Rest1]} = conditional(Rest),
    {Expr, Rest1};
unary([n|Rest]) -> {n, Rest};
unary([N|Rest]) when is_integer(N) -> {N, Rest}.

eval(n, N) -> N;
eval(I, _) when is_integer(I) -> I;
eval({unary, $!, E}, N) -> bool(eval(E, N) =:= 0);
eval({unary, $+, E}, N) -> eval(E, N);
eval({unary, $-, E}, N) -> -eval(E, N);
eval({conditional, C, Yes, No}, N) ->
    case eval(C, N) of 0 -> eval(No, N); _ -> eval(Yes, N) end;
eval({'&&', A, B}, N) ->
    bool(eval(A, N) =/= 0 andalso eval(B, N) =/= 0);
eval({'||', A, B}, N) ->
    bool(eval(A, N) =/= 0 orelse eval(B, N) =/= 0);
eval({Op, A, B}, N) -> op(Op, eval(A, N), eval(B, N)).

op('==', A, B) -> bool(A =:= B);
op('!=', A, B) -> bool(A =/= B);
op({char, $<}, A, B) -> bool(A < B);
op({char, $>}, A, B) -> bool(A > B);
op('<=', A, B) -> bool(A =< B);
op('>=', A, B) -> bool(A >= B);
op({char, $+}, A, B) -> A + B;
op({char, $-}, A, B) -> A - B;
op({char, $*}, A, B) -> A * B;
op({char, $/}, A, B) -> A div B;
op({char, $%}, A, B) -> A rem B.

bool(true) -> 1;
bool(false) -> 0.

