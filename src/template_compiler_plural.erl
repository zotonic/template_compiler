%% @doc Evaluate plural-rule ASTs and select precompiled translation forms.
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

-export([index/2, english/0, select/3, resolve/3, translations/4, translations/5]).

%% Catalog loaders construct this restricted, context-free expression tree.
%% Boolean results are integers (0 or 1); division is integer division.
-type binary_operator() :: '==' | '!=' | '<=' | '>=' | '&&' | '||'
    | {char, $< | $> | $+ | $- | $* | $/ | $%}.
-type expression() :: integer() | n
    | {unary, $! | $+ | $-, expression()}
    | {conditional, expression(), expression(), expression()}
    | {binary_operator(), expression(), expression()}.
-type rule() :: {pos_integer(), expression()}.
-export_type([rule/0, expression/0]).

%% @doc The English source language always has two forms.
-spec english() -> rule().
english() -> {2, {'!=', n, 1}}.

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
select({plural_candidates, Candidates}, N, Fallback) ->
    case choose(Candidates, N) of
        {ok, Form} -> Form;
        error -> fallback(N, Fallback)
    end;
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

%% @doc Resolve each language's contextual/contextless candidates independently.
%% Untranslated forms are excluded before the runtime chooses a language, so
%% a missing form still permits the next fallback language to be selected.
-spec resolve({trans, list()}, non_neg_integer(), {rule(), tuple()}) -> {trans, list()}.
resolve(_, N, _) when not is_integer(N); N < 0 ->
    error({invalid_plural_count, N});
resolve({trans, Tr}, N, Fallback) ->
    Forms = [{Lang, Form} || {Lang, Value} <- Tr, {ok, Form} <- [choose([Value], N)]],
    case proplists:is_defined(en, Forms) of
        true -> {trans, Forms};
        false -> {trans, [{en, fallback(N, Fallback)}|Forms]}
    end.

choose([], _) -> error;
choose([{plural_candidates, Candidates}|Rest], N) ->
    case choose(Candidates, N) of
        error -> choose(Rest, N);
        Found -> Found
    end;
choose([{Rule, Forms}|Rest], N) when is_tuple(Forms) ->
    case index(Rule, N) of
        {ok, I} when I < tuple_size(Forms) ->
            case element(I + 1, Forms) of
                undefined -> choose(Rest, N);
                <<>> -> choose(Rest, N);
                Form -> {ok, Form}
            end;
        _ -> choose(Rest, N)
    end;
choose([_|Rest], N) -> choose(Rest, N).

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

%% @doc Optional contextual runtime lookup, falling back for older runtimes.
-spec translations(module(), binary(), binary(), binary(), term()) -> {trans, list()}.
translations(Runtime, Singular, Plural, MsgCtxt, Context) ->
    {module, Runtime} = code:ensure_loaded(Runtime),
    case erlang:function_exported(Runtime, get_plural_translations, 4) of
        true ->
            {trans, Tr} = Runtime:get_plural_translations(Singular, Plural, MsgCtxt, Context),
            case proplists:is_defined(en, Tr) of
                true -> {trans, Tr};
                false -> {trans, [{en, {english(), {Singular, Plural}}}|Tr]}
            end;
        false -> translations(Runtime, Singular, Plural, Context)
    end.

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

