-module(template_compiler_plural_tests).
-include_lib("eunit/include/eunit.hrl").

evaluation_test() ->
    BadDivision = {{char, $/}, 1, 0},
    Cases = [
        {{conditional, n, 2, BadDivision}, 1, 2},
        {{conditional, n, BadDivision, 2}, 0, 2},
        {{'&&', n, BadDivision}, 0, 0},
        {{'||', n, BadDivision}, 1, 1},
        {{{char, $+}, 1, {{char, $*}, 2, 3}}, 0, 7},
        {{{char, $/}, 8, 3}, 0, 2},
        {{{char, $%}, 8, 3}, 0, 2},
        {{{char, $-}, {{char, $-}, 8, 3}, 2}, 0, 3},
        {{unary, $!, {'==', n, 1}}, 2, 1},
        {{{char, $+}, {unary, $-, {unary, $-, n}}, {unary, $+, 1}}, 2, 3},
        {{conditional, 0, 1, {conditional, 0, 2, 3}}, 0, 3}
    ],
    lists:foreach(fun({Ast, N, Expected}) ->
        ?assertEqual({ok, Expected}, template_compiler_plural:index({10, Ast}, N))
    end, Cases),
    ?assertEqual({ok, 0}, template_compiler_plural:index(template_compiler_plural:english(), 1)),
    ?assertEqual({ok, 1}, template_compiler_plural:index(template_compiler_plural:english(), 0)).

invalid_result_test() ->
    lists:foreach(fun(Ast) ->
        ?assertEqual({error, invalid_plural_index}, template_compiler_plural:index({2, Ast}, 5))
    end, [-1, 2, {{char, $/}, 1, 0}, invalid_ast]),
    ?assertEqual({error, invalid_plural_index}, template_compiler_plural:index({2, 0}, -1)),
    ?assertEqual({error, invalid_plural_index}, template_compiler_plural:index({2, 0}, <<"1">>)).

fallback_test() ->
    English = {template_compiler_plural:english(), {<<"one">>, <<"many">>}},
    Arabic = {6, {conditional, {'==', n, 1}, 1, 5}},
    ?assertEqual(<<"one">>, template_compiler_plural:select({Arabic, {<<"zero">>}}, 1, English)),
    ?assertEqual(<<"many">>, template_compiler_plural:select({Arabic, {<<"zero">>}}, 100, English)),
    ?assertEqual(<<"many">>, template_compiler_plural:select({element(1, English), {<<"een">>, <<>>}}, 2, English)).
