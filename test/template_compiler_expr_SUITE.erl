-module(template_compiler_expr_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).


suite() ->
    [
        {timetrap, {seconds, 30}}
    ].

all() ->
    [
        {group, basic}
    ].

groups() ->
    [{basic, [],
        [expr_test
        ,expr_op_test
        ,expr_op_eq_neq_test
        ,expr_filter
        ,expr_literals
        ,expr_nested
        ,expr_autoid
        ,expr_map
        ,expr_model_call
        ,expr_orelse
        ,expr_andalso
        ]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(template_compiler),
    application:set_env(template_compiler, template_dir, test_data_dir(Config)),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(basic, Config) ->
    Config.

end_per_group(basic, _Config) ->
    ok.

expr_test(_Config) ->
    % io:format(user, "~p", [Config]),
    {ok, Bin1} = template_compiler:render("expr.tpl", #{}, [], undefined),
    <<>> = iolist_to_binary(Bin1),
    {ok, Bin2} = template_compiler:render("expr.tpl", #{ v => "aüç" }, [], undefined),
    <<"aüç"/utf8>> = iolist_to_binary(Bin2),
    {ok, Bin3} = template_compiler:render("expr.tpl", #{ v => 1234 }, [], undefined),
    <<"1234">> = iolist_to_binary(Bin3),
    {ok, Bin4} = template_compiler:render("expr.tpl", #{ v => atom }, [], undefined),
    <<"atom">> = iolist_to_binary(Bin4),
    {ok, Bin5} = template_compiler:render("expr.tpl", #{ v => <<"binary">> }, [], undefined),
    <<"binary">> = iolist_to_binary(Bin5),
    {ok, Bin6} = template_compiler:render("expr.tpl", #{ v => {{2008,12,10},{15,30,0}} }, [], undefined),
    <<"2008-12-10 15:30:00">> = iolist_to_binary(Bin6),
    ok.

expr_op_test(_Config) ->
    {ok, Bin1} = template_compiler:render("expr_op.tpl", #{ a => 10, b => 5 }, [], undefined),
    <<"15|5|50|5|2.0">> = iolist_to_binary(Bin1),
    ok.

expr_op_eq_neq_test(_Config) ->
    % {{ a == b }}|{{ a === b }}|{{ a /= b }}|{{ a =/= b }}|{{ a != b }}|{{ a !== b }}
    {ok, Bin1} = template_compiler:render("expr_op_eq_neq.tpl", #{ a => 10, b => 5 }, [], undefined),
    <<"false|false|true|true|true|true">> = iolist_to_binary(Bin1),
    {ok, Bin2} = template_compiler:render("expr_op_eq_neq.tpl", #{ a => 5, b => 5 }, [], undefined),
    <<"true|true|false|false|false|false">> = iolist_to_binary(Bin2),
    {ok, Bin3} = template_compiler:render("expr_op_eq_neq.tpl", #{ a => 5, b => <<"5">> }, [], undefined),
    <<"true|false|false|true|false|true">> = iolist_to_binary(Bin3),
    {ok, Bin4} = template_compiler:render("expr_op_eq_neq.tpl", #{ a => 30, b => true }, [], undefined),
    <<"true|false|false|true|false|true">> = iolist_to_binary(Bin4),
    ok.

expr_filter(_Config) ->
    {ok, Bin1} = template_compiler:render("expr_filter.tpl", #{ v => 10 }, [], undefined),
    <<"a20b">> = iolist_to_binary(Bin1),
    {ok, Bin2} = template_compiler:render("expr_filter_2.tpl", #{ a => 1, b => 2, c => 3 }, [], undefined),
    <<"*1:2*1:3:2*">> = iolist_to_binary(Bin2),
    ok.

expr_literals(_Config) ->
    {ok, Bin1} = template_compiler:render("literals.tpl", #{}, [], undefined),
    <<"true
false
undefined
42
1.61803
<<104,101,108,108,111,32,119,111,114,108,100>>
atom
[a,b,c]
#{<<97>> => 1,<<98>> => 2}">> = iolist_to_binary(Bin1),
    ok.

expr_nested(_Config) ->
    Vars1 = #{
        a => #{
            b => #{
                c => 20
            }
        }
    },
    {ok, Bin1} = template_compiler:render("expr_nested.tpl", Vars1, [], undefined),
    <<"a20c">> = iolist_to_binary(Bin1),
    Vars2 = #{
        v => b,
        a => #{
            b => #{
                c => 20
            }
        }
    },
    {ok, Bin2} = template_compiler:render("expr_nested_2.tpl", Vars2, [], undefined),
    <<"a20c">> = iolist_to_binary(Bin2).


expr_autoid(_Config) ->
    {ok, Bin1} = template_compiler:render("expr_autoid.tpl", #{}, [], undefined),
    {match, _} = re:run(iolist_to_binary(Bin1), "x:[a-zA-Z0-9]+-id:y"),
    {ok, Bin2} = template_compiler:render("expr_autoid_2.tpl", #{ foo => 20 }, [], undefined),
    {match, _} = re:run(iolist_to_binary(Bin2), "x:[a-zA-Z0-9]+-id-20:y").


expr_map(_Config) ->
    {ok, Bin1} = template_compiler:render("expr_map.tpl", #{}, [], undefined),
    <<"X<pre>#{}</pre>Y">> = iolist_to_binary(Bin1),
    {ok, Bin2} = template_compiler:render("expr_map_2.tpl", #{}, [], undefined),
    <<"X<pre>#{&lt;&lt;&quot;a&quot;&gt;&gt; =&gt; 1}</pre>Y">> = iolist_to_binary(Bin2),
    {ok, Bin3} = template_compiler:render("expr_map_3.tpl", #{ <<"c">> => 2 }, [], undefined),
    <<"X<pre>#{&lt;&lt;&quot;a&quot;&gt;&gt; =&gt; 1,&lt;&lt;&quot;b&quot;&gt;&gt; =&gt; 4}</pre>Y">> = iolist_to_binary(Bin3),
    {ok, Bin4} = template_compiler:render("expr_map_4.tpl", #{}, [], undefined),
    <<"1">> = iolist_to_binary(Bin4),
    {ok, Bin5} = template_compiler:render("expr_map_5.tpl", #{}, [], undefined),
    <<"1">> = iolist_to_binary(Bin5).

expr_model_call(_Config) ->
    {ok, Bin1} = template_compiler:render("expr_model_call.tpl", #{}, [], undefined),
    <<"model:foo [<<\"bar\">>,<<\"baz\">>,3] :: 4">> = iolist_to_binary(Bin1).

expr_orelse(_Config) ->
    erlang:erase(x),
    {ok, Bin1} = template_compiler:render("expr_orelse.tpl", #{ <<"a">> => 0, <<"b">> => 0 }, [], undefined),
    <<"false">> = iolist_to_binary(Bin1),
    1 = erlang:get(x),
    erlang:erase(x),
    {ok, Bin2} = template_compiler:render("expr_orelse.tpl", #{ <<"a">> => 1, <<"b">> => 0 }, [], undefined),
    <<"true">> = iolist_to_binary(Bin2),
    undefined = erlang:get(x),
    erlang:erase(x),
    {ok, Bin3} = template_compiler:render("expr_orelse.tpl", #{ <<"a">> => 0, <<"b">> => 1 }, [], undefined),
    <<"true">> = iolist_to_binary(Bin3),
    1 = erlang:get(x),
    erlang:erase(x).

expr_andalso(_Config) ->
    erlang:erase(x),
    {ok, Bin1} = template_compiler:render("expr_andalso.tpl", #{ <<"a">> => 0, <<"b">> => 0 }, [], undefined),
    <<"false">> = iolist_to_binary(Bin1),
    undefined = erlang:get(x),
    erlang:erase(x),
    {ok, Bin2} = template_compiler:render("expr_andalso.tpl", #{ <<"a">> => 1, <<"b">> => 1 }, [], undefined),
    <<"true">> = iolist_to_binary(Bin2),
    1 = erlang:get(x),
    erlang:erase(x),
    {ok, Bin3} = template_compiler:render("expr_andalso.tpl", #{ <<"a">> => 1, <<"b">> => 0 }, [], undefined),
    <<"false">> = iolist_to_binary(Bin3),
    1 = erlang:get(x),
    erlang:erase(x).

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).
