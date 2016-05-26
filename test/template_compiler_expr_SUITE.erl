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
        ]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(template_compiler),
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
