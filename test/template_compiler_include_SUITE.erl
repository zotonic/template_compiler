-module(template_compiler_include_SUITE).

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
        [include_test
        ,include_dynamic_test
        ,include_args_test
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

include_test(_Config) ->
    % io:format(user, "~p", [Config]),
    {ok, Bin1} = template_compiler:render("include.tpl", #{}, [], undefined),
    <<"abc">> = iolist_to_binary(Bin1),
    ok.

include_dynamic_test(_Config) ->
    {ok, Bin1} = template_compiler:render("include_dynamic.tpl", #{ t => "include_b.tpl" }, [], undefined),
    <<"abc">> = iolist_to_binary(Bin1),
    ok.

include_args_test(_Config) ->
    {ok, Bin1} = template_compiler:render("include_args.tpl", #{ a => 1, b => 2 }, [], undefined),
    <<"a3:2:truec">> = iolist_to_binary(Bin1),
    ok.
