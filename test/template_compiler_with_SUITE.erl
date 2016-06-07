-module(template_compiler_with_SUITE).

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
        [with_test
        ,with_multiple_test
        ,with_tuple_test
        ,with_value_test
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

with_test(_Config) ->
    % io:format(user, "~p", [Config]),
    {ok, Bin1} = template_compiler:render("with.tpl", #{ v => 1 }, [], undefined),
    <<"1">> = iolist_to_binary(Bin1),
    ok.

with_multiple_test(_Config) ->
    {ok, Bin1} = template_compiler:render("with_multiple.tpl", #{ v => 1, w => 2 }, [], undefined),
    <<"1,2">> = iolist_to_binary(Bin1),
    ok.

with_tuple_test(_Config) ->
    {ok, Bin1} = template_compiler:render("with_tuple.tpl", #{ v => {1,2} }, [], undefined),
    <<"1,2">> = iolist_to_binary(Bin1),
    ok.

with_value_test(_Config) ->
    {ok, Bin1} = template_compiler:render("with_value.tpl", #{ v => 1 }, [], undefined),
    <<"1">> = iolist_to_binary(Bin1),
    ok.

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).
