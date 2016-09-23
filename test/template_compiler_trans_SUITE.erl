-module(template_compiler_trans_SUITE).

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
        [trans_test
        ,trans_escape_test
        ,trans_string_test
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

trans_test(_Config) ->
    % io:format(user, "~p", [Config]),
    {ok, Bin1} = template_compiler:render("trans.tpl", #{ v => "bar" }, [], undefined),
    <<"Hello bar Bye">> = iolist_to_binary(Bin1),
    {ok, Bin2} = template_compiler:render("trans.tpl", #{ v => 2*20 }, [], undefined),
    <<"Hello 40 Bye">> = iolist_to_binary(Bin2),
    ok.

trans_escape_test(_Config) ->
    {ok, Bin1} = template_compiler:render("trans_escape.tpl", #{ v => "bar" }, [], undefined),
    <<"Hello {foo} bar Bye">> = iolist_to_binary(Bin1),
    ok.

trans_string_test(_Config) ->
    {ok, Bin1} = template_compiler:render("trans_string.tpl", #{}, [], undefined),
    <<"Hello">> = iolist_to_binary(Bin1),
    ok.

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).
