-module(template_compiler_basic_SUITE).

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
        [hello_world_test
        ,hello_world2_test
        ,hello_world_block_test
        ,hello_world_block2_test
        ,hello_world_block3_test
        ,hello_world_comment_test
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

hello_world_test(_Config) ->
    % io:format(user, "~p", [Config]),
    {ok, Bin} = template_compiler:render("hello_world.tpl", #{}, [], undefined),
    <<"Hello World!">> = iolist_to_binary(Bin),
    ok.

hello_world2_test(_Config) ->
    {ok, Bin} = template_compiler:render("hello_world2.tpl", #{}, [], undefined),
    <<"Hello World!">> = iolist_to_binary(Bin),
    ok.

hello_world_block_test(_Config) ->
    {ok, Bin} = template_compiler:render("hello_world_block.tpl", #{}, [], undefined),
    <<"Hello World!">> = iolist_to_binary(Bin),
    ok.

hello_world_block2_test(_Config) ->
    {ok, Bin} = template_compiler:render("hello_world_block2.tpl", #{}, [], undefined),
    <<"Bye World!">> = iolist_to_binary(Bin),
    ok.

hello_world_block3_test(_Config) ->
    {ok, Bin} = template_compiler:render("hello_world_block3.tpl", #{}, [], undefined),
    <<"Bye Hello World!">> = iolist_to_binary(Bin),
    ok.

hello_world_comment_test(_Config) ->
    {ok, Bin} = template_compiler:render("hello_world_comment.tpl", #{}, [], undefined),
    <<"Hello World!">> = iolist_to_binary(Bin),
    ok.