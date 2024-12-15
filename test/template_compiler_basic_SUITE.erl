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
        ,block_nested_error_test
        ,block_render_test
        ,raw_test
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

block_nested_error_test(_Config) ->
    {error, {duplicate_nested_block, <<"main">>}} = template_compiler:render("block_nested_error.tpl", #{}, [], undefined),
    ok.

block_render_test(_Config) ->
    {ok, BinA} = template_compiler:render_block(a, "block_render.tpl", #{}, [], undefined),
    <<"A">> = iolist_to_binary(BinA),
    {ok, BinB} = template_compiler:render_block(b, "block_render.tpl", #{}, [], undefined),
    <<"B">> = iolist_to_binary(BinB),
    ok.

raw_test(_Config) ->
    {ok, Bin} = template_compiler:render("raw.tpl", #{}, [], undefined),
    <<"Before This is {%%} raw  After">> = iolist_to_binary(Bin),
    ok.

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).
