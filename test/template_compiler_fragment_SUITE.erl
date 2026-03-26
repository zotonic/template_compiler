-module(template_compiler_fragment_SUITE).

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
        [fragment_use_test
        ,fragment_useblock_test
        ,fragment_useblock_blocks_test
        ,fragment_useblock_multiple_fragments_test
        ,fragment_extends_same_name_blocks_test
        ,fragment_extends_lookup_test
        ,fragment_extends_override_test
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

fragment_use_test(_Config) ->
    {ok, Bin} = template_compiler:render("fragment.tpl", #{}, [], undefined),
    <<"AB[x][y]C">> = iolist_to_binary(Bin),
    ok.

fragment_useblock_test(_Config) ->
    {ok, Bin} = template_compiler:render("fragment_useblock.tpl", #{}, [], undefined),
    <<"<div>T:Body</div>">> = iolist_to_binary(Bin),
    ok.

fragment_useblock_blocks_test(_Config) ->
    {ok, Bin} = template_compiler:render("fragment_useblock_blocks.tpl", #{}, [], undefined),
    <<"<section>Caller Base<div>Body</div></section>">> = iolist_to_binary(Bin),
    ok.

fragment_useblock_multiple_fragments_test(_Config) ->
    {ok, Bin} = template_compiler:render("fragment_useblock_multiple.tpl", #{}, [], undefined),
    <<"<section>A1 Base A<div>Body A</div></section><section>B1 Base B<div>Body B</div></section>">> =
        z_string:trim(iolist_to_binary(Bin)),
    ok.

fragment_extends_same_name_blocks_test(_Config) ->
    {ok, Bin} = template_compiler:render("fragment_extends_same_name_child.tpl", #{}, [], undefined),
    <<"<section>Caller Child Base<div>Body</div></section>">> = iolist_to_binary(Bin),
    ok.

fragment_extends_lookup_test(_Config) ->
    {ok, Bin} = template_compiler:render("fragment_extends_child.tpl", #{}, [], undefined),
    <<"Hello child">> = iolist_to_binary(Bin),
    ok.

fragment_extends_override_test(_Config) ->
    {ok, Bin} = template_compiler:render("fragment_extends_override_child.tpl", #{}, [], undefined),
    <<"Hi child">> = iolist_to_binary(Bin),
    ok.

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).
