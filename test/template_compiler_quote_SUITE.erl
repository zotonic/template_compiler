-module(template_compiler_quote_SUITE).

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
        [single_quote_test
        ,single_quote_slash_test
        ,double_quote_test
        ,double_quote_slash_test
        ,back_quote_test
        ,back_quote_slash_test
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

single_quote_test(_Config) ->
    {ok, Bin} = template_compiler:render("quote_single.tpl", #{}, [], undefined),
    <<"<pre>&lt;&lt;&quot;foo&quot;&gt;&gt;</pre>'bar'">> = iolist_to_binary(Bin),
    ok.

single_quote_slash_test(_Config) ->
    {ok, Bin} = template_compiler:render("quote_single_slash.tpl", #{}, [], undefined),
    <<"<pre>&lt;&lt;&quot;&#39;foo&#39;&quot;&gt;&gt;</pre>\\'bar\\'">> = iolist_to_binary(Bin),
    ok.

double_quote_test(_Config) ->
    {ok, Bin} = template_compiler:render("quote_double.tpl", #{}, [], undefined),
    <<"<pre>&lt;&lt;&quot;foo&quot;&gt;&gt;</pre>\"bar\"">> = iolist_to_binary(Bin),
    ok.

double_quote_slash_test(_Config) ->
    {ok, Bin} = template_compiler:render("quote_double_slash.tpl", #{}, [], undefined),
    <<"<pre>&lt;&lt;&quot;\\&quot;foo\\&quot;&quot;&gt;&gt;</pre>\\\"bar\\\"">> = iolist_to_binary(Bin),
    ok.

back_quote_test(_Config) ->
    {ok, Bin} = template_compiler:render("quote_back.tpl", #{}, [], undefined),
    <<"<pre>foo</pre>`bar`">> = iolist_to_binary(Bin),
    ok.

back_quote_slash_test(_Config) ->
    {ok, Bin} = template_compiler:render("quote_back_slash.tpl", #{}, [], undefined),
    <<"<pre>&#39;\\\\`foo\\\\`&#39;</pre>\\`bar\\`">> = iolist_to_binary(Bin),
    ok.

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).
