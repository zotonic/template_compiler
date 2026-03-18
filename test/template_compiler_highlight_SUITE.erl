-module(template_compiler_highlight_SUITE).

-include_lib("common_test/include/ct.hrl").

-include("../include/template_compiler.hrl").

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
        [highlight_file_test
        ,highlight_binary_test
        ,highlight_escape_text_test
        ,highlight_escape_string_literal_test
        ,highlight_module_test
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

highlight_file_test(Config) ->
    Filename = filename:join([test_data_dir(Config), "hello_world.tpl"]),
    {ok, Html} = template_compiler:highlight_file(Filename),
    true = is_binary(Html),
    {_, _} = binary:match(Html, <<"template-compiler-highlight">>),
    {_, _} = binary:match(Html, <<"Hello World!">>),
    ok.

highlight_binary_test(_Config) ->
    {ok, Html} = template_compiler:highlight_binary(<<"Hello {{ name }}!">>),
    true = is_binary(Html),
    {_, _} = binary:match(Html, <<"template-compiler-highlight">>),
    {_, _} = binary:match(Html, <<"Hello ">>),
    {_, _} = binary:match(Html, <<"{{">>),
    ok.

highlight_escape_text_test(_Config) ->
    Template = <<"<script>alert(\"x\")</script>&'">>,
    {ok, Html} = template_compiler:highlight_binary(Template, <<"escape_text.tpl">>),
    nomatch = binary:match(Html, <<"<script>">>),
    nomatch = binary:match(Html, <<"</script>">>),
    {_, _} = binary:match(Html, <<"&lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt;&amp;&#39;">>),
    ok.

highlight_escape_string_literal_test(_Config) ->
    Template = <<"{{ \"<b>&\\\"\" }}">>,
    {ok, Html} = template_compiler:highlight_binary(Template, <<"escape_string.tpl">>),
    nomatch = binary:match(Html, <<"<b>">>),
    nomatch = binary:match(Html, <<"</b>">>),
    ok.

highlight_module_test(_Config) ->
    {ok, #template_file{ filename = Filename }} = template_compiler_runtime:map_template(<<"debug_value.tpl">>, [], undefined),
    {ok, Mod} = template_compiler:lookup(Filename, [], undefined),
    [{_DbgFilename, Line, Col} | _] = Mod:debug_points(),
    Value = iolist_to_binary([integer_to_binary(Line), <<":">>, integer_to_binary(Col)]),
    {ok, Html} = template_compiler:highlight_module(Mod),
    {_, _} = binary:match(Html, <<"template-compiler-highlight">>),
    {_, _} = binary:match(Html, <<"template-compiler-debug-point">>),
    {_, _} = binary:match(Html, <<"<input type=\"checkbox\"">>),
    {_, _} = binary:match(Html, <<"value=\"", Value/binary, "\"">>),
    ok.

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).
