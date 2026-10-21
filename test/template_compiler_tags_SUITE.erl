-module(template_compiler_tags_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("template_compiler/include/template_compiler.hrl").
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
        [print_test
        ,custom_tag_test
        ,filter_tag_test
        ,autoescape_test
        ,cycle_test
        ,spaceless_test
        ,lib_flush_test
        ,leading_tag_test
        ,lib_url_requires_filename_test
        ,lib_tag_debug_points_test
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

print_test(_Config) ->
    % io:format(user, "~p", [Config]),
    {ok, Bin1} = template_compiler:render("print.tpl", #{ v => <<"hallo ?<>">> }, [], undefined),
    <<"x-<pre>&lt;&lt;&quot;hallo ?&lt;&gt;&quot;&gt;&gt;</pre>-y">> = iolist_to_binary(Bin1),
    ok.

custom_tag_test(_Config) ->
    {ok, Bin1} = template_compiler:render("custom_tag.tpl", #{ v => <<"hallo">> }, [], undefined),
    <<"hallo-hallo-1-true">> = iolist_to_binary(Bin1),
    ok.

% Called by custom_tag.tpl
render(Args, Vars, _Context) ->
    [
        proplists:get_value(a, Args),
        $-, maps:get(v, Vars),
        $-, z_convert:to_binary(proplists:get_value(b, Args)),
        $-, z_convert:to_binary(proplists:get_value(c, Args))
    ].

filter_tag_test(_Config) ->
    {ok, Bin1} = template_compiler:render("filter_tag.tpl", #{ v => 10 }, [], undefined),
    <<"a-20-b">> = iolist_to_binary(Bin1),
    {ok, Bin2} = template_compiler:render("filter_tag_2.tpl", #{ v => 10, c => "foo" }, [], undefined),
    <<"a-20:x:foo-b">> = iolist_to_binary(Bin2),
    ok.

autoescape_test(_Config) ->
    {ok, Bin1} = template_compiler:render("autoescape_tag.tpl", #{ v => "<foo>" }, [], undefined),
    <<"a-&lt;foo&gt;-b">> = iolist_to_binary(Bin1),
    {ok, Bin2} = template_compiler:render("autoescape_tag_2.tpl", #{ v => "<foo>" }, [], undefined),
    <<"a-<foo>-b">> = iolist_to_binary(Bin2),
    ok.

cycle_test(_Config) ->
    {ok, Bin1} = template_compiler:render("cycle_tag.tpl", #{ v => [1,2,3,4] }, [], undefined),
    <<"a-abca-b">> = iolist_to_binary(Bin1),
    {ok, Bin2} = template_compiler:render("cycle_tag_2.tpl", #{ v => [1,2,3,4] }, [], undefined),
    <<"a-abca-b">> = iolist_to_binary(Bin2),
    ok.

spaceless_test(_Config) ->
    {ok, Bin1} = template_compiler:render("spaceless_tag.tpl", #{}, [], undefined),
    <<"a-<a> x<span>xxx </span></a>-b">> = iolist_to_binary(Bin1),
    ok.

lib_tag_debug_points_test(_Config) ->
    {ok, #template_file{filename = Filename}} = template_compiler_runtime:map_template(<<"lib_tag.tpl">>, [], undefined),
    {ok, Mod} = template_compiler:lookup(Filename, [], undefined),
    [_Point] = Mod:debug_points(),
    ok.

%% Parse and compile at column one as well as after ordinary template text.
lib_flush_test(Config) ->
    lists:foreach(
        fun({Source, ExpectedValue}) ->
            lists:foreach(
                fun(Prefix) ->
                    Template = <<Prefix/binary, Source/binary>>,
                    {ok, Tokens} = template_compiler_scanner:scan(<<"lib_flush.tpl">>, Template),
                    {ok, {base, Elements}} = template_compiler_parser:parse(Tokens),
                    {lib, _, [], [{{identifier, _, <<"flush">>}, Value}]} = lists:last(Elements),
                    case ExpectedValue of
                        true -> true = Value;
                        _ -> {string_literal, _, ExpectedValue} = Value
                    end,
                    Filename = filename:join(test_data_dir(Config), "lib_flush_generated.tpl"),
                    {ok, Mod} = template_compiler:compile_binary(Template, Filename, [], undefined),
                    Prefix = iolist_to_binary(Mod:render(#{}, #{}, undefined))
                end,
                [<<>>, <<"prefix\n">>])
        end,
        [{<<"{% lib flush %}">>, true},
         {<<"{% lib flush=\"css\" %}">>, <<"css">>},
         {<<"{% lib flush=\"javascript\" %}">>, <<"javascript">>},
         {<<"{% lib flush=\"js\" %}">>, <<"js">>}]),
    ok.

%% The root ambiguity affected ordinary tags too, not only lib tags.
leading_tag_test(Config) ->
    lists:foreach(
        fun({Source, Expected}) ->
            Filename = filename:join(test_data_dir(Config), "leading_tag_generated.tpl"),
            {ok, Mod} = template_compiler:compile_binary(Source, Filename, [], undefined),
            Expected = iolist_to_binary(Mod:render(#{}, #{}, undefined))
        end,
        [{<<"{% lib \"a.js\" collect %}tail">>, <<"tail">>},
         {<<"{% lib_url \"a.js\" %}tail">>, <<"tail">>},
         {<<"{% if true %}{% lib flush %}inside{% endif %}tail">>, <<"insidetail">>},
         {<<"{% spaceless %} <b>x</b> <i>y</i> {% endspaceless %}">>, <<"<b>x</b><i>y</i>">>},
         {<<"{% comment %}hidden{% endcomment %}tail">>, <<"tail">>},
         {<<"{{ 1 }}tail">>, <<"1tail">>},
         {<<>>, <<>>}]),
    ok.

lib_url_requires_filename_test(_Config) ->
    lists:foreach(
        fun(Source) ->
            {ok, Tokens} = template_compiler_scanner:scan(<<"lib_url.tpl">>, Source),
            {error, _} = template_compiler_parser:parse(Tokens)
        end,
        [<<"{% lib_url flush %}">>, <<"{% lib_url %}">>]),
    ok.

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).
