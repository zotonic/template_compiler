-module(template_compiler_tags_SUITE).

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
        [print_test
        ,custom_tag_test
        ,filter_tag_test
        ,autoescape_test
        ,cycle_test
        ,spaceless_test
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
        maps:get(a, Args),
        $-, maps:get(v, Vars),
        $-, z_convert:to_binary(maps:get(b, Args)),
        $-, z_convert:to_binary(maps:get(c, Args))
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

