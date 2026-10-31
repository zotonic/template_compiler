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
        ,multiline_trans_text_token_position_test
        ,plural_test
        ,plural_languages_test
        ,plural_requires_n_test
        ,plural_extraction_test
        ,plural_scope_test
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

multiline_trans_text_token_position_test(_Config) ->
    Template = <<"{_ foo\nbar _}{% print x %}">>,
    {ok, Tokens} = template_compiler_scanner:scan(Template),
    {identifier, {undefined, 2, 16}, <<"x">>} = lists:nth(6, Tokens),
    ok.

plural_test(Config) ->
    Template = <<"{% trans \"{n} hour ago\" \"{n} hours ago\" n=hours %}">>,
    Filename = filename:join(?config(priv_dir, Config), "plural.tpl"),
    {ok, Mod} = template_compiler:compile_binary(Template, Filename, [], undefined),
    lists:foreach(fun({N, Expected}) ->
        Expected = iolist_to_binary(Mod:render(#{hours => N}, #{}, undefined)),
        % An unsupported language uses the English rule and forms.
        Expected = iolist_to_binary(Mod:render(#{hours => N, z_language => ar}, #{}, undefined))
    end, [{0, <<"0 hours ago">>}, {1, <<"1 hour ago">>}, {2, <<"2 hours ago">>}]),
    ok.

plural_languages_test(Config) ->
    Template = <<"{% autoescape on %}{% trans \"{n} hour {who}\" \"{n} hours {who}\" n=hours who=name %}{% endautoescape %}">>,
    Filename = filename:join(?config(priv_dir, Config), "plural_languages.tpl"),
    {ok, Mod} = template_compiler:compile_binary(Template, Filename, [{runtime, ?MODULE}], undefined),
    lists:foreach(fun({Lang, N, Expected}) ->
        put(plural_count_reads, 0),
        Expected = iolist_to_binary(Mod:render(#{hours => N, name => <<"<x>">>, z_language => Lang}, #{}, undefined)),
        1 = erase(plural_count_reads)
    end, [{cs, 1, <<"1 hodina &lt;x&gt;">>}, {cs, 3, <<"3 hodiny &lt;x&gt;">>},
        {cs, 5, <<"5 hodin &lt;x&gt;">>}, {ja, 5, <<"5 jikan &lt;x&gt;">>},
        {ar, 2, <<"2 hours &lt;x&gt;">>}]),
    ok.

plural_requires_n_test(Config) ->
    Filename = filename:join(?config(priv_dir, Config), "plural_missing_n.tpl"),
    {error, _} = template_compiler:compile_binary(<<"{% trans \"one\" \"many\" %}">>, Filename, [], undefined),
    ok.

plural_extraction_test(Config) ->
    Filename = filename:join(?config(priv_dir, Config), "plural_extract.tpl"),
    ok = file:write_file(Filename, <<"{% trans \"{n} hour\" \"{n} hours\" n=hours %}">>),
    {ok, [{{plural, <<"{n} hour">>, <<"{n} hours">>}, [], {_, 1, _}}]} = template_compiler:translations(Filename),
    ok.

plural_scope_test(Config) ->
    Template = <<"{% trans \"\" \"many\" n=1 %}|"
        "{% for number in numbers %}"
        "{% trans \"{n} x\" \"{n} xs\" n=number + 1 %};{% endfor %}">>,
    Filename = filename:join(?config(priv_dir, Config), "plural_scope.tpl"),
    {ok, Mod} = template_compiler:compile_binary(Template, Filename, [], undefined),
    <<"|1 x;2 xs;">> = iolist_to_binary(Mod:render(#{numbers => [0, 1]}, #{}, undefined)),
    ok.

%% Minimal test runtime: make a count lookup observable and provide catalogs.
get_plural_translations(_, _, _) ->
    {ok, Czech} = template_compiler_plural:parse(<<"nplurals=3; plural=n==1 ? 0 : n>=2 && n<=4 ? 1 : 2;">>),
    {ok, Japanese} = template_compiler_plural:parse(<<"nplurals=1; plural=0;">>),
    {trans, [
        {cs, {Czech, {<<"{n} hodina {who}">>, <<"{n} hodiny {who}">>, <<"{n} hodin {who}">>}}},
        {ja, {Japanese, {<<"{n} jikan {who}">>}}}
    ]}.

find_value(hours, Value, Vars, Context) ->
    put(plural_count_reads, get(plural_count_reads) + 1),
    template_compiler_runtime:find_value(hours, Value, Vars, Context);
find_value(Key, Value, Vars, Context) ->
    template_compiler_runtime:find_value(Key, Value, Vars, Context).
find_nested_value(Keys, Vars, Context) ->
    template_compiler_runtime:find_nested_value(Keys, Vars, Context).
compile_map_nested_value(Tokens, Var, Context) ->
    template_compiler_runtime:compile_map_nested_value(Tokens, Var, Context).
lookup_translation(Tr, Vars, Context) ->
    template_compiler_runtime:lookup_translation(Tr, Vars, Context).
to_render_result(Value, Vars, Context) ->
    template_compiler_runtime:to_render_result(Value, Vars, Context).
escape(Value, Context) -> template_compiler_runtime:escape(Value, Context).
get_context_name(_) -> undefined.
trace_compile(_, _, _, _) -> ok.

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).
