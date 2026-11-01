-module(template_compiler_trans_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

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
        ,msgctxt_scope_test
        ,msgctxt_extraction_test
        ,msgctxt_invalid_test
        ,msgctxt_templates_test
        ,msgctxt_old_runtime_test
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
msgctxt_scope_test(Config) ->
    Template = <<"{% msgctxt \"file\" %}{_ Open _}|{{ _\"Open\" }}|"
        "{% with_msgctxt \"outer\" %}{_ Open _}|"
        "{% with_msgctxt \"inner\" %}{_ Open _}{% endwith_msgctxt %}|"
        "{_ Open _}{% endwith_msgctxt %}|"
        "{% trans \"Open\" msgctxt=\"explicit\" %}|{_ Open _}">>,
    ?assertEqual(<<"file:Open|file:Open|outer:Open|inner:Open|outer:Open|explicit:Open|file:Open">>,
        render_context(Template, "scope.tpl", Config)).

msgctxt_extraction_test(Config) ->
    File = filename:join(?config(priv_dir, Config), "contexts_extract.tpl"),
    ok = file:write_file(File, <<"{% comment %}{% msgctxt \"ignored\" %}{_ Ignore _}{% endcomment %}"
        "{% msgctxt \"file\" %}{_ Open _}{{ _\"Close\" }}"
        "{% with_msgctxt \"block\" %}{% trans \"{n} hour\" \"{n} hours\" n=n msgctxt=\"explicit\" %}"
        "{_ Block _}{% endwith_msgctxt %}{_ End _}">>),
    {ok, Messages} = template_compiler:translations(File),
    ?assertEqual(lists:sort([
        {context, <<"file">>, <<"Open">>}, {context, <<"file">>, <<"Close">>},
        {context, <<"explicit">>, {plural, <<"{n} hour">>, <<"{n} hours">>}},
        {context, <<"block">>, <<"Block">>}, {context, <<"file">>, <<"End">>}
    ]), lists:sort([Key || {Key, _, _} <- Messages])).

msgctxt_invalid_test(Config) ->
    File = filename:join(?config(priv_dir, Config), "invalid_context.tpl"),
    lists:foreach(fun(Source) ->
        ?assertMatch({error, _}, template_compiler:compile_binary(Source, File, [], undefined)),
        ok = file:write_file(File, Source),
        ?assertMatch({error, _}, template_compiler:translations(File))
    end, [<<"{% msgctxt variable %}{_ Open _}">>,
        <<"{% msgctxt \"a\" %}{% msgctxt \"b\" %}">>,
        <<"output{% msgctxt \"a\" %}">>,
        <<"{% with_msgctxt variable %}{_ Open _}{% endwith_msgctxt %}">>,
        <<"{% with_msgctxt \"a\" %}{_ Open _}">>,
        <<"{% endwith_msgctxt %}">>,
        <<"{% trans \"Open\" msgctxt=variable %}">>,
        <<"{% trans \"Open\" msgctxt=\"a\"|default:\"b\" %}">>,
        <<"{% trans \"Open\" msgctxt=\"a\" msgctxt=\"b\" %}">>]).

msgctxt_templates_test(Config) ->
    Dir = ?config(priv_dir, Config),
    Included = filename:join(Dir, "included_context.tpl"),
    Parent = filename:join(Dir, "parent_context.tpl"),
    ok = file:write_file(Included, <<"{% msgctxt \"included\" %}{_ Open _}">>),
    Source = iolist_to_binary(["{% msgctxt \"caller\" %}{_ Open _}|{% include \"", Included, "\" %}"]),
    ?assertEqual(<<"caller:Open|included:Open">>, render_context(Source, "caller.tpl", Config)),
    ok = file:write_file(Parent, <<"{% msgctxt \"parent\" %}{_ Open _}|{% block body %}{_ Open _}{% endblock %}">>),
    Child = iolist_to_binary(["{% msgctxt \"child\" %}\n{% extends \"", Parent,
        "\" %}{% block body %}{_ Open _}{% endblock %}"]),
    ?assertEqual(<<"parent:Open|child:Open">>, render_context(Child, "child.tpl", Config)),
    AfterExtends = iolist_to_binary(["{% extends \"", Parent,
        "\" %}{% msgctxt \"child2\" %}{% block body %}{_ Open _}{% endblock %}"]),
    ?assertEqual(<<"parent:Open|child2:Open">>, render_context(AfterExtends, "child2.tpl", Config)).

msgctxt_old_runtime_test(Config) ->
    Source = <<"{% msgctxt \"unknown\" %}{_ Open _}|{% trans \"{n} hour\" \"{n} hours\" n=2 %}">>,
    File = filename:join(?config(priv_dir, Config), "old_runtime_context.tpl"),
    {ok, Mod} = template_compiler:compile_binary(Source, File, [], undefined),
    ?assertEqual(<<"Open|2 hours">>, iolist_to_binary(Mod:render(#{}, #{}, undefined))),
    Variables = <<"{% trans \"{value}\" value=msgctxt %}|{% trans \"{value}\" value=object.msgctxt %}">>,
    {ok, Mod2} = template_compiler:compile_binary(Variables, File ++ "vars", [], undefined),
    ?assertEqual(<<"variable|property">>, iolist_to_binary(Mod2:render(
        #{msgctxt => <<"variable">>, object => #{msgctxt => <<"property">>}}, #{}, undefined))).

render_context(Source, Name, Config) ->
    File = filename:join(?config(priv_dir, Config), Name),
    ok = file:write_file(File, Source),
    {ok, Output} = template_compiler:render(File, #{}, [{runtime, ?MODULE}], undefined),
    iolist_to_binary(Output).

get_translations(Text, _) -> {trans, [{en, Text}]}.
get_translations(Text, MsgCtxt, _) -> {trans, [{en, <<MsgCtxt/binary, ":", Text/binary>>}]}.
map_template(Template, Vars, Context) -> template_compiler_runtime:map_template(Template, Vars, Context).
is_modified(File, Time, Context) -> template_compiler_runtime:is_modified(File, Time, Context).
trace_render(_, _, _) -> ok.
trace_block(_, _, _, _) -> ok.
set_context_vars(Vars, Context) -> template_compiler_runtime:set_context_vars(Vars, Context).

get_plural_translations(_, _, _) ->
    Czech = {3, {conditional, {'==', n, 1}, 0,
        {conditional, {'&&', {'>=', n, 2}, {'<=', n, 4}}, 1, 2}}},
    Japanese = {1, 0},
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
