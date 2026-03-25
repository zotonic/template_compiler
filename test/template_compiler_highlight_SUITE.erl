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
        ,highlight_line_numbers_test
        ,highlight_model_expr_test
        ,highlight_unicode_model_expr_test
        ,highlight_model_bracket_expr_test
        ,highlight_trans_literal_test
        ,highlight_trans_tag_test
        ,highlight_unicode_trans_tag_test
        ,highlight_single_quote_include_test
        ,highlight_nav_anchor_include_test
        ,highlight_nav_anchor_extends_overrules_test
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

highlight_line_numbers_test(_Config) ->
    {ok, Html} = template_compiler:highlight_binary(<<"a\nb">>),
    {_, _} = binary:match(Html, <<"template-compiler-line-number">>),
    {_, _} = binary:match(Html, <<"data-line=\"1\"">>),
    {_, _} = binary:match(Html, <<"data-line=\"2\"">>),
    ok.

highlight_model_expr_test(Config) ->
    Filename = filename:join([test_data_dir(Config), "expr_model_call.tpl"]),
    {ok, Html} = template_compiler:highlight_file(Filename),
    nomatch = binary:match(Html, <<"{model,">>),
    {_, _} = binary:match(Html, <<"m">>),
    {_, _} = binary:match(Html, <<"foo">>),
    {_, _} = binary:match(Html, <<"bar">>),
    {_, _} = binary:match(Html, <<"baz">>),
    nomatch = binary:match(Html, <<".3">>),
    {_, _} = binary:match(Html, <<"[">>),
    {_, _} = binary:match(Html, <<"3">>),
    {_, _} = binary:match(Html, <<"]">>),
    {_, _} = binary:match(Html, <<"::">>),
    ok.

highlight_unicode_model_expr_test(_Config) ->
    Template = <<"é"/utf8, "{{ m.foo[3]::4 }}">>,
    {ok, Html} = template_compiler:highlight_binary(Template, <<"unicode_expr.tpl">>),
    nomatch = binary:match(Html, <<"{model,">>),
    assert_in_order(Html, [
        <<"1</span>é{{ "/utf8>>,
        <<"<span style=\"color:#0f766e;font-weight:600;\">m</span>">>,
        <<"<span style=\"color:#be123c;font-weight:600;\">.</span>">>,
        <<"<span style=\"color:#1d4ed8;\">foo</span>">>,
        <<"<span style=\"color:#be123c;font-weight:600;\">[</span>">>,
        <<"3">>,
        <<"<span style=\"color:#be123c;font-weight:600;\">]</span>">>,
        <<"<span style=\"color:#be123c;font-weight:600;\">::</span>">>,
        <<"4 }}</span>">>
    ]),
    ok.

highlight_model_bracket_expr_test(_Config) ->
    {ok, Html} = template_compiler:highlight_binary(<<"{{ m.foo[a + 1] }}">>),
    nomatch = binary:match(Html, <<"{model,">>),
    {_, _} = binary:match(Html, <<"foo">>),
    {_, _} = binary:match(Html, <<"[">>),
    {_, _} = binary:match(Html, <<"a">>),
    {_, _} = binary:match(Html, <<"+">>),
    {_, _} = binary:match(Html, <<"1">>),
    {_, _} = binary:match(Html, <<"]">>),
    ok.

highlight_trans_literal_test(Config) ->
    Filename = filename:join([test_data_dir(Config), "trans_string.tpl"]),
    {ok, Html} = template_compiler:highlight_file(Filename),
    nomatch = binary:match(Html, <<"{trans,">>),
    {_, _} = binary:match(Html, <<"{{">>),
    {_, _} = binary:match(Html, <<"_">>),
    {_, _} = binary:match(Html, <<"Hello">>),
    {_, _} = binary:match(Html, <<"}}">>),
    ok.

highlight_trans_tag_test(_Config) ->
    {ok, Html} = template_compiler:highlight_binary(<<"{% trans \"hello {name}\" name=\"piet\" %}">>),
    nomatch = binary:match(Html, <<"{trans_ext,">>),
    {_, _} = binary:match(Html, <<"{%">>),
    {_, _} = binary:match(Html, <<"trans">>),
    {_, _} = binary:match(Html, <<"hello {name}">>),
    {_, _} = binary:match(Html, <<"name">>),
    {_, _} = binary:match(Html, <<"piet">>),
    {_, _} = binary:match(Html, <<"%}">>),
    ok.

highlight_unicode_trans_tag_test(_Config) ->
    Template = <<"ø"/utf8, "{% trans \"héllo {name}\" name=\"søren\" %}"/utf8>>,
    {ok, Html} = template_compiler:highlight_binary(Template, <<"unicode_trans.tpl">>),
    nomatch = binary:match(Html, <<"{trans_ext,">>),
    assert_in_order(Html, [
        <<"1</span>ø{% "/utf8>>,
        <<"<span style=\"color:#0f766e;font-weight:600;\">trans</span> ">>,
        <<"<span style=\"color:#b45309;\">&quot;héllo {name}&quot;</span> "/utf8>>,
        <<"<span style=\"color:#1d4ed8;\">name</span>">>,
        <<"<span style=\"color:#be123c;font-weight:600;\">=</span>">>,
        <<"<span style=\"color:#b45309;\">&quot;søren&quot;</span> %}"/utf8>>
    ]),
    ok.

highlight_single_quote_include_test(_Config) ->
    Template = <<"{% include 'a.tpl' %}">>,
    {ok, Html} = template_compiler:highlight_binary(Template, <<"single_quote_include.tpl">>),
    {_, _} = binary:match(Html, <<"{%">>),
    {_, _} = binary:match(Html, <<"include">>),
    {_, _} = binary:match(Html, <<"&#39;a.tpl&#39;">>),
    {_, _} = binary:match(Html, <<"%}">>),
    ok.

highlight_nav_anchor_include_test(_Config) ->
    Template = <<"{% include 'a.tpl' %}">>,
    {ok, Html} = template_compiler:highlight_binary(Template, <<"nav_include.tpl">>),
    {_, _} = binary:match(Html, <<"template-compiler-nav-anchor">>),
    assert_in_order(Html, [
        <<"data-template-nav=\"include\"">>,
        <<"data-line=\"1\"">>,
        <<"data-column=\"1\"">>,
        <<"\"></span>{% ">>,
        <<"<span style=\"color:#0f766e;font-weight:600;\">include</span>">>
    ]),
    ok.

highlight_nav_anchor_extends_overrules_test(_Config) ->
    Template = <<"{% extends \"base.tpl\" %}\n{% overrules %}">>,
    {ok, Html} = template_compiler:highlight_binary(Template, <<"nav_extends_overrules.tpl">>),
    assert_in_order(Html, [
        <<"data-template-nav=\"extends\"">>,
        <<"data-line=\"1\"">>,
        <<"data-column=\"1\"">>,
        <<"\"></span>{% ">>,
        <<"<span style=\"color:#0f766e;font-weight:600;\">extends</span>">>,
        <<"data-template-nav=\"overrules\"">>,
        <<"data-line=\"2\"">>,
        <<"data-column=\"1\"">>,
        <<"\"></span>{% ">>,
        <<"<span style=\"color:#0f766e;font-weight:600;\">overrules</span>">>
    ]),
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
    assert_in_order(Html, [
        <<"value=\"", Value/binary, "\"">>,
        <<"</label>">>,
        <<"<span style=\"color:#1d4ed8;\">a</span>">>
    ]),
    ok.

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).

assert_in_order(Bin, Parts) ->
    assert_in_order(Bin, Parts, 0).

assert_in_order(_Bin, [], _Offset) ->
    ok;
assert_in_order(Bin, [Part | Rest], Offset) ->
    Scope = byte_size(Bin) - Offset,
    case binary:match(Bin, Part, [{scope, {Offset, Scope}}]) of
        {Pos, Len} ->
            assert_in_order(Bin, Rest, Pos + Len);
        nomatch ->
            ct:fail({missing_in_order, Part, Offset})
    end.
