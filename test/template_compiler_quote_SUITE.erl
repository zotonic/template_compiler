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
        ,translated_literal_column_test
        ,list_operator_column_test
        ,multiline_string_token_position_test
        ,multiline_trans_literal_token_position_test
        ,multiline_comment_token_position_test
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


%% Every consumed source character contributes one column, including the
%% translation prefix, escaped quotes and Unicode code points.
translated_literal_column_test(_Config) ->
    Prefixes = [
        "{{ value|default:_\"Untitled\"",
        "{{ value|default:_'Untitled'",
        "{{ value|default:_\"a\\\"b\"",
        "{{ value|default:_'a\\'b'",
        "😀 {{ value|default:_\"café 日本語\"",
        "{{ value|default:_\"one\"|default:_'two'"
    ],
    lists:foreach(fun(Prefix) ->
        Template = unicode:characters_to_binary(Prefix ++ "|escape }}"),
        {ok, Tokens} = template_compiler_scanner:scan(Template),
        Column = length(Prefix) + 2,
        [{identifier, {undefined, 1, Column}, <<"escape">>}] =
            [T || {identifier, _, <<"escape">>} = T <- Tokens],
        CloseColumn = Column + 7,
        {close_var, {undefined, 1, CloseColumn}, <<"}}">>} = lists:last(Tokens)
    end, Prefixes).

%% Two-character operators must not shift later expression locations.
list_operator_column_test(_Config) ->
    lists:foreach(fun(Operator) ->
        {ok, Tokens} = template_compiler_scanner:scan(
            iolist_to_binary(["{{ a ", Operator, " b }}"])),
        [{identifier, {undefined, 1, 9}, <<"b">>}] =
            [T || {identifier, _, <<"b">>} = T <- Tokens]
    end, ["++", "--"]).

multiline_string_token_position_test(_Config) ->
    Template = <<"{% print \"foo\nbar\" x %}">>,
    {ok, Tokens} = template_compiler_scanner:scan(Template),
    {identifier, {undefined, 2, 6}, <<"x">>} = lists:nth(4, Tokens),
    ok.

multiline_trans_literal_token_position_test(_Config) ->
    Template = <<"{% print _\"foo\nbar\" x %}">>,
    {ok, Tokens} = template_compiler_scanner:scan(Template),
    {identifier, {undefined, 2, 6}, <<"x">>} = lists:nth(4, Tokens),
    ok.

multiline_comment_token_position_test(_Config) ->
    Template = <<"{# foo\n#}{% print x %}">>,
    {ok, Tokens} = template_compiler_scanner:scan(Template),
    {identifier, {undefined, 2, 12}, <<"x">>} = lists:nth(3, Tokens),
    ok.

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).
