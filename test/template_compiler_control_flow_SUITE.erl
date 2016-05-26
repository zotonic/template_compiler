-module(template_compiler_control_flow_SUITE).

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
        [if_test
        ,if_else_test
        ,if_var_test
        ,for_test
        ,for_forloop_test
        ,for_multivar_test
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

if_test(_Config) ->
    % io:format(user, "~p", [Config]),
    {ok, Bin} = template_compiler:render("if.tpl", #{}, [], undefined),
    <<"Yes">> = iolist_to_binary(Bin),
    ok.

if_else_test(_Config) ->
    {ok, Bin} = template_compiler:render("if_else.tpl", #{}, [], undefined),
    <<"Else">> = iolist_to_binary(Bin),
    ok.

if_var_test(_Config) ->
    {ok, Bin1} = template_compiler:render("if_var.tpl", #{ v => 1 }, [], undefined),
    <<"one">> = z_string:trim(iolist_to_binary(Bin1)),
    {ok, Bin2} = template_compiler:render("if_var.tpl", #{ v => 2 }, [], undefined),
    <<"two">> = z_string:trim(iolist_to_binary(Bin2)),
    {ok, Bin3} = template_compiler:render("if_var.tpl", #{ v => 3 }, [], undefined),
    <<"three">> = z_string:trim(iolist_to_binary(Bin3)),
    {ok, Bin4} = template_compiler:render("if_var.tpl", #{ v => 4 }, [], undefined),
    <<"else">> = z_string:trim(iolist_to_binary(Bin4)),
    ok.

for_test(_Config) ->
    {ok, Bin1} = template_compiler:render("for.tpl", #{ v => [1,2,3,4] }, [], undefined),
    <<"1,2,3,4,">> = z_string:trim(iolist_to_binary(Bin1)),
    {ok, Bin2} = template_compiler:render("for_empty.tpl", #{ v => [] }, [], undefined),
    <<"empty">> = z_string:trim(iolist_to_binary(Bin2)),
    ok.

for_forloop_test(_Config) ->
    {ok, Bin1} = template_compiler:render("for_forloop_counter.tpl", #{ v => [a,b,c] }, [], undefined),
    <<"a:1,b:2,c:3,">> = z_string:trim(iolist_to_binary(Bin1)),
    {ok, Bin2} = template_compiler:render("for_forloop_firstlast.tpl", #{ v => [a,b,c] }, [], undefined),
    <<"first|a,b,c,|last">> = z_string:trim(iolist_to_binary(Bin2)),
    ok.

for_multivar_test(_Config) ->
    {ok, Bin1} = template_compiler:render("for_multivar.tpl", #{ v => [{a,1},{b,2},{c,3}] }, [], undefined),
    <<"a:1,b:2,c:3,">> = z_string:trim(iolist_to_binary(Bin1)),
    ok.
