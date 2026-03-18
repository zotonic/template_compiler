-module(template_compiler_include_SUITE).

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
        [include_test
        ,include_dynamic_test
        ,include_args_test
        ,include_undefined_test
        ,compose_test
        ,compose_inherit_test
        ,debug_points_metadata_test
        ,debug_points_runtime_test
        ,flush_debug_test
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

include_test(_Config) ->
    % io:format(user, "~p", [Config]),
    {ok, Bin1} = template_compiler:render("include.tpl", #{}, [], undefined),
    <<"abc">> = iolist_to_binary(Bin1),

    {ok, #template_file{ filename = Filename }} = template_compiler_runtime:map_template(<<"include.tpl">>, [], undefined),
    {ok, Mod} = template_compiler:lookup(Filename, [], undefined),
    [ #{
        template := <<"include_b.tpl">>,
        line := 1,
        column := _,
        is_catinclude := false,
        method := normal
      } ] = Mod:includes(),

    ok.

include_dynamic_test(_Config) ->
    {ok, Bin1} = template_compiler:render("include_dynamic.tpl", #{ t => "include_b.tpl" }, [], undefined),
    <<"abc">> = iolist_to_binary(Bin1),

    {ok, Bin2} = template_compiler:render("include_dynamic.tpl", #{ t => "include_b_no_trailing_newline.tpl" }, [], undefined),
    <<"abc">> = iolist_to_binary(Bin2),

    {ok, Bin3} = template_compiler:render("include_dynamic.tpl", #{ t => "include_b_trailing_dos_newline.tpl" }, [], undefined),
    <<"abc">> = iolist_to_binary(Bin3),

    ok.

include_args_test(_Config) ->
    {ok, Bin1} = template_compiler:render("include_args.tpl", #{ a => 1, b => 2 }, [], undefined),
    <<"a3:2:truec">> = iolist_to_binary(Bin1),
    ok.

include_undefined_test(_Config) ->
    {ok, Bin1} = template_compiler:render("include_undefined.tpl", #{ template => undefined }, [], undefined),
    <<"ac">> = iolist_to_binary(Bin1),
    ok.

compose_test(_Config) ->
    {ok, Bin1} = template_compiler:render("compose.tpl", #{}, [], undefined),
    <<"AxB1yC">> = iolist_to_binary(Bin1),
    ok.

compose_inherit_test(_Config) ->
    {ok, Bin1} = template_compiler:render("compose2.tpl", #{}, [], undefined),
    <<"AxBXYC1yD">> = iolist_to_binary(Bin1),
    ok.

debug_points_metadata_test(_Config) ->
    {ok, #template_file{ filename = Filename }} = template_compiler_runtime:map_template(<<"debug_value.tpl">>, [], undefined),
    {ok, Mod1} = template_compiler:lookup(Filename, [], undefined),
    false = Mod1:is_debug_compiled(),
    [Point|_] = Mod1:debug_points(),

    {ok, Mod2} = template_compiler:lookup(Filename, [{debug_points, [Point]}], undefined),
    true = (Mod1 =:= Mod2),
    true = Mod2:is_debug_compiled(),
    true = (Mod1:debug_points() =:= Mod2:debug_points()),
    ok.

debug_points_runtime_test(_Config) ->
    Context = #{ trace_pid => self() },
    {ok, #template_file{ filename = Filename }} = template_compiler_runtime:map_template(<<"debug_value.tpl">>, [], undefined),
    {ok, Mod0} = template_compiler:lookup(Filename, [], Context),
    [Point|_] = Mod0:debug_points(),
    Options0 = [
        {runtime, template_compiler_debug_runtime},
        {debug_points, [Point]}
    ],
    {ok, Mod} = template_compiler:lookup(Filename, Options0, Context),
    [Point|_] = Mod:debug_points(),

    {ok, Bin1} = template_compiler:render("debug_value.tpl", #{ a => 1 }, Options0, Context),
    <<"1">> = iolist_to_binary(Bin1),
    receive
        {trace_debug, _, Vars1} ->
            false = maps:is_key(debug_vars, Vars1)
    after 50 ->
        ok
    end,

    {ok, Bin2} = template_compiler:render("debug_value.tpl", #{ a => 2 }, Options0, Context),
    <<"2">> = iolist_to_binary(Bin2),
    {TracePos, DebugVars} = receive_debug_trace(Filename, 1000),
    2 = maps:get(a, DebugVars),
    false = maps:is_key('$template', DebugVars),
    false = maps:is_key('$line', DebugVars),
    false = maps:is_key('$column', DebugVars),
    Point = TracePos,
    ok.

flush_debug_test(_Config) ->
    {ok, #template_file{ filename = Filename }} = template_compiler_runtime:map_template(<<"debug_value.tpl">>, [], undefined),
    {ok, Mod0} = template_compiler:lookup(Filename, [], undefined),
    [Point|_] = Mod0:debug_points(),
    {ok, Mod1} = template_compiler:lookup(Filename, [{debug_points, [Point]}], undefined),
    true = Mod1:is_debug_compiled(),
    ok = template_compiler:flush_debug(),
    {ok, Mod2} = template_compiler:lookup(Filename, [], undefined),
    false = Mod2:is_debug_compiled(),
    true = (Mod1 =:= Mod2),
    ok.

receive_debug_trace(Filename, Timeout) ->
    receive
        {trace_debug, {Filename, _, _} = SrcPos, Vars} ->
            {SrcPos, Vars}
    after Timeout ->
        ct:fail(no_debug_checkpoint)
    end.

test_data_dir(Config) ->
    filename:join([
        filename:dirname(filename:dirname(?config(data_dir, Config))),
        "test-data"]).
