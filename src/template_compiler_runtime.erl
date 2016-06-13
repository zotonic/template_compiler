%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Simple runtime for the compiled templates. Needs to be
%%      copied and adapted for different environments.

%% Copyright 2016 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(template_compiler_runtime).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    map_template/3,
    map_template_all/3,
    is_modified/3,
    compile_map_nested_value/3,
    find_nested_value/3,
    find_value/4,
    set_context_vars/2,
    get_translations/2,
    lookup_translation/3,
    custom_tag/4,
    builtin_tag/5,
    cache_tag/6,
    javascript_tag/3,
    spaceless_tag/3,
    to_bool/2,
    to_list/2,
    to_render_result/3,
    escape/2,
    trace_compile/4,
    trace_render/3,
    trace_block/4
    ]).


-callback map_template(template_compiler:template(), #{}, term()) -> 
        {ok, template_compiler:template_file()} | {error, enoent|term()}.
-callback map_template_all(template_compiler:template(), #{}, term()) -> [template_compiler:template_file()].

-callback is_modified(filename:filename(), calendar:datetime(), term()) -> boolean().

-callback compile_map_nested_value(Tokens :: list(), ContextVar::string(), Context :: term()) -> NewTokens :: list().
-callback find_nested_value(Keys :: list(), TplVars :: term(), Context :: term()) -> term().
-callback find_value(Key :: term(), Vars :: term(), TplVars :: #{}, Context :: term()) -> term().

-callback set_context_vars(#{}|[], Context::term()) -> Context::term().

-callback get_translations(Text :: binary(), Context :: term()) -> binary() | {trans, [{atom(), binary()}]}.
-callback lookup_translation({trans, list({atom(), binary()})}, TplVars :: #{}, Context :: term()) -> binary().

-callback custom_tag(Module::atom(), Args::list(), TplVars::#{}, Context::term()) -> template_compiler:render_result().
-callback builtin_tag(template_compiler:builtin_tag(), term(), Args::list(), TplVars::#{}, Context::term()) -> template_compiler:render_result().
-callback cache_tag(Seconds::integer(), Name::binary(), Args::list(), function(), TplVars::#{}, Context::term()) -> template_compiler:render_result().
-callback javascript_tag(template_compiler:render_result(), #{}, term()) -> template_compiler:render_result().
-callback spaceless_tag(template_compiler:render_result(), #{}, term()) -> template_compiler:render_result().

-callback to_bool(Value :: term(), Context :: term()) -> boolean().
-callback to_list(Value :: term(), Context :: term()) -> list().
-callback to_render_result(Value :: term(), TplVars :: #{}, Context :: term()) -> template_compiler:render_result().
-callback escape(iolist(), Context :: term()) -> iolist().

-callback trace_compile(atom(), binary(), template_compiler:options(), term()) -> ok.
-callback trace_render(binary(), template_compiler:options(), term()) -> ok.
-callback trace_block({binary(),integer(),integer()}, atom(), atom(), term()) -> ok | {ok, iolist(), iolist()}.


-include("template_compiler.hrl").

%% @doc Dynamic mapping of a template to a template name, context sensitive on the template vars.
-spec map_template(template_compiler:template(), #{}, Context::term()) -> 
        {ok, template_compiler:template_file()} | {error, enoent|term()}.
map_template(#template_file{} = TplFile, _Vars, _Context) ->
    {ok, TplFile};
map_template({cat, Template}, Vars, Context) ->
    map_template(Template, Vars, Context);
map_template({cat, Template, _}, Vars, Context) ->
    map_template(Template, Vars, Context);
map_template(Template, _Vars, _Context) ->
    case application:get_env(template_compiler, template_dir) of
        {ok, {App, SubDir}} when is_atom(App) ->
            case code:priv_dir(App) of
                {error, _} = Error ->
                    Error;
                PrivDir ->
                    {ok, #template_file{
                        template=Template, 
                        filename=filename:join([PrivDir, SubDir, Template])
                    }}
            end;
        {ok, Dir} when is_list(Dir); is_binary(Dir) ->
            {ok, #template_file{
                template=Template, 
                filename=filename:join([Dir, Template])
            }};
        undefined ->
            {error, enoent}
    end.

%% @doc Dynamically find all templates matching the template
-spec map_template_all(template_compiler:template(), #{}, Context::term()) -> [template_compiler:template_file()].
map_template_all(Template, Vars, Context) ->
    case map_template(Template, Vars, Context) of
        {ok, Tpl} -> [Tpl];
        {error, _} -> []
    end.

%% @doc Check if a file has been modified
-spec is_modified(filename:filename(), calendar:datetime(), term()) -> boolean().
is_modified(Filename, Mtime, _Context) ->
    filelib:last_modified(Filename) /= Mtime.

%% @doc Compile time mapping of nested value lookup
-spec compile_map_nested_value(Tokens :: list(), _ContextVar::string(), Context :: term()) -> NewTokens :: list().
compile_map_nested_value(Ts, _ContextVar, _Context) ->
    Ts.

%% @doc Find a list of values at once, easier and more efficient than a nested find_value/4
%%      Add pattern matching here for nested lookups.
find_nested_value([K|Ks], TplVars, Context) ->
    find_nested_value_1(find_value(K, TplVars, TplVars, Context), Ks, TplVars, Context).

find_nested_value_1(undefined, _Ks, _TplVars, _Context) ->
    undefined;
find_nested_value_1(V, [], _TplVars, _Context) ->
    V;
find_nested_value_1(V, [K|Ks], TplVars, Context) ->
    find_nested_value_1(find_value(K, V, TplVars, Context), Ks, TplVars, Context).


%% @doc Find the value of key in some structure.
-spec find_value(Key :: term(), Vars :: term(), TplVars :: #{}, Context :: term()) -> term().
find_value(undefined, _, _TplVars, _Context) ->
    undefined;
find_value(_, undefined, _TplVars, _Context) ->
    undefined;
find_value(Name, #{} = Vars, _TplVars, _Context) ->
    maps:get(Name, Vars, undefined);
find_value(Name, Vars, _TplVars, _Context) when is_atom(Name), is_list(Vars) ->
    proplists:get_value(Name, Vars);
find_value(Nr, Vars, _TplVars, _Context) when is_integer(Nr), is_list(Vars) ->
    try lists:nth(Nr, Vars)
    catch _:_ -> undefined
    end;
find_value(IsoAtom, {trans, Tr}, _TplVars, _Context) ->
    proplists:get_value(IsoAtom, Tr, <<>>);
find_value(Key, {obj, Props}, _TplVars, _Context) when is_list(Props) ->
    proplists:get_value(z_convert:to_list(Key), Props);
find_value(Key, {obj, Props}, _TplVars, _Context) when is_list(Props) ->
    proplists:get_value(z_convert:to_list(Key), Props);
find_value(Key, {struct, Props}, _TplVars, _Context) when is_list(Props) ->
    case proplists:get_value(z_convert:to_binary(Key), Props) of
        null -> undefined;
        V -> V
    end;
find_value(Key, [{B,_}|_] = L, _TplVars, _Context) when is_list(B) ->
    proplists:get_value(z_convert:to_list(Key), L);
find_value(Key, [{B,_}|_] = L, _TplVars, _Context) when is_binary(B) ->
    proplists:get_value(z_convert:to_binary(Key), L);
find_value(Key, Tuple, _TplVars, _Context) when is_tuple(Tuple) ->
    case element(1, Tuple) of
        dict -> 
            case dict:find(Key, Tuple) of
                {ok, Val} -> Val;
                _ -> undefined
            end;
        _ when is_integer(Key) ->
            try element(Key, Tuple)
            catch _:_ -> undefined
            end;
        _ ->
            undefined
    end;
find_value(Key, F, _TplVars, _Context) when is_function(F, 1) ->
    F(Key);
find_value(Key, F, _TplVars, Context) when is_function(F, 2) ->
    F(Key, Context);
find_value(Key, F, TplVars, Context) when is_function(F, 3) ->
    F(Key, TplVars, Context);
find_value(_Key, _Vars, _TplVars, _Context) ->
    undefined.


%% @doc Set any contextual arguments from the map or argument list. User for sudo/anondo and language settings
-spec set_context_vars(#{}|[], term()) -> term().
set_context_vars(Args, Context) when is_map(Args); is_list(Args) ->
    Context.


%% @doc Fetch the translations for the given text.
-spec get_translations(binary(), term()) -> binary() | {trans, [{atom(), binary()}]}.
get_translations(Text, _Context) ->
    {trans, [{en, Text}]}.

%% @doc Find the best fitting translation.
-spec lookup_translation({trans, list({atom(), binary()})}, TplVars :: #{}, Context :: term()) -> binary().
lookup_translation({trans, Tr}, #{} = TplVars, _Context) ->
    Lang = maps:get(z_language, TplVars, en),
    case lists:keyfind(Lang, 1, Tr) of
        {Lang, Text} ->
            Text;
        false when Lang =/= en -> 
            case lists:keyfind(en, 1, Tr) of
                {Lang, Text} -> Text;
                false -> <<>>
            end;
        false ->
            <<>>
    end.


%% @doc Render a custom tag (Zotonic scomp) - this can be changed to more complex runtime lookups.
-spec custom_tag(Tag::atom(), Args::list(), Vars::#{}, Context::term()) -> template_compiler:render_result().
custom_tag(Tag, Args, Vars, Context) ->
    Tag:render(Args, Vars, Context).


%% @doc Render image/image_url/media/url/lib tag. The Expr is the media item or dispatch rule.
-spec builtin_tag(template_compiler:builtin_tag(), Expr::term(), Args::list(), Vars::#{}, Context::term()) -> 
            template_compiler:render_result().
builtin_tag(_Tag, _Expr, _Args, _Vars, _Context) ->
    <<>>.


%% @doc Render a block, cache the result for some time. Caching should be implemented by the runtime.
-spec cache_tag(Seconds::integer(), Name::binary(), Args::list(), function(), TplVars::#{}, Context::term()) -> template_compiler:render_result().
cache_tag(_Seconds, _Name, Args, Fun, TplVars, Context) ->
    FunVars = lists:foldl(
                    fun({K,V}, Acc) ->
                        Acc#{K => V}
                    end,
                    TplVars,
                    Args),
    Fun(FunVars, Context).


%% @doc Render a script block, for Zotonic this is added to the scripts in the Context
-spec javascript_tag(template_compiler:render_result(), #{}, term()) -> template_compiler:render_result().
javascript_tag(_Javascript, _TplVars, _Context) ->
    <<>>.

%% @doc Remove spaces between HTML tags
-spec spaceless_tag(template_compiler:render_result(), #{}, term()) -> template_compiler:render_result().
spaceless_tag(Value, _TplVars, _Context) ->
    Contents1 = re:replace(iolist_to_binary(Value), "^[ \t\n\f\r]+<", "<"),
    Contents2 = re:replace(Contents1, ">[ \t\n\f\r]+$", ">"),
    re:replace(Contents2, ">[ \t\n\f\r]+<", "><", [global]).


%% @doc Convert a value to a boolean.
-spec to_bool(Value :: term(), Context :: term()) -> boolean().
to_bool(Value, _Context) ->
    z_convert:to_bool_strict(Value).

%% @doc Convert a value to a list.
-spec to_list(Value :: term(), Context :: term()) -> list().
to_list(undefined, _Context) ->
    [];
to_list(#{} = Map, _Context) ->
    maps:to_list(Map);
to_list({trans, Tr}, _Context) when is_list(Tr) ->
    Tr;
to_list(Tuple, _Context) when is_tuple(Tuple) ->
    tuple_to_list(Tuple);
to_list(B, _Context) when is_binary(B) ->
    [B];
to_list(Value, _Context) ->
    z_convert:to_list(Value).


%% @doc Convert a value to an render_result, used for converting values in {{ ... }} expressions.
-spec to_render_result(Value::term(), TplVars:: #{}, Context::term()) -> template_compiler:render_result().
to_render_result(undefined, _TplVars, _Context) -> <<>>;
to_render_result(B, _TplVars, _Context) when is_binary(B) -> 
    B;
to_render_result(A, _TplVars, _Context) when is_atom(A) -> 
    atom_to_binary(A, 'utf8');
to_render_result(N, _TplVars, _Context) when is_integer(N) -> 
    integer_to_binary(N);
to_render_result(F, _TplVars, _Context) when is_float(F) -> 
    io_lib:format("~p", [F]);
to_render_result({{Y,M,D},{H,I,S}} = Date, TplVars, _Context)
    when is_integer(Y), is_integer(M), is_integer(D),
         is_integer(H), is_integer(I), is_integer(S) ->
    Options = [
        {tz, maps:get(tz, TplVars, "GMT")}
    ],
    z_dateformat:format(Date, "Y-m-d H:i:s", Options);
to_render_result(T, _TplVars, _Context) when is_tuple(T) -> 
    io_lib:format("~p", [T]);
to_render_result(L, TplVars, Context) when is_list(L) ->
    try
        unicode:characters_to_binary(L)
    catch
        error:badarg ->
            [ to_render_result(C, TplVars, Context) || C <- L ]
    end.

%% @doc HTML escape a value
-spec escape(Value :: iolist(), Context :: term()) -> iolist().
escape(Value, _Context) ->
    z_html:escape(iolist_to_binary(Value)).


%% @doc Called when compiling a module
-spec trace_compile(atom(), binary(), template_compiler:options(), term()) -> ok.
trace_compile(_Module, Filename, Options, _Context) ->
    case proplists:get_value(trace_position, Options) of
        {File, Line, _Col} ->
            lager:debug("[template_compiler] Compiling \"~s\" (called from \"~s:~p\")",
                        [Filename, File, Line]);
        undefined ->
            lager:debug("[template_compiler] Compiling \"~s\"",
                        [Filename])
    end,
    ok.

%% @block Called when a template is rendered (could be from an include)
-spec trace_render(binary(), template_compiler:options(), term()) -> ok | {ok, iolist(), iolist()}.
trace_render(Filename, Options, _Context) ->
    case proplists:get_value(trace_position, Options) of
        {File, Line, _Col} ->
            lager:debug("[template_compiler] Include by \"~s:~p\" of \"~s\"",
                        [File, Line, Filename]);
        undefined ->
            lager:debug("[template_compiler] Render \"~s\"",
                        [Filename])
    end,
    ok.

%% @block Called when a block function is called
-spec trace_block({binary(), integer(), integer()}, atom(), atom(), term()) -> ok | {ok, iolist(), iolist()}.
trace_block(_SrcPos, _Name, _Module, _Context) ->
    ok.

