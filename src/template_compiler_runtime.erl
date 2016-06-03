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
    find_template/3,
    context_name/1,
    compile_map_nested_value/2,
    find_nested_value/3,
    find_value/4,
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
    escape/2
    ]).


-callback map_template(template_compiler:template(), #{}, term()) -> template_compiler:template1().
-callback map_template_all(template_compiler:template(), #{}, term()) -> [template_compiler:template1()].
-callback find_template(template_compiler:template(), ContextName::term(), Context::term()) -> 
        {ok, filename:filename()} | {error, enoent|term()}.
-callback context_name(Context :: term()) -> term().

-callback compile_map_nested_value(Tokens :: list(), Context :: term()) -> NewTokens :: list().
-callback find_nested_value(Keys :: list(), TplVars :: term(), Context :: term()) -> term().
-callback find_value(Key :: term(), Vars :: term(), TplVars :: #{}, Context :: term()) -> term().

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


%% @doc Dynamic mapping of a template to a template name, context sensitive on the template vars.
-spec map_template(template_compiler:template(), #{}, Context::term()) -> template_compiler:template1().
map_template({cat, Template}, _Vars, _Context) ->
    Template;
map_template({cat, Template, _}, _Vars, _Context) ->
    Template;
map_template(Template, _Vars, _Context) ->
    Template.

%% @doc Dynamically find all templates matching the template
-spec map_template_all(template_compiler:template(), #{}, Context::term()) -> [template_compiler:template1()].
map_template_all(Template, _Vars, _Context) ->
    [Template].

%% @doc Map a template name to a template file, this is a static mapping with respect to the context.
-spec find_template(binary(), ContextName::term(), Context::term()) ->
            {ok, filename:filename()} | {error, enoent|term()}.
find_template({filename, Filename}, _ContextName, _Context) ->
    {ok, Filename};
find_template(Template, _ContextName, _Context) ->
    case application:get_env(template_compiler, template_dir) of
        {ok, {App, SubDir}} when is_atom(App) ->
            case code:priv_dir(App) of
                {error, _} = Error ->
                    Error;
                PrivDir ->
                    {ok, filename:join([PrivDir, SubDir, Template])}
            end;
        {ok, Dir} when is_list(Dir), is_binary(Dir) ->
            {ok, filename:join([Dir, Template])};
        undefined ->
            {error, enoent}
    end.

%% @doc Fetch the name to tag template lookups. This should make the name of the template unique with
%%      respect to the mapping of a template to a filename.  Example: {site, ua_class}
-spec context_name(Context::term()) -> term().
context_name(_Context) ->
    default.


%% @doc Compile time mapping of nested value lookup
-spec compile_map_nested_value(Tokens :: list(), Context :: term()) -> NewTokens :: list().
compile_map_nested_value(Ts, _Context) ->
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

