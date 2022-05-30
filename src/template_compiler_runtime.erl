%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2020 Marc Worrell
%% @doc Simple runtime for the compiled templates. Needs to be
%%      copied and adapted for different environments.

%% Copyright 2016-2020 Marc Worrell
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
    find_nested_value/4,
    find_value/4,
    get_context_name/1,
    set_context_vars/2,
    get_translations/2,
    lookup_translation/3,
    model_call/4,
    custom_tag/4,
    builtin_tag/5,
    cache_tag/6,
    javascript_tag/3,
    spaceless_tag/3,
    to_bool/2,
    to_list/2,
    to_simple_value/2,
    to_render_result/3,
    escape/2,
    trace_compile/4,
    trace_render/3,
    trace_block/4
    ]).


-callback map_template(template_compiler:template(), map(), term()) ->
        {ok, template_compiler:template_file()} | {error, enoent|term()}.
-callback map_template_all(template_compiler:template(), map(), term()) -> [template_compiler:template_file()].

-callback is_modified(file:filename_all(), calendar:datetime(), term()) -> boolean().

-callback compile_map_nested_value(Tokens :: list(), ContextVar::string(), Context :: term()) -> NewTokens :: list().
-callback find_nested_value(Keys :: list(), TplVars :: term(), Context :: term()) -> term().
-callback find_nested_value(BaseValue :: term(), Keys :: list(), TplVars :: term(), Context :: term()) -> term().
-callback find_value(Key :: term(), Vars :: term(), TplVars :: map(), Context :: term()) -> term().

-callback get_context_name(Context::term()) -> atom().
-callback set_context_vars(map()|list(), Context::term()) -> Context::term().

-callback get_translations(Text :: binary(), Context :: term()) -> binary() | {trans, [{atom(), binary()}]}.
-callback lookup_translation({trans, list({atom(), binary()})}, TplVars :: map(), Context :: term()) -> binary().

-callback model_call(Model::atom(), Path::list(), Payload::term(), Context::term()) -> template_compiler:model_return().
-callback custom_tag(Module::atom(), Args::list(), TplVars::map(), Context::term()) -> template_compiler:render_result().
-callback builtin_tag(template_compiler:builtin_tag(), term(), Args::list(), TplVars::map(), Context::term()) -> template_compiler:render_result().
-callback cache_tag(Seconds::integer(), Name::binary(), Args::list(), function(), TplVars::map(), Context::term()) -> template_compiler:render_result().
-callback javascript_tag(template_compiler:render_result(), map(), term()) -> template_compiler:render_result().
-callback spaceless_tag(template_compiler:render_result(), map(), term()) -> template_compiler:render_result().

-callback to_bool(Value :: term(), Context :: term()) -> boolean().
-callback to_list(Value :: term(), Context :: term()) -> list().
-callback to_simple_value(Value :: term(), Context :: term()) -> term().
-callback to_render_result(Value :: term(), TplVars :: map(), Context :: term()) -> template_compiler:render_result().
-callback escape(iodata() | undefined, Context :: term()) -> iodata().

-callback trace_compile(atom(), binary(), template_compiler:options(), term()) -> ok.
-callback trace_render(binary(), template_compiler:options(), term()) -> ok | {ok, iodata(), iodata()}.
-callback trace_block({binary(),integer(),integer()}, atom(), atom(), term()) -> ok | {ok, iodata(), iodata()}.


-include_lib("kernel/include/logger.hrl").
-include("template_compiler.hrl").

%% @doc Dynamic mapping of a template to a template name, context sensitive on the template vars.
-spec map_template(template_compiler:template(), map(), Context::term()) ->
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
-spec map_template_all(template_compiler:template(), map(), Context::term()) -> [template_compiler:template_file()].
map_template_all(Template, Vars, Context) ->
    case map_template(Template, Vars, Context) of
        {ok, Tpl} -> [Tpl];
        {error, _} -> []
    end.

%% @doc Check if a file has been modified
-spec is_modified(file:filename_all(), calendar:datetime(), term()) -> boolean().
is_modified(Filename, Mtime, _Context) ->
    template_compiler_utils:file_mtime(Filename) /= Mtime.

%% @doc Compile time mapping of nested value lookup
-spec compile_map_nested_value(Tokens :: list(), _ContextVar::string(), Context :: term()) -> NewTokens :: list().
compile_map_nested_value(Ts, _ContextVar, _Context) ->
    Ts.

%% @doc Find a list of values at once, easier and more efficient than a nested find_value/4
%%      Add pattern matching here for nested lookups.
find_nested_value([K|Ks], TplVars, Context) ->
    find_nested_value(find_value(K, TplVars, TplVars, Context), Ks, TplVars, Context).

find_nested_value(undefined, _Ks, _TplVars, _Context) ->
    undefined;
find_nested_value(V, [], _TplVars, _Context) ->
    V;
find_nested_value(V, [K|Ks], TplVars, Context) ->
    find_nested_value(find_value(K, V, TplVars, Context), Ks, TplVars, Context).


%% @doc Find the value of key in some structure.
-spec find_value(Key :: term(), Vars :: term(), TplVars :: map(), Context :: term()) -> term().
find_value(undefined, _, _TplVars, _Context) ->
    undefined;
find_value(_, undefined, _TplVars, _Context) ->
    undefined;
find_value(Name, Vars, _TplVars, _Context) when is_map(Vars) ->
    case maps:find(Name, Vars) of
        {ok, V} -> V;
        error when is_atom(Name) ->
            % Maybe keys are binary
            maps:get(atom_to_binary(Name, utf8), Vars, undefined);
        error when is_binary(Name) ->
            % Maybe keys are atoms
            try
                Name1 = binary_to_existing_atom(Name, utf8),
                maps:get(Name1, Vars, undefined)
            catch _:_ -> undefined
            end;
        error ->
            undefined
    end;
find_value(Name, [ V | _ ] = Vars, _TplVars, _Context) when is_binary(Name), is_atom(V) ->
    try
        Atom = binary_to_existing_atom(Name, utf8),
        proplists:get_value(Atom, Vars)
    catch _:_ -> undefined
    end;
find_value(Key, [{B,_}|_] = L, _TplVars, _Context) when is_list(B) ->
    proplists:get_value(z_convert:to_list(Key), L);
find_value(Key, [{B,_}|_] = L, _TplVars, _Context) when is_binary(B) ->
    proplists:get_value(z_convert:to_binary(Key), L);
find_value(Name, Vars, _TplVars, _Context) when is_atom(Name), is_list(Vars) ->
    proplists:get_value(Name, Vars);
find_value(Name, [{A,_}|_] = L, _TplVars, _Context) when is_atom(A), is_binary(Name) ->
    try
        Name1 = binary_to_existing_atom(Name, utf8),
        proplists:get_value(Name1, L)
    catch _:_ -> undefined
    end;
find_value(Nr, Vars, _TplVars, _Context) when is_integer(Nr), is_list(Vars) ->
    try lists:nth(Nr, Vars)
    catch _:_ -> undefined
    end;
find_value(SomeKey, [ {_, _} | _ ] = List, _TplVars, _Context) ->
    proplists:get_value(SomeKey, List);
find_value(IsoAtom, {trans, Tr}, _TplVars, _Context) when is_atom(IsoAtom) ->
    proplists:get_value(IsoAtom, Tr, <<>>);
find_value(Iso, {trans, Tr}, _TplVars, _Context) when is_binary(Iso) ->
    try
        IsoAtom = binary_to_existing_atom(Iso, utf8),
        proplists:get_value(IsoAtom, Tr, <<>>)
    catch _:_ -> <<>>
    end;
find_value(Key, {obj, Props}, _TplVars, _Context) when is_list(Props) ->
    proplists:get_value(z_convert:to_list(Key), Props);
find_value(Key, {obj, Props}, _TplVars, _Context) when is_list(Props) ->
    proplists:get_value(z_convert:to_list(Key), Props);
find_value(Key, {struct, Props}, _TplVars, _Context) when is_list(Props) ->
    case proplists:get_value(z_convert:to_binary(Key), Props) of
        null -> undefined;
        V -> V
    end;
find_value(Key, Tuple, _TplVars, _Context) when is_tuple(Tuple) ->
    case element(1, Tuple) of
        dict ->
            find_value_dict(Key, Tuple);
        _ when is_integer(Key) ->
            try element(Key, Tuple)
            catch _:_ -> undefined
            end;
        _ when is_binary(Key) ->
            try
                Key1 = binary_to_integer(Key),
                element(Key1, Tuple)
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


-dialyzer({nowarn_function, find_value_dict/2}).
-spec find_value_dict( term(), tuple() ) -> term().
find_value_dict( Key, Dict ) ->
    try
        case dict:find(Key, Dict) of
            {ok, Val} -> Val;
            _ -> undefined
        end
    catch _:_ -> undefined
    end.


%% @doc Set the context name for this context, used for flush or recompile all templates
%%      beloging to a certain context (like a single site).
-spec get_context_name( term() ) -> atom().
get_context_name(Context) when is_map(Context) ->
    maps:get(context_name, Context, undefined);
get_context_name(Context) when is_list(Context) ->
    proplists:get_value(context_name, Context, undefined);
get_context_name(Context) when is_atom(Context) ->
    Context;
get_context_name(_) ->
    undefined.


%% @doc Set any contextual arguments from the map or argument list. User for sudo/anondo and language settings
-spec set_context_vars(map()|list(), term()) -> term().
set_context_vars(Args, Context) when is_map(Args); is_list(Args) ->
    Context.


%% @doc Fetch the translations for the given text.
-spec get_translations(binary(), term()) -> binary() | {trans, [{atom(), binary()}]}.
get_translations(Text, _Context) ->
    {trans, [{en, Text}]}.

%% @doc Find the best fitting translation.
-spec lookup_translation({trans, list({atom(), binary()})}, TplVars :: map(), Context :: term()) -> binary().
lookup_translation({trans, Tr}, TplVars, _Context) when is_map(TplVars) ->
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

%% @doc A model call with optional payload. Compiled from m.model.path!payload
-spec model_call(Model::atom(), Path::list(), Payload::term(), Context::term()) -> template_compiler:model_return().
model_call(Model, Path, Payload, _Context) ->
    {ok, {io_lib:format("model:~p ~p :: ~p", [ Model, Path, Payload ]), []}}.

%% @doc Render a custom tag (Zotonic scomp) - this can be changed to more complex runtime lookups.
-spec custom_tag(Tag::atom(), Args::list(), Vars::map(), Context::term()) -> template_compiler:render_result().
custom_tag(Tag, Args, Vars, Context) ->
    Tag:render(Args, Vars, Context).


%% @doc Render image/image_url/image_data_url/media/url/lib/lib_url tag.
%%      The Expr is the media item or dispatch rule.
-spec builtin_tag(template_compiler:builtin_tag(), Expr::term(), Args::list(), Vars::map(), Context::term()) -> 
            template_compiler:render_result().
builtin_tag(_Tag, _Expr, _Args, _Vars, _Context) ->
    <<>>.


%% @doc Render a block, cache the result for some time. Caching should be implemented by the runtime.
-spec cache_tag(Seconds::integer(), Name::binary(), Args::list(), function(), TplVars::map(), Context::term()) -> template_compiler:render_result().
cache_tag(_Seconds, _Name, Args, Fun, TplVars, Context) ->
    FunVars = lists:foldl(
                    fun({K,V}, Acc) ->
                        Acc#{K => V}
                    end,
                    TplVars,
                    Args),
    Fun(FunVars, Context).


%% @doc Render a script block, for Zotonic this is added to the scripts in the Context
-spec javascript_tag(template_compiler:render_result(), map(), term()) -> template_compiler:render_result().
javascript_tag(_Javascript, _TplVars, _Context) ->
    <<>>.

%% @doc Remove spaces between HTML tags
-spec spaceless_tag(template_compiler:render_result(), map(), term()) -> template_compiler:render_result().
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
to_list(<<>>, _Context) ->
    [];
to_list(Map, _Context) when is_map(Map) ->
    maps:to_list(Map);
to_list({trans, Tr}, _Context) when is_list(Tr) ->
    Tr;
to_list(Tuple, _Context) when is_tuple(Tuple) ->
    tuple_to_list(Tuple);
to_list(B, _Context) when is_binary(B) ->
    [B];
to_list(Value, _Context) ->
    z_convert:to_list(Value).

%% @doc Convert a value to a more simpler value like binary, list, boolean.
-spec to_simple_value(Value :: term(), Context :: term()) -> term().
to_simple_value({trans, _} = Tr, Context) -> z_convert:to_binary(Tr, Context);
to_simple_value(T, _Context) -> T.

%% @doc Convert a value to an render_result, used for converting values in {{ ... }} expressions.
-spec to_render_result(Value::term(), TplVars::map(), Context::term()) -> template_compiler:render_result().
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
-spec escape(Value :: iodata() | undefined, Context :: term()) -> iodata().
escape(undefined, _Context) ->
    <<>>;
escape(Value, _Context) ->
    z_html:escape(iolist_to_binary(Value)).


%% @doc Called when compiling a module
-spec trace_compile(atom(), binary(), template_compiler:options(), term()) -> ok.
trace_compile(_Module, Filename, Options, _Context) ->
    case proplists:get_value(trace_position, Options) of
        {File, Line, _Col} ->
            ?LOG_DEBUG(#{
                text => <<"Compiling template">>,
                filename => Filename,
                at => File,
                line => Line
            });
        undefined ->
            ?LOG_DEBUG(#{
                text => <<"Compiling template">>,
                filename => Filename
            })
    end,
    ok.

%% @doc Called when a template is rendered (could be from an include) - the return is
%%      kept in a trace for displaying template extends recursion information.
-spec trace_render(binary(), template_compiler:options(), term()) -> ok | {ok, iodata(), iodata()}.
trace_render(Filename, Options, _Context) ->
    case proplists:get_value(trace_position, Options) of
        {File, Line, _Col} ->
            ?LOG_DEBUG(#{
                text => <<"Template include">>,
                filename => Filename,
                at => File,
                line => Line
            });
        undefined ->
            ?LOG_DEBUG(#{
                text => <<"Template render">>,
                filename => Filename
            })
    end,
    ok.

%% @doc Called when a block function is called
-spec trace_block({binary(), integer(), integer()}, atom(), atom(), term()) -> ok | {ok, iodata(), iodata()}.
trace_block(_SrcPos, _Name, _Module, _Context) ->
    ok.

