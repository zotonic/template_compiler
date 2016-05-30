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
    find_template/3,
    context_name/1,
    find_nested_value/3,
    find_value/4,
    get_translations/2,
    lookup_translation/3,
    to_bool/2,
    to_list/2,
    to_iolist/3
    ]).

-callback find_nested_value(Keys :: list(), TplVars :: term(), Context :: term()) -> term().
-callback find_value(Key :: term(), Vars :: term(), TplVars :: #{}, Context :: term()) -> term().

-callback get_translations(Text :: binary(), Context :: term()) -> binary() | {trans, [{atom(), binary()}]}.
-callback lookup_translation({trans, list({atom(), binary()})}, TplVars :: #{}, Context :: term()) -> binary().

-callback find_template(binary(), ContextName::term(), Vars::#{}, Context::term()) ->
                {ok, filename:filename()} | {error, notfound|term()}.

-callback context_name(Context :: term()) -> term().

-callback to_bool(Value :: term(), Context :: term()) -> boolean().
-callback to_list(Value :: term(), Context :: term()) -> list().
-callback to_iolist(Value :: term(), TplVars :: #{}, Context :: term()) -> iolist().



%% @doc Map a template name to a template file.
-spec find_template(binary(), ContextName::term(), Context::term()) ->
            {ok, filename:filename()} | {error, notfound|term()}.
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
        undefind ->
            {error, notfound}
    end.

%% @doc Fetch the name to tag template lookups. This should make the name of the template unique with
%%      respect to the mapping of a template to a filename.  Example: {site, ua_class}
-spec context_name(Context::term()) -> term().
context_name(_Context) ->
    default.


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

%% @doc Convert a value to a boolean.
-spec to_bool(Value :: term(), Context :: term()) -> boolean().
to_bool(Value, _Context) ->
    z_convert:to_bool(Value).

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
to_list(Value, _Context) ->
    z_convert:to_list(Value).


%% @doc Convert a value to an iolist, used for converting values in {{ ... }} expressions.
-spec to_iolist(Value::term(), TplVars:: #{}, Context::term()) -> iolist().
to_iolist(undefined, _TplVars, _Context) -> <<>>;
to_iolist(B, _TplVars, _Context) when is_binary(B) -> 
    B;
to_iolist(A, _TplVars, _Context) when is_atom(A) -> 
    atom_to_binary(A, 'utf8');
to_iolist(N, _TplVars, _Context) when is_integer(N) -> 
    integer_to_binary(N);
to_iolist(F, _TplVars, _Context) when is_float(F) -> 
    io_lib:format("~p", [F]);
to_iolist({{Y,M,D},{H,I,S}} = Date, TplVars, _Context)
    when is_integer(Y), is_integer(M), is_integer(D),
         is_integer(H), is_integer(I), is_integer(S) ->
    Options = [
        {tz, maps:get(tz, TplVars, "GMT")}
    ],
    z_dateformat:format(Date, "Y-m-d H:i:s", Options);
to_iolist(T, _TplVars, _Context) when is_tuple(T) -> 
    io_lib:format("~p", [T]);
to_iolist(L, TplVars, Context) when is_list(L) ->
    try
        unicode:characters_to_binary(L)
    catch
        error:badarg ->
            [ to_iolist(C, TplVars, Context) || C <- L ]
    end.
