%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2023 Marc Worrell
%% @doc Misc support routines.
%% @end

%% Copyright 2016-2023 Marc Worrell
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

-module(template_compiler_utils).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    var/1,
    next_vars_var/2,
    next_context_var/2,
    to_atom/1,
    unescape_string_literal/1,
    file_mtime/1,
    pos/1,
    set_pos/2
    ]).

-include("template_compiler.hrl").
-include("template_compiler_internal.hrl").
-include_lib("kernel/include/file.hrl").


%% @doc Generate an unique variable name
-spec var(#ws{}) -> {#ws{}, string()}.
var(#ws{nr=Nr} = Ws) ->
    {Ws#ws{nr=Nr+1}, "V"++integer_to_list(Nr)}.

%% @doc Set the next "Vars" variable name in cs for an enclosed block.
-spec next_vars_var(#cs{}, #ws{}) -> {#cs{}, #ws{}}.
next_vars_var(CState, #ws{nr=Nr} = Ws) ->
    Vars = "Vars"++integer_to_list(Nr+1),
    {CState#cs{vars_var=Vars}, Ws#ws{nr=Nr+1}}.


%% @doc Set the next "Context" variable name in cs for an enclosed block.
-spec next_context_var(#cs{}, #ws{}) -> {#cs{}, #ws{}}.
next_context_var(CState, #ws{nr=Nr} = Ws) ->
    CtxVar = "Context"++integer_to_list(Nr+1),
    {CState#cs{context_var=CtxVar}, Ws#ws{nr=Nr+1}}.


%% @doc Convert a list or binary to an atom.
-spec to_atom(string()|binary()|atom()) -> atom().
to_atom(L) when is_list(L) -> erlang:list_to_atom(L);
to_atom(B) when is_binary(B) -> erlang:binary_to_atom(B, 'utf8');
to_atom(A) when is_atom(A) -> A.


%% @doc Expand escape sequences like \n and \t in a string.
-spec unescape_string_literal(binary()) -> binary().
unescape_string_literal(String) ->
    unescape_string_literal_1(String, <<>>).

unescape_string_literal_1(<<>>, Acc) ->
    Acc;
unescape_string_literal_1(<<$\\, $n, Rest/binary>>, Acc) ->
    unescape_string_literal_1(Rest, <<Acc/binary, $\n>>);
unescape_string_literal_1(<<$\\, $r, Rest/binary>>, Acc) ->
    unescape_string_literal_1(Rest, <<Acc/binary, $\r>>);
unescape_string_literal_1(<<$\\, $t, Rest/binary>>, Acc) ->
    unescape_string_literal_1(Rest, <<Acc/binary, $\t>>);
unescape_string_literal_1(<<$\\, C/utf8, Rest/binary>>, Acc) ->
    unescape_string_literal_1(Rest, <<Acc/binary, C/utf8>>);
unescape_string_literal_1(<<C/utf8, Rest/binary>>, Acc) ->
    unescape_string_literal_1(Rest, <<Acc/binary, C/utf8>>).


%% @doc Return the (universal) modification time of file, 0 on enoent
-spec file_mtime(file:filename_all()) -> calendar:datetime() | 0.
file_mtime(File) ->
    case file:read_file_info(File, [{time, universal}]) of
        {ok, #file_info{mtime=MTime}} -> MTime;
        {error, enoent} -> 0;
        {error, _} -> 0
    end.

-spec set_pos({file:filename_all(), pos_integer(), pos_integer()}, term()) -> term().
set_pos(SrcPos, Tree) ->
    erl_syntax:set_pos(Tree, pos(SrcPos)).

-spec pos({file:filename_all(), pos_integer(), pos_integer()}) -> {pos_integer(), pos_integer()}.
pos({_File, Line, Column}) ->
    {Line, Column}.
