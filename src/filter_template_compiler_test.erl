%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2023 Marc Worrell
%% @doc Filter used in testing the template compiler. Also
%%      an example on how to write filters.
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

-module(filter_template_compiler_test).

-export([
    template_compiler_test/2,
    template_compiler_test/3,
    template_compiler_test/4
    ]).

template_compiler_test(Value, _Context) when is_integer(Value) ->
    2*z_convert:to_integer(Value);
template_compiler_test(Value, _Context) ->
    2*z_convert:to_integer(iolist_to_binary(Value)).

template_compiler_test(A, <<"w">>, _Context) ->
    io_lib:format("~w", [A]);
template_compiler_test(A, B, _Context) ->
    [
        z_convert:to_binary(A),
        $:, z_convert:to_binary(B)
    ].

template_compiler_test(A, B, C, _Context) ->
    [
        z_convert:to_binary(A),
        $:, z_convert:to_binary(B),
        $:, z_convert:to_binary(C)
    ].
