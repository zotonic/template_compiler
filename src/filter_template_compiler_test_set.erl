%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022-2023 Marc Worrell
%% @doc Filter used in testing the template compiler. Sets a process dict value
%% to trace execution or expressions.
%% @end

%% Copyright 2022-2023 Marc Worrell
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

-module(filter_template_compiler_test_set).

-export([
    template_compiler_test_set/4
    ]).

template_compiler_test_set(Expr, Var, Value, _Context) ->
    erlang:put(Var, Value),
    Expr.
