%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2016 Marc Worrell
%% @doc Operators for expression evaluation in templates

%% Copyright 2010-2016 Marc Worrell
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

-module(template_compiler_operators).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    'and'/4,
    'not'/3,
    'or'/4,
    'xor'/4,

    concat/4,
    subtract/4,

    add/4,
    sub/4,
    divide/4,
    multiply/4,
    modulo/4,

    negate/3,
    
    ge/4,
    le/4,
    gt/4,
    lt/4,
    eq/4,
    ne/4
]).


'and'(A, B, Runtime, Context) ->
    Runtime:to_bool(A, Context) andalso Runtime:to_bool(B, Context).

'not'(A, Runtime, Context) ->
    not Runtime:to_bool(A, Context).

'or'(A, B, Runtime, Context) ->
    Runtime:to_bool(A, Context) orelse Runtime:to_bool(B, Context).

'xor'(A, B, Runtime, Context) ->
    Runtime:to_bool(A, Context) xor Runtime:to_bool(B, Context).


concat(A, undefined, _Runtime, _Context) when is_binary(A) ->
    A;
concat(undefined, B, _Runtime, _Context) when is_binary(B) ->
    B;
concat(A, undefined, Runtime, Context) ->
    Runtime:to_list(A, Context);
concat(undefined, B, Runtime, Context) ->
    Runtime:to_list(B, Context);
concat(A, B, _Runtime, _Context) when is_list(A), is_list(B) ->
    A++B;
concat(A, B, _Runtime, Context) when is_binary(A), is_binary(B) ->
    ABin = z_convert:to_binary(A, Context),
    BBin = z_convert:to_binary(B, Context),
    <<ABin/binary, BBin/binary>>;
concat(A, B, Runtime, Context) ->
    concat(Runtime:to_list(A, Context), Runtime:to_list(B, Context), Runtime, Context).

subtract(A, undefined, _Runtime, _Context) ->
    A;
subtract(undefined, _, _Runtime, _Context) ->
    undefined;
subtract(A, B, _Runtime, _Context) when is_list(A), is_list(B) ->
    A--B;
subtract(A, B, Runtime, Context) ->
    subtract(Runtime:to_list(A, Context), Runtime:to_list(B, Context), Runtime, Context).


add(A, B, _Runtime, _Context) ->
    case to_numbers(A, B) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A1, B1} -> A1 + B1
    end.

sub(A, B, _Runtime, _Context) ->
    case to_numbers(A, B) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A1, B1} -> A1 - B1
    end.

divide(A, B, _Runtime, _Context) ->
    case to_numbers(A, B) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {_, 0} -> undefined;
        {_, 0.0} -> undefined;
        {A1, B1} -> A1 / B1
    end.

multiply(A, B, _Runtime, _Context) ->
    case to_numbers(A, B) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A1, B1} -> A1 * B1
    end.

modulo(A, B, _Runtime, _Context) ->
    case to_numbers(A, B) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {_, 0} -> undefined;
        {_, 0.0} -> undefined;
        {A1, B1} -> A1 rem B1
    end.


negate(undefined, _Runtime, _Context) ->
    undefined;
negate(A, _Runtime, _Context) when is_number(A) ->
    0 - A;
negate(A, _Runtime, _Context) ->
    0 - z_convert:to_integer(A).


ge(Input, Value, _Runtime, _Context) ->
    case to_values(Input, Value) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A >= B
    end.

le(Input, Value, _Runtime, _Context) ->
    case to_values(Input, Value) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A =< B
    end.

gt(Input, Value, _Runtime, _Context) ->
    case to_values(Input, Value) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A > B
    end.

lt(Input, Value, _Runtime, _Context) ->
    case to_values(Input, Value) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A < B
    end.

eq(Input, Value, _Runtime, _Context) ->
    {A, B} = to_values(Input, Value),
    A == B.

ne(Input, Value, _Runtime, _Context) ->
    {A, B} = to_values(Input, Value),
    A /= B.


%% @doc Convert the two parameters to compatible values
to_values(undefined, B) ->
    {undefined, B};
to_values(A, undefined) ->
    {A, undefined};
to_values(A, B) when is_number(A), is_number(B) -> 
    {A,B};
to_values(A, B) when is_boolean(A); is_boolean(B) -> 
    {z_convert:to_bool(A), z_convert:to_bool(B)};
to_values(A, B) when is_integer(A); is_integer(B) -> 
    {z_convert:to_integer(A), z_convert:to_integer(B)};
to_values(A, B) when is_float(A); is_float(B) -> 
    {z_convert:to_float(A), z_convert:to_float(B)};
to_values(A, B) when is_binary(A), is_binary(B) -> 
    {A,B};
to_values(A, B) when is_tuple(A), is_tuple(B) -> 
    {A, B};
to_values(A, B) -> 
    {z_convert:to_list(A), z_convert:to_list(B)}.


%% @doc Convert the two parameters to compatible numerical values
to_numbers(undefined, B) ->
    {undefined, B};
to_numbers(A, undefined) ->
    {A, undefined};
to_numbers(A, B) when is_number(A), is_number(B) -> 
    {A,B};
to_numbers(A, B) when is_integer(A); is_integer(B) -> 
    {z_convert:to_integer(A), z_convert:to_integer(B)};
to_numbers(A, B) when is_float(A); is_float(B) -> 
    {z_convert:to_float(A), z_convert:to_float(B)};
to_numbers(A, B) -> 
    {z_convert:to_integer(A), z_convert:to_integer(B)}.
