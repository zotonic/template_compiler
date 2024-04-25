%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2023 Marc Worrell
%% @doc Operators for expression evaluation in templates
%% @end

%% Copyright 2010-2023 Marc Worrell
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
    'not'/3,
    'xor'/4,
    'and'/4,
    'or'/4,

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
    ne/4,
    seq/4,
    sne/4
]).

'not'(false, _Runtime, _Context) -> true;
'not'(true, _Runtime, _Context) -> false;
'not'(undefined, _Runtime, _Context) -> true;
'not'(A, Runtime, Context) ->
    not Runtime:to_bool(A, Context).

'xor'(A, A, _Runtime, _Context) -> false;
'xor'(A, B, Runtime, Context) ->
    A1 = Runtime:to_simple_value(A, Context),
    B1 = Runtime:to_simple_value(B, Context),
    Runtime:to_bool(A1, Context) xor Runtime:to_bool(B1, Context).

% 'and' and 'or' are used by the expression compiler.
'and'(A, B, Runtime, Context) ->
    A1 = Runtime:to_simple_value(A, Context),
    B1 = Runtime:to_simple_value(B, Context),
    Runtime:to_bool(A1, Context) andalso Runtime:to_bool(B1, Context).

'or'(A, B, Runtime, Context) ->
    A1 = Runtime:to_simple_value(A, Context),
    B1 = Runtime:to_simple_value(B, Context),
    Runtime:to_bool(A1, Context) orelse Runtime:to_bool(B1, Context).


concat(A, undefined, _Runtime, _Context) when is_binary(A) -> A;
concat(undefined, B, _Runtime, _Context) when is_binary(B) -> B;
concat({trans, _} = Tr, B, Runtime, Context) ->
    concat(Runtime:to_simple_value(Tr, Context), B, Runtime, Context);
concat(A, {trans, _} = Tr, Runtime, Context) ->
    concat(A, Runtime:to_simple_value(Tr, Context), Runtime, Context);
concat(A, undefined, Runtime, Context) ->
    to_maybe_list(A, Runtime, Context);
concat(undefined, B, Runtime, Context) ->
    to_maybe_list(B, Runtime, Context);
concat(A, B, _Runtime, _Context) when is_list(A), is_list(B) ->
    A++B;
concat(A, B, _Runtime, _Context) when is_binary(A), is_binary(B) ->
    <<A/binary, B/binary>>;
concat(A, B, Runtime, Context) when is_binary(A); is_binary(B) ->
    concat(
        to_maybe_binary(A, Runtime, Context),
        to_maybe_binary(B, Runtime, Context),
        Runtime, Context);
concat(A, B, Runtime, Context) ->
    concat(
        to_maybe_list(A, Runtime, Context),
        to_maybe_list(B, Runtime, Context),
        Runtime, Context).

subtract(A, undefined, _Runtime, _Context) ->
    A;
subtract(undefined, _, _Runtime, _Context) ->
    undefined;
subtract(A, B, _Runtime, _Context) when is_list(A), is_list(B) ->
    A--B;
subtract(A, B, Runtime, Context) ->
    subtract(
        to_maybe_list(A, Runtime, Context),
        to_maybe_list(B, Runtime, Context),
        Runtime, Context).


add(A, B, Runtime, Context) ->
    case to_numbers(A, B, Runtime, Context) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A1, B1} -> A1 + B1
    end.

sub(A, B, Runtime, Context) ->
    case to_numbers(A, B, Runtime, Context) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A1, B1} -> A1 - B1
    end.

divide(A, B, Runtime, Context) ->
    case to_numbers(A, B, Runtime, Context) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {_, 0} -> undefined;
        {_, 0.0} -> undefined;
        {A1, B1} -> A1 / B1
    end.

multiply(A, B, Runtime, Context) ->
    case to_numbers(A, B, Runtime, Context) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A1, B1} -> A1 * B1
    end.

modulo(A, B, Runtime, Context) ->
    case to_numbers(A, B, Runtime, Context) of
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
negate(A, Runtime, Context) ->
    negate(to_maybe_integer(A, Runtime, Context), Runtime, Context).

ge(Input, Value, Runtime, Context) ->
    case to_values(Input, Value, Runtime, Context) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A >= B
    end.

le(Input, Value, Runtime, Context) ->
    case to_values(Input, Value, Runtime, Context) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A =< B
    end.

gt(Input, Value, Runtime, Context) ->
    case to_values(Input, Value, Runtime, Context) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A > B
    end.

lt(Input, Value, Runtime, Context) ->
    case to_values(Input, Value, Runtime, Context) of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {A, B} -> A < B
    end.

eq(A, A, _Runtime, _Context) -> true;
eq(Input, Value, Runtime, Context) ->
    {A, B} = to_values(Input, Value, Runtime, Context),
    A == B.

ne(A, A, _Runtime, _Context) -> false;
ne(Input, Value, Runtime, Context) ->
    {A, B} = to_values(Input, Value, Runtime, Context),
    A /= B.

seq(A, A, _Runtime, _Context) -> true;
seq(Input, Value, Runtime, Context) ->
    A1 = Runtime:to_simple_value(Input, Context),
    B1 = Runtime:to_simple_value(Value, Context),
    A1 == B1.

sne(A, A, _Runtime, _Context) -> false;
sne(Input, Value, Runtime, Context) ->
    A1 = Runtime:to_simple_value(Input, Context),
    B1 = Runtime:to_simple_value(Value, Context),
    A1 /= B1.


%% @doc Convert the two parameters to compatible values
to_values(undefined, B, _Runtime, _Context) ->
    {undefined, B};
to_values(A, undefined, _Runtime, _Context) ->
    {A, undefined};
to_values({trans, _} = Tr, B, Runtime, Context) ->
    to_values(Runtime:to_simple_value(Tr, Context), B, Runtime, Context);
to_values(A, {trans, _} = Tr, Runtime, Context) ->
    to_values(A, Runtime:to_simple_value(Tr, Context), Runtime, Context);
to_values(A, B, _Runtime, _Context) when is_number(A), is_number(B) ->
    {A,B};
to_values(A, B, Runtime, Context) when is_boolean(A); is_boolean(B) ->
    {z_convert:to_bool(Runtime:to_simple_value(A, Context)),
     z_convert:to_bool(Runtime:to_simple_value(B, Context))};
to_values(A, B, Runtime, Context) when is_integer(A); is_integer(B) ->
    {to_maybe_integer(A, Runtime, Context),
     to_maybe_integer(B, Runtime, Context)};
to_values(A, B, Runtime, Context) when is_float(A); is_float(B) ->
    {to_maybe_float(A, Runtime, Context),
     to_maybe_float(B, Runtime, Context)};
to_values(A, B, _Runtime, _Context) when is_binary(A), is_binary(B) ->
    {A,B};
to_values(A, B, _Runtime, _Context) when is_list(A), is_list(B) ->
    {A,B};
to_values(A, B, Runtime, Context) when is_binary(A); is_binary(B) ->
    {to_maybe_binary(A, Runtime, Context),
     to_maybe_binary(B, Runtime, Context)};
to_values(A, B, Runtime, Context) when is_tuple(A), is_tuple(B) ->
    {Runtime:to_simple_value(A, Context),
     Runtime:to_simple_value(B, Context)};
to_values(A, B, Runtime, Context) when is_binary(A); is_binary(B) ->
    {to_maybe_binary(A, Runtime, Context),
     to_maybe_binary(B, Runtime, Context)};
to_values(A, B, Runtime, Context) ->
    {to_maybe_list(A, Runtime, Context),
     to_maybe_list(B, Runtime, Context)}.


%% @doc Convert the two parameters to compatible numerical values
to_numbers(undefined, B, _Runtime, _Context) ->
    {undefined, B};
to_numbers(A, undefined, _Runtime, _Context) ->
    {A, undefined};
to_numbers(A, B, _Runtime, _Context) when is_number(A), is_number(B) ->
    {A,B};
to_numbers(A, B, Runtime, Context) when is_float(A); is_float(B) ->
    {to_maybe_float(A, Runtime, Context), to_maybe_float(B, Runtime, Context)};
to_numbers(A, B, Runtime, Context) ->
    {to_maybe_integer(A, Runtime, Context), to_maybe_integer(B, Runtime, Context)}.

to_maybe_integer(A, _Runtime, _Context) when is_number(A) ->
    A;
to_maybe_integer(A, Runtime, Context) ->
    try
        A1 = Runtime:to_simple_value(A, Context),
        z_convert:to_integer(A1)
    catch
        _:_ -> undefined
    end.

to_maybe_float(A, _Runtime, _Context) when is_float(A) ->
    A;
to_maybe_float(A, Runtime, Context) ->
    try
        A1 = Runtime:to_simple_value(A, Context),
        z_convert:to_float(A1)
    catch
        _:_ -> undefined
    end.

to_maybe_binary(A, _Runtime, _Context) when is_binary(A) ->
    A;
to_maybe_binary(A, Runtime, Context) ->
    try
        A1 = Runtime:to_simple_value(A, Context),
        z_convert:to_binary(A1)
    catch
        _:_ -> undefined
    end.

to_maybe_list(A, _Runtime, _Context) when is_list(A) ->
    A;
to_maybe_list(A, Runtime, Context) ->
    try
        A1 = Runtime:to_simple_value(A, Context),
        z_convert:to_list(A1)
    catch
        _:_ -> undefined
    end.
