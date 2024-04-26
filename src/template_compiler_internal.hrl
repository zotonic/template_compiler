%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2023 Marc Worrell
%% @doc Template compiler internal definitions.
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


%% @doc Increment this with compiler bug fixes
-define(COMPILER_VERSION, 1).

-type linecol() :: {Line::integer(), Column::integer(), file:filename_all()}.

-type token() :: {atom(), linecol(), term()}
               | identifier_token().

-type identifier_token() :: {identifier, linecol(), binary()}.

-type block_element() :: {block, identifier_token(), elements()}.

-type elements() :: list( element() ).

-type element() :: block_element()
                 | true
                 | false
                 | undefined
                 | term().


%% @doc Treewalk state. Has counter for variable names and flags if the forloop, unique-var,
%%      and/or includes with inherited arguments are used inside a code block.
-record(ws, {
        nr = 1 :: integer(),
        custom_tags = [],
        is_forloop_var = false :: boolean(),
        is_autoid_var = false :: boolean(),
        includes = [] :: [ binary() ]
    }).

%% @doc State for the compiler. Also records the current block's arguments variable, and context variable.
-record(cs, {
        filename = <<>> :: binary(),
        module = undefined :: atom(),
        block = undefined :: atom(),
        blocks = [] :: list( {atom(), erl_syntax:syntaxTree(), #ws{}} ),
        runtime = template_compiler_runtime :: atom(),
        context = undefined :: term(),
        vars_var = "Vars" :: string(),
        context_var = "Context" :: string(),
        context_vars = [] :: list(binary()),
        is_autoescape = false :: boolean()
    }).
