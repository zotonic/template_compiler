%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2026 Marc Worrell
%% @doc Build the template module from the parts.
%% @end

%% Copyright 2016-2026 Marc Worrell
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

-module(template_compiler_module).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    compile/10
    ]).

-include_lib("syntax_tools/include/merl.hrl").

compile(Module, Filename, Mtime, ContentChecksum, IsAutoid, Runtime, Extends, Includes, BlockAsts, undefined) ->
    compile(Module, Filename, Mtime, ContentChecksum, IsAutoid, Runtime, Extends, Includes, BlockAsts, erl_syntax:abstract(<<>>), [], false);
compile(Module, Filename, Mtime, ContentChecksum, IsAutoid, Runtime, Extends, Includes, BlockAsts, {undefined, DebugPoints, IsDebugCompiled}) ->
    compile(Module, Filename, Mtime, ContentChecksum, IsAutoid, Runtime, Extends, Includes, BlockAsts, erl_syntax:abstract(<<>>), DebugPoints, IsDebugCompiled);
compile(Module, Filename, Mtime, ContentChecksum, IsAutoid, Runtime, Extends, Includes, BlockAsts, {TemplateAst, DebugPoints, IsDebugCompiled}) ->
    compile(Module, Filename, Mtime, ContentChecksum, IsAutoid, Runtime, Extends, Includes, BlockAsts, TemplateAst, DebugPoints, IsDebugCompiled);
compile(Module, Filename, Mtime, ContentChecksum, IsAutoid, Runtime, Extends, Includes, BlockAsts, TemplateAst) ->
    compile(Module, Filename, Mtime, ContentChecksum, IsAutoid, Runtime, Extends, Includes, BlockAsts, TemplateAst, [], false).

compile(Module, Filename, Mtime, ContentChecksum, IsAutoid, Runtime, Extends, Includes, BlockAsts, TemplateAst, DebugPoints, IsDebugCompiled) ->
    Now = os:timestamp(),
    BlockNames = [ BN || {BN, _Tree, _Ws} <- BlockAsts ],
    Forms1 = lists:flatten(
        ?Q(["-module('@Module@').",
            "-export([",
                "render/3,",
                "render_block/4,",
                "timestamp/0,",
                "blocks/0,",
                "module/0,",
                "extends/0,",
                "includes/0,",
                "debug_points/0,",
                "content_checksum/0,",
                "is_debug_compiled/0,",
                "filename/0,",
                "mtime/0,",
                "is_autoid/0,"
                "runtime/0",
            "]).",
            "render(Vars, Blocks, Context) -> _@TemplateAst.",
            "timestamp() -> _@Now@.",
            "blocks() -> _@BlockNames@.",
            "module() -> _@Module@.",
            "extends() -> _@Extends@.",
            "includes() -> _@Includes@.",
            "content_checksum() -> _@ContentChecksum@.",
            "filename() -> _@Filename@.",
            "mtime() -> _@Mtime@.",
            "is_autoid() -> _@IsAutoid@.",
            "runtime() -> _@Runtime@.",
            "'@_functions'() -> _."
            ],
            [
                {functions, blocksfun(BlockAsts)}
            ])),
    Forms = Forms1 ++ [
        function(debug_points, lists:usort(DebugPoints)),
        function(is_debug_compiled, IsDebugCompiled)
    ],
    [
        merl:quote(1, "-file(\""++unicode:characters_to_list(Filename)++"\", 1).")
        | Forms
    ].

blocksfun(Blocks) ->
    Clauses = [
        ?Q("(_@BlockName@, Vars, Blocks, Context) -> _@BlockAst")
        || {BlockName, BlockAst, _BlockWs} <- Blocks
    ] ++ [
        ?Q("(_BlockName, _Vars, _Blocks, _Context) -> <<>>")
    ],
    erl_syntax:function(erl_syntax:atom(render_block), Clauses).

function(Name, Value) ->
    erl_syntax:function(
        erl_syntax:atom(Name),
        [erl_syntax:clause([], none, [erl_syntax:abstract(Value)])]).
