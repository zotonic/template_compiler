%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2026 Marc Worrell
%% @doc Main template compiler entry points.
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

-module(template_compiler).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    render/4,
    render/5,
    render_block/5,
    highlight_file/1,
    highlight_binary/1,
    highlight_binary/2,
    highlight_module/1,
    lookup/3,
    flush/0,
    flush_debug/0,
    flush_debug/1,
    flush_file/1,
    flush_context_name/1,
    compile_file/3,
    compile_binary/4,
    get_option/2,
    is_enabled_debug_points/1,
    is_template_module/1,
    translations/1,
    compile_blocks/2,
    namespace_fragment_blocks/2
    ]).

-include_lib("syntax_tools/include/merl.hrl").
-include_lib("kernel/include/logger.hrl").
-include("template_compiler_internal.hrl").
-include("template_compiler.hrl").


-type option() :: {runtime, atom()}
                | {context_name, term()}
                | {context_vars, list(binary())}
                | {debug_points, all | [{integer(), integer()}] | map()}
                % Internal option, not part of the external API.
                | {trace_position, {Filename::binary(), Line::integer(), Col::integer()}}.
-type options() :: list(option()).
-type template_file() :: #template_file{}.
-type template() :: binary()
                  | string()
                  | {cat, binary()|string()}
                  | {cat, binary()|string(), term()}
                  | {overrules, binary()|string(), file:filename_all()}
                  | template_file().
-type template_key() :: {ContextName::term(), Runtime::atom(), template()}.
-type render_result() :: binary() | string() | term() | list(render_result()).

-type model_return() :: {ok, {term(), list()}}
                      | {error, term()}.

-type builtin_tag() :: image
                     | image_url
                     | image_data_url
                     | media
                     | url
                     | lib.

-type translation_message() :: {
    Text :: binary(),
    Args :: proplists:proplist(),
    {
        Filename :: file:filename(),
        Line :: pos_integer(),
        Column :: pos_integer()
    }
}.

-export_type([
    option/0,
    options/0,
    template/0,
    template_file/0,
    template_key/0,
    builtin_tag/0,
    translation_message/0,
    render_result/0,
    model_return/0
]).


%% @doc Render a template. This looks up the templates needed, ensures compilation and
%%      returns the rendering result.
-spec render(Template, Vars, Options, Context) -> {ok, render_result()} | {error, term()} when
    Template :: template(),
    Vars :: map() | list(),
    Options :: options(),
    Context :: term().
render(Template0, Vars, Options, Context) ->
    render(Template0, #{}, Vars, Options, Context).

%% @doc Render a template. This looks up the templates needed, ensures compilation and
%%      returns the rendering result. Start with a block-map to find some predefined blocks.
-spec render(Template, BlockMap, Vars, Options, Context) -> {ok, render_result()} | {error, term()} when
    Template :: template(),
    BlockMap :: map(),
    Vars :: map() | list(),
    Options :: options(),
    Context :: term().
render(Template0, BlockMap0, Vars, Options, Context) when is_list(Vars) ->
    render(Template0, BlockMap0, props_to_map(Vars, #{}), Options, Context);
render(Template0, BlockMap0, Vars, Options, Context) when is_map(Vars) ->
    Options1 = normalize_options(Options),
    Template = normalize_template(Template0),
    Runtime = proplists:get_value(runtime, Options1, template_compiler_runtime),
    case block_lookup(Runtime:map_template(Template, Vars, Context), BlockMap0, [], [], Options1, Vars, Runtime, Context) of
        {ok, BaseModule, ExtendsStack, BlockMap, OptDebugWrap, VarsResolved, ContextResolved} ->
            % Start with the render function of the "base" template
            % Optionally add the unique prefix for this rendering.
            Vars1 = case BaseModule:is_autoid()
                        orelse lists:any(fun(M) -> M:is_autoid() end, ExtendsStack)
                    of
                        true ->
                            VarsResolved#{
                                '$autoid' => template_compiler_runtime_internal:unique()
                            };
                        false ->
                            VarsResolved
                    end,
            {ok, maybe_wrap(BaseModule:render(Vars1, BlockMap, ContextResolved), OptDebugWrap)};
        {error, {Loc, template_compiler_parser, S}} ->
            {error, {Loc, template_compiler_parser, iolist_to_binary(S)}};
        {error, _} = Error ->
            Error
    end.

%% @doc Render a named block, defined in a template
-spec render_block(Block, Template, Vars, Options, Context) -> {ok, render_result()} | {error, term()} when
    Block :: atom(),
    Template :: template(),
    Vars :: map() | list(),
    Options :: options(),
    Context :: term().
render_block(Block, Template, Vars, Options, Context) when is_list(Vars) ->
    render_block(Block, Template, props_to_map(Vars, #{}), Options, Context);
render_block(Block, Template0, Vars, Options, Context) when is_map(Vars) ->
    Options1 = normalize_options(Options),
    Template = normalize_template(Template0),
    Runtime = proplists:get_value(runtime, Options1, template_compiler_runtime),
    case block_lookup(Runtime:map_template(Template, Vars, Context), #{}, [], [], Options1, Vars, Runtime, Context) of
        {ok, BaseModule, ExtendsStack, BlockMap, _OptDebugWrap, VarsResolved, ContextResolved} ->
            % Optionally add the unique prefix for this rendering.
            Vars1 = case BaseModule:is_autoid()
                        orelse lists:any(fun(M) -> M:is_autoid() end, ExtendsStack)
                    of
                        true ->
                            VarsResolved#{
                                '$autoid' => template_compiler_runtime_internal:unique()
                            };
                        false ->
                            VarsResolved
                    end,
            % Render the specific block
            {ok, template_compiler_runtime_internal:block_call({<<>>,1,1}, Block, Vars1, BlockMap, Runtime, ContextResolved)};
        {error, {Loc, template_compiler_parser, S}} ->
            {error, {Loc, template_compiler_parser, iolist_to_binary(S)}};
        {error, _} = Error ->
            Error
    end.

%% @doc Return syntax highlighted HTML for a template source file.
-spec highlight_file(file:filename_all()) -> {ok, binary()} | {error, term()}.
highlight_file(Filename) ->
    template_compiler_highlight:highlight_file(Filename).

%% @doc Return syntax highlighted HTML for an in-memory template source.
-spec highlight_binary(binary()) -> {ok, binary()} | {error, term()}.
highlight_binary(Bin) ->
    template_compiler_highlight:highlight_binary(Bin).

%% @doc Return syntax highlighted HTML for an in-memory template source.
-spec highlight_binary(binary(), file:filename_all()) -> {ok, binary()} | {error, term()}.
highlight_binary(Bin, Filename) ->
    template_compiler_highlight:highlight_binary(Bin, Filename).

%% @doc Return syntax highlighted HTML for a compiled template module,
%%      including checkbox inputs for the module's debug points.
-spec highlight_module(module()) -> {ok, binary()} | {error, term()}.
highlight_module(Module) ->
    template_compiler_highlight:highlight_module(Module).


maybe_wrap(RenderResult, []) ->
    RenderResult;
maybe_wrap(RenderResult, [ok|Rest]) ->
    maybe_wrap(RenderResult, Rest);
maybe_wrap(RenderResult, [{ok, Before, After}|Rest]) ->
    maybe_wrap([Before, RenderResult, After], Rest).

enabled_debug_points_map(all) ->
    all;
enabled_debug_points_map(DebugPoints) when is_map(DebugPoints) ->
    DebugPoints;
enabled_debug_points_map(DebugPoints) when is_list(DebugPoints) ->
    maps:from_keys(DebugPoints, true);
enabled_debug_points_map(_) ->
    #{}.

normalize_options(Options) ->
    DebugPoints = enabled_debug_points_map(get_option(debug_points, Options)),
    [
        {debug_points, DebugPoints}
        | lists:keydelete(debug_points, 1, Options)
    ].

props_to_map([], Map) ->
    Map;
props_to_map([{K,V}|Rest], Map) ->
    props_to_map(Rest, Map#{K => V});
props_to_map([K|Rest], Map) ->
    props_to_map(Rest, Map#{K => true}).

%% @doc Map all string() template names to binary().
-spec normalize_template(TemplateName) -> NormalizedTemplateName when
    TemplateName :: template(),
    NormalizedTemplateName :: template().
normalize_template(Template) when is_binary(Template) ->
    Template;
normalize_template(#template_file{filename=Fn, template=Tpl} = T) when is_binary(Fn), is_binary(Tpl)  ->
    T;
normalize_template({cat, Template} = T) when is_binary(Template) ->
    T;
normalize_template({cat, Template, _} = T) when is_binary(Template) ->
    T;
normalize_template({overrules, Template, _Filename} = T) when is_binary(Template) ->
    T;
normalize_template(Template) when is_list(Template) ->
    unicode:characters_to_binary(Template);
normalize_template(#template_file{filename=Fn, template=Tpl}) ->
    #template_file{
        filename = unicode:characters_to_binary(Fn),
        template = unicode:characters_to_binary(Tpl)
    };
normalize_template({cat, Template}) when is_list(Template) ->
    {cat, unicode:characters_to_binary(Template)};
normalize_template({cat, Template, IsA}) when is_list(Template) ->
    {cat, unicode:characters_to_binary(Template), IsA};
normalize_template({overrules, Template, Filename}) when is_list(Template) ->
    {overrules, unicode:characters_to_binary(Template), Filename}.

%% @doc Recursive lookup of blocks via the extends-chain of a template.
block_lookup({ok, TplFile}, BlockMap, ExtendsStack, DebugTrace, Options, Vars, Runtime, Context) ->
    TplFilename = TplFile#template_file.filename,
    Trace = Runtime:trace_render(TplFile#template_file.filename, Options, Context),
    case template_compiler_admin:lookup(TplFile#template_file.filename, Options, Context) of
        {ok, Module} ->
            case lists:member(Module, ExtendsStack) of
                true ->
                    FileTrace = [Module:filename() | [ M:filename() || M <- ExtendsStack ]],
                    ?LOG_ERROR(#{
                        text => <<"Template recursion">>,
                        result => error,
                        reason => recursion,
                        trace => FileTrace
                    }),
                    {error, {recursion, [Trace|DebugTrace]}};
                false ->
                    % Check extended/overruled templates (build block map)
                    BlockMap1 = add_blocks(Module:blocks(), Module, BlockMap),
                    case Module:extends_runtime(Vars, Context) of
                        {undefined, Vars1, Context1} ->
                            {ok, Module, ExtendsStack, BlockMap1, [Trace|DebugTrace], Vars1, Context1};
                        {overrules, Vars1, Context1} ->
                            Options1 = [
                                {trace_position, {TplFilename, 0, 0}}
                                | lists:keydelete(trace_position, 1, Options)
                            ],
                            Template = TplFile#template_file.template,
                            Next = Runtime:map_template({overrules, Template, Module:filename()}, Vars1, Context1),
                            block_lookup(Next, BlockMap1, [Module|ExtendsStack], [Trace|DebugTrace], Options1, Vars1, Runtime, Context1);
                        {Extends, Vars1, Context1} when is_binary(Extends) ->
                            Options1 = [
                                {trace_position, {TplFilename, 0, 0}}
                                | lists:keydelete(trace_position, 1, Options)
                            ],
                            Next = Runtime:map_template(Extends, Vars1, Context1),
                            block_lookup(Next, BlockMap1, [Module|ExtendsStack], [Trace|DebugTrace], Options1, Vars1, Runtime, Context1)
                    end
            end;
        {error, _} = Error ->
            Error
    end;
block_lookup({error, _} = Error, _BlockMap, _ExtendsStack, _DebugTrace, _Options, _Vars, _Runtime, _Context) ->
    Error.


add_blocks([], _Module, BlockMap) ->
    BlockMap;
add_blocks([Block|Blocks], Module, BlockMap) ->
    List = maps:get(Block, BlockMap, []),
    BlockMap1 = BlockMap#{ Block => List ++ [Module]},
    add_blocks(Blocks, Module, BlockMap1).


%% @doc Extract compiler options and handle possible defaults.
-spec get_option(Option :: atom(), Options :: options()) -> term().
get_option(runtime, Options) ->
    proplists:get_value(runtime, Options, template_compiler_runtime);
get_option(context_vars, Options) ->
    proplists:get_value(context_vars, Options, []);
get_option(debug_points, Options) ->
    proplists:get_value(debug_points, Options, []).

%% @doc Find the module of a compiled template, if not yet compiled then
%% compile the template.
-spec lookup(Filename, Options, Context) -> {ok, Module} | {error, term()} when
    Filename :: binary(),
    Options :: options(),
    Context :: term(),
    Module :: module().
lookup(Filename, Options, Context) ->
    template_compiler_admin:lookup(Filename, normalize_options(Options), Context).


%% @doc Remove all template lookups, forces recheck.
-spec flush() -> ok.
flush() ->
    template_compiler_admin:flush().

%% @doc Force recheck of all templates which are compiled with debug points enabled.
-spec flush_debug() -> ok.
flush_debug() ->
    template_compiler_admin:flush_debug().

%% @doc Force recheck of debug-compiled templates for a specific context name.
-spec flush_debug(ContextName::term()) -> ok.
flush_debug(ContextName) ->
    template_compiler_admin:flush_debug(ContextName).

%% @doc Ping that a template has been changed
-spec flush_file(file:filename_all()) -> ok.
flush_file(Filename) ->
    template_compiler_admin:flush_file(Filename).

%% @doc Ping that a template has been changed
-spec flush_context_name(ContextName::term()) -> ok.
flush_context_name(ContextName) ->
    template_compiler_admin:flush_context_name(ContextName).


%% @doc Compile a template to a module. The template is the path of the
%% template to be compiled.
-spec compile_file(Filename, Options, Context) -> {ok, Module} | {error, term()} when
    Filename :: file:filename_all(),
    Options :: options(),
    Context :: term(),
    Module :: module().
compile_file(Filename, Options, Context) ->
    Options1 = normalize_options(Options),
    case file:read_file(Filename) of
        {ok, Tpl} ->
            compile_binary(Tpl, Filename, Options1, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Compile a in-memory template to a module.
-spec compile_binary(TemplateBin, Filename, Options, Context) -> {ok, Module} | {error, term()} when
    TemplateBin :: binary(),
    Filename :: file:filename_all(),
    Options :: options(),
    Context :: term(),
    Module :: module().
compile_binary(Tpl, Filename, Options, Context) when is_binary(Tpl) ->
    Options1 = normalize_options(Options),
    Mtime = template_compiler_utils:file_mtime(Filename),
    case template_compiler_scanner:scan(Filename, Tpl) of
        {ok, Tokens} ->
            Runtime = get_option(runtime, Options1),
            ContextVars = get_option(context_vars, Options1),
            DebugPoints = get_option(debug_points, Options1),
            EnabledDebugPoints = enabled_debug_points_map(DebugPoints),
            IsDebugPoints = is_enabled_debug_points(EnabledDebugPoints),
            Tokens1 = maybe_drop_text(Tokens, Tokens),
            Tokens2 = expand_translations(Tokens1, Runtime, Context),
            ContentChecksum = content_checksum(Tokens2),
            Module = module_name(Runtime, Filename, ContextVars),
            case erlang:module_loaded(Module) of
                true ->
                    case is_matching_module(Module, ContentChecksum, EnabledDebugPoints, IsDebugPoints) of
                        true ->
                            ok = template_compiler_admin:register(Filename, Options1, Context, Module),
                            {ok, Module};
                        false ->
                            compile_binary_1(Module, Filename, Mtime, Runtime, ContentChecksum, EnabledDebugPoints, IsDebugPoints, Tokens2, Options1, Context)
                    end;
                false ->
                    compile_binary_1(Module, Filename, Mtime, Runtime, ContentChecksum, EnabledDebugPoints, IsDebugPoints, Tokens2, Options1, Context)
            end;
        {error, _} = Error ->
            Error
    end.


%% @doc Check if the modulename looks like a module generated by the template compiler.
-spec is_template_module(binary()|string()|atom()) -> boolean().
is_template_module(<<"tpl_", _/binary>>) -> true;
is_template_module("tpl_" ++ _) -> true;
is_template_module(X) when is_binary(X) -> false;
is_template_module(X) when is_list(X) -> false;
is_template_module(Name) -> is_template_module(z_convert:to_binary(Name)).


%% @doc Fetch all translatable strings from a template file.
-spec translations(file:filename_all()) -> {ok, [translation_message()]} | {error, term()}.
translations(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            case template_compiler_scanner:scan(Filename, Bin) of
                {ok, Tokens} ->
                    Tokens1 = map_text_tokens(Tokens, []),
                    {ok, extract_translations(Tokens1)};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%%%% --------------------------------- Internal ----------------------------------

module_name(Runtime, Filename, SpecialContextArgs) ->
    Term = {
        compiler_version(),
        unicode:characters_to_binary(Filename),
        Runtime,
        SpecialContextArgs
    },
    IdentityChecksum = crypto:hash(sha, term_to_binary(Term)),
    Hex = z_string:to_lower(z_url:hex_encode(IdentityChecksum)),
    binary_to_atom(iolist_to_binary(["tpl_",Hex]), 'utf8').

-spec compiler_version() -> string().
compiler_version() ->
    case application:get_key(template_compiler, vsn) of
        {ok, Vsn} ->
            Vsn;
        undefined ->
            "unknown"
    end.

content_checksum(Tokens) ->
    crypto:hash(sha, term_to_binary(remove_srcpos(Tokens))).

is_enabled_debug_points(all) ->
    true;
is_enabled_debug_points(DebugPoints) when is_map(DebugPoints) ->
    map_size(DebugPoints) > 0;
is_enabled_debug_points(_) ->
    false.

% Ensure that duplicate files have the same checksum by removing the filename.
remove_srcpos(Tokens) ->
    [ {Token, V} || {Token, _SrcPos, V} <- Tokens ].


compile_forms(Filename, Forms) ->
    % case compile:forms(Forms, [nowarn_shadow_vars]) of
    Forms1 = [ erl_syntax:revert(Form) || Form <- Forms ],
    case compile:forms(Forms1, [report_errors]) of
        Compiled when element(1, Compiled) =:= ok ->
            [ok, Module, Bin | _Info] = tuple_to_list(Compiled),
            code:purge(Module),
            case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin) of
                {module, _Module} ->
                    {ok, Module};
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        text => <<"Error loading compiling forms">>,
                        module => Module,
                        filename => Filename,
                        result => error,
                        reason => Reason
                    }),
                    Error
            end;
        error ->
            ?LOG_ERROR(#{
                text => <<"Error compiling forms">>,
                result => error,
                reason => compile,
                filename => Filename
            }),
            {error, {compile, []}};
        {error, Es, Ws} ->
            ?LOG_ERROR(#{
                text => <<"Errors compiling">>,
                filename => Filename,
                result => error,
                reason => compile,
                errors => Es,
                warnings => Ws
            }),
            {error, {compile, Es, Ws}}
    end.

is_matching_module(Module, ContentChecksum, EnabledDebugPoints, IsDebugPoints) ->
    erlang:function_exported(Module, content_checksum, 0)
        andalso erlang:function_exported(Module, enabled_debug_points, 0)
        andalso Module:is_debug_compiled() =:= IsDebugPoints
        andalso Module:enabled_debug_points() =:= EnabledDebugPoints
        andalso Module:content_checksum() =:= ContentChecksum.

compile_binary_1(Module, Filename, Mtime, Runtime, ContentChecksum, EnabledDebugPoints, IsDebugPoints, Tokens, Options, Context) ->
    Runtime:trace_compile(Module, Filename, Options, Context),
    case compile_tokens(
            template_compiler_parser:parse(Tokens),
            cs(Module, Filename, Options, Context, EnabledDebugPoints),
            Options)
    of
        {ok, {Extends, ExtendsRuntimeAst, Includes, BlockAsts, TemplateAst, IsAutoid, DebugPoints}} ->
            Forms = template_compiler_module:compile(
                                Module, Filename, Mtime, ContentChecksum, IsAutoid, Runtime,
                                {Extends, ExtendsRuntimeAst}, Includes, BlockAsts, {TemplateAst, DebugPoints, EnabledDebugPoints, IsDebugPoints}),
            case compile_forms(Filename, Forms) of
                {ok, CompiledModule} = Ok ->
                    ok = template_compiler_admin:register(Filename, Options, Context, CompiledModule),
                    Ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

cs(Module, Filename, Options, Context, EnabledDebugPoints) ->
    #cs{
        filename=Filename,
        module=Module,
        block_owner=Module,
        runtime=get_option(runtime, Options),
        context_vars=get_option(context_vars, Options),
        enabled_debug_points=EnabledDebugPoints,
        context=Context
    }.

compile_tokens({ok, {extends, {_TagPos, {string_literal, _, Extend}, ExtendsArgs}, Elements}}, CState, _Options) ->
    case find_blocks(Elements) of
        {ok, Blocks} ->
            {Ws, BlockAsts} = compile_blocks(Blocks, CState),
            {Ws1, ExtendsRuntimeAst} = compile_extends_runtime_ast(Extend, ExtendsArgs, CState),
            Ws2 = merge_ws(Ws, Ws1),
            {ok, {Extend, ExtendsRuntimeAst, Ws2#ws.includes, BlockAsts, undefined, Ws2#ws.is_autoid_var, collect_debug_points(undefined, BlockAsts)}};
        {error, _} = Error ->
            Error
    end;
compile_tokens({ok, {overrules, {_TagPos, OverrulesArgs}, Elements}}, CState, _Options) ->
    case find_blocks(Elements) of
        {ok, Blocks} ->
            {Ws, BlockAsts} = compile_blocks(Blocks, CState),
            {Ws1, ExtendsRuntimeAst} = compile_extends_runtime_ast(overrules, OverrulesArgs, CState),
            Ws2 = merge_ws(Ws, Ws1),
            {ok, {overrules, ExtendsRuntimeAst, Ws2#ws.includes, BlockAsts, undefined, Ws2#ws.is_autoid_var, collect_debug_points(undefined, BlockAsts)}};
        {error, _} = Error ->
            Error
    end;
compile_tokens({ok, {base, Elements}}, CState, _Options) ->
    case find_blocks(Elements) of
        {ok, Blocks} ->
            {Ws, BlockAsts} = compile_blocks(Blocks, CState),
            CStateElts = CState#cs{blocks = BlockAsts},
            {Ws1, TemplateAsts} = template_compiler_element:compile(Elements, CStateElts, Ws),
            {ok, {undefined, default_extends_runtime_ast(undefined, CState), Ws1#ws.includes, BlockAsts, TemplateAsts, Ws1#ws.is_autoid_var, collect_debug_points(Ws1, BlockAsts)}};
        {error, _} = Error ->
            Error
    end;
compile_tokens({error, {Loc, template_compiler_parser, Msg}}, #cs{ filename = Filename }, Options) ->
    % Try format the Yecc error
    Err = split_loc(Loc),
    Stack = [
        {filename:basename(Filename), render, 1, [
            {file, maps:get(at, Err)},
            {line, {maps:get(line, Err), maps:get(column, Err)}}
        ]}
        | case proplists:get_value(trace_position, Options) of
            {SrcFile, SrcLine, _SrcCol} ->
                [
                    {filename:basename(SrcFile), render, 1, [
                        {file, SrcFile},
                        {line, SrcLine}
                    ]}
                ];
            undefined ->
                []
        end
    ],
    Err1 = Err#{
        result => error,
        reason => syntax,
        text => iolist_to_binary(Msg),
        stack => Stack
    },
    {error, Err1};
compile_tokens({error, _} = Error, _CState, _Options) ->
    Error.

default_extends_runtime_ast(Extends, CState) ->
    erl_syntax:tuple([
        erl_syntax:abstract(Extends),
        erl_syntax:variable(CState#cs.vars_var),
        erl_syntax:variable(CState#cs.context_var)
    ]).

compile_extends_runtime_ast(Extends, [], CState) ->
    {#ws{}, default_extends_runtime_ast(Extends, CState)};
compile_extends_runtime_ast(Extends, Args, CState) ->
    {Ws1, ArgsList} = compile_with_args(Args, CState, #ws{}),
    VarsVarName = "VarsExtends",
    VarsAst = erl_syntax:map_expr(
        erl_syntax:variable(CState#cs.vars_var),
        [ erl_syntax:map_field_assoc(WName, WExpr) || {WName, WExpr} <- ArgsList ]),
    PrefixAsts = [
        erl_syntax:match_expr(
            erl_syntax:variable(VarsVarName),
            VarsAst)
    ],
    case is_context_vars_arg(Args, CState) of
        true ->
            ContextVarName = "ContextExtends",
            ContextAst = erl_syntax:application(
                erl_syntax:atom(CState#cs.runtime),
                erl_syntax:atom(set_context_vars),
                [
                    erl_syntax:variable(VarsVarName),
                    erl_syntax:variable(CState#cs.context_var)
                ]),
            Ast = erl_syntax:block_expr(
                PrefixAsts ++
                [
                    erl_syntax:match_expr(
                        erl_syntax:variable(ContextVarName),
                        ContextAst),
                    erl_syntax:tuple([
                        erl_syntax:abstract(Extends),
                        erl_syntax:variable(VarsVarName),
                        erl_syntax:variable(ContextVarName)
                    ])
                ]),
            {Ws1, Ast};
        false ->
            Ast = erl_syntax:block_expr(
                PrefixAsts ++
                [
                    erl_syntax:tuple([
                        erl_syntax:abstract(Extends),
                        erl_syntax:variable(VarsVarName),
                        erl_syntax:variable(CState#cs.context_var)
                    ])
                ]),
            {Ws1, Ast}
    end.

compile_with_args(With, CState, Ws) ->
    lists:foldl(
        fun
            ({Ident, true}, {WsAcc, Acc}) ->
                VarAst = erl_syntax:atom(ident_as_atom(Ident)),
                {WsAcc, [{VarAst, erl_syntax:atom(true)}|Acc]};
            ({Ident, Expr}, {WsAcc, Acc}) ->
                {Ws1, ExprAst} = template_compiler_expr:compile(Expr, CState, WsAcc),
                VarAst = erl_syntax:atom(ident_as_atom(Ident)),
                {Ws1, [{VarAst, ExprAst}|Acc]}
        end,
        {Ws, []},
        With).

is_context_vars_arg(Args, CState) when is_list(Args) ->
    lists:any(
        fun
            ({{identifier, _, Ident}, _Val}) ->
                lists:member(Ident, CState#cs.context_vars)
        end,
        Args).

ident_as_atom({identifier, _SrcPos, Ident}) ->
    template_compiler_utils:to_atom(Ident).

merge_ws(WsA, WsB) ->
    WsA#ws{
        nr = erlang:max(WsA#ws.nr, WsB#ws.nr),
        custom_tags = WsA#ws.custom_tags ++ WsB#ws.custom_tags,
        is_forloop_var = WsA#ws.is_forloop_var orelse WsB#ws.is_forloop_var,
        is_autoid_var = WsA#ws.is_autoid_var orelse WsB#ws.is_autoid_var,
        includes = WsA#ws.includes ++ WsB#ws.includes,
        debug_points = WsA#ws.debug_points ++ WsB#ws.debug_points
    }.

namespace_fragment_blocks(FragmentName, Elements) ->
    [ namespace_fragment_element(FragmentName, E) || E <- Elements ].

namespace_fragment_element(FragmentName, {block, {identifier, Pos, Name}, Elts}) ->
    {block, {identifier, Pos, namespaced_fragment_block_name(FragmentName, Name)}, namespace_fragment_blocks(FragmentName, Elts)};
namespace_fragment_element(FragmentName, {for, Expr, Loop, Empty}) ->
    {for, Expr, namespace_fragment_blocks(FragmentName, Loop), namespace_fragment_blocks(FragmentName, Empty)};
namespace_fragment_element(FragmentName, {'if', Expr, If, Else}) ->
    {'if', Expr, namespace_fragment_blocks(FragmentName, If), namespace_fragment_blocks(FragmentName, Else)};
namespace_fragment_element(FragmentName, {spaceless, Expr, Elts}) ->
    {spaceless, Expr, namespace_fragment_blocks(FragmentName, Elts)};
namespace_fragment_element(FragmentName, {autoescape, Expr, Elts}) ->
    {autoescape, Expr, namespace_fragment_blocks(FragmentName, Elts)};
namespace_fragment_element(FragmentName, {with, Expr, Elts}) ->
    {with, Expr, namespace_fragment_blocks(FragmentName, Elts)};
namespace_fragment_element(FragmentName, {cache, Expr, Elts}) ->
    {cache, Expr, namespace_fragment_blocks(FragmentName, Elts)};
namespace_fragment_element(FragmentName, {javascript, Expr, Elts}) ->
    {javascript, Expr, namespace_fragment_blocks(FragmentName, Elts)};
namespace_fragment_element(FragmentName, {filter, Expr, Elts}) ->
    {filter, Expr, namespace_fragment_blocks(FragmentName, Elts)};
namespace_fragment_element(_FragmentName, Element) ->
    Element.

namespaced_fragment_block_name(FragmentName, Name) ->
    <<FragmentName/binary, "$", Name/binary>>.

split_loc({Filename, Line, Col}) ->
    #{
        at => Filename,
        line => Line,
        column => Col
    };
split_loc({Filename, Line}) ->
    #{
        at => Filename,
        line => Line
    }.

collect_debug_points(undefined, BlockAsts) ->
    lists:usort(lists:flatten([ BlockWs#ws.debug_points || {_, _, BlockWs} <- BlockAsts ]));
collect_debug_points(Ws, BlockAsts) ->
    lists:usort(Ws#ws.debug_points ++ collect_debug_points(undefined, BlockAsts)).

-spec compile_blocks([block_element()], #cs{}) -> {#ws{}, [{atom(), erl_syntax:syntaxTree(), #ws{}}]}.
compile_blocks(Blocks, CState) ->
    Ws = #ws{},
    lists:foldl(
        fun(Block, {WsAcc, BlockAcc}) ->
            CState1 = CState#cs{blocks = BlockAcc},
            {WsAcc1, B} = compile_block(Block, CState1, WsAcc),
            {WsAcc1, [B|BlockAcc]}
        end,
        {Ws,[]},
        Blocks).

%% @doc Compile a block definition to a function name and its body elements.
-spec compile_block(block_element(), #cs{}, #ws{}) -> {#ws{}, {atom(), erl_syntax:syntaxTree(), #ws{}}}.
compile_block({block, {identifier, _Pos, Name}, Elts}, CState, Ws) ->
    compile_named_block(Name, Elts, CState, Ws);
compile_block({fragment, {identifier, _Pos, Name}, Elts}, CState, Ws) ->
    compile_named_block(Name, Elts, CState, Ws).

compile_named_block(Name, Elts, CState, Ws) ->
    BlockName = template_compiler_utils:to_atom(Name),
    {Ws1, Body} = template_compiler_element:compile(Elts, CState#cs{block=BlockName}, reset_block_ws(Ws)),
    {Ws1, {BlockName, Body, Ws1}}.

reset_block_ws(Ws) ->
    Ws#ws{is_forloop_var=false}.


%% @doc Extract all block definitions from the parse tree, returns deepest nested blocks first
find_blocks(Elements) ->
    find_blocks(Elements, [], []).

find_blocks([], Acc, _Stack) ->
    {ok, Acc};
find_blocks([B|Bs], Acc, Stack) ->
    case find_blocks(B, Acc, Stack) of
        {ok, Acc1} ->
            find_blocks(Bs, Acc1, Stack);
        {error, _} = Error ->
            Error
    end;
find_blocks({block, {identifier, _Pos, Name}, Elements} = Block, Acc, Stack) ->
    find_named_block(Name, Elements, Block, Acc, Stack);
find_blocks({fragment, {identifier, _Pos, Name}, Elements} = Fragment, Acc, Stack) ->
    Elements1 = namespace_fragment_blocks(Name, Elements),
    find_named_block(Name, Elements1, setelement(3, Fragment, Elements1), Acc, Stack);
find_blocks(Element, Acc, Stack) ->
    find_blocks(block_elements(Element), Acc, Stack).

find_named_block(Name, Elements, NamedBlock, Acc, Stack) ->
    case lists:member(Name, Stack) of
        true ->
            {error, {duplicate_nested_block, Name}};
        false ->
            Acc1 = [ NamedBlock | Acc ],
            Stack1 = [ Name | Stack ],
            find_blocks(Elements, Acc1, Stack1)
    end.

block_elements({compose, _, Elts}) -> Elts;
block_elements({catcompose, _, Elts}) -> Elts;
block_elements({fragment, _, Elts}) -> Elts;
block_elements({for, _, Loop, Empty}) -> [Loop,Empty];
block_elements({'if', _, If, Else}) -> [If, Else];
block_elements({spaceless, _, Elts}) -> Elts;
block_elements({autoescape, _, Elts}) -> Elts;
block_elements({with, _, Elts}) -> Elts;
block_elements({cache, _, Elts}) -> Elts;
block_elements({javascript, _, Elts}) -> Elts;
block_elements({filter, _, Elts}) -> Elts;
block_elements(_) -> [].


%% @doc Optionally drop text before {% extends %} or {% overrules %}.
maybe_drop_text([{text, _SrcRef, _Text}|Rest], OrgTks) ->
    maybe_drop_text(Rest, OrgTks);
maybe_drop_text([{comment, _Text}|Rest], OrgTks) ->
    maybe_drop_text(Rest, OrgTks);
maybe_drop_text([{open_tag, _, _}, {extends_keyword, _, _}|_] = Tks, _OrgTks) ->
    Tks;
maybe_drop_text([{open_tag, _, _}, {overrules_keyword, _, _}|_] = Tks, _OrgTks) ->
    Tks;
maybe_drop_text(_, [{open_tag, SrcRef, _}|_] = OrgTks) ->
    [{text, SrcRef, <<>>}|OrgTks];
maybe_drop_text(_, OrgTks) ->
    OrgTks.


%% @doc Expand all translations in the tokens. Translations are always looked up at compile time.
expand_translations(Tokens, Runtime, Context) ->
    Tokens1 = map_text_tokens(Tokens, []),
    [ expand_translation(Token, Runtime, Context) || Token <- Tokens1 ].


map_text_tokens([], Acc) ->
    lists:reverse(Acc);
map_text_tokens([{trans_keyword, _, _}=Trans, {string_literal, SrcPos, Text}|Ts], Acc) ->
    Acc1 = [{trans_literal, SrcPos, unescape_trim(Text)},Trans|Acc],
    map_text_tokens(Ts, Acc1);
map_text_tokens([{trans_text, SrcPos, Text}|Ts], Acc) ->
    Acc1 = [{trans_text, SrcPos, unescape_trim(Text)}|Acc],
    map_text_tokens(Ts, Acc1);
map_text_tokens([{trans_literal, SrcPos, Text}|Ts], Acc) ->
    Acc1 = [{trans_literal, SrcPos, unescape_trim(Text)}|Acc],
    map_text_tokens(Ts, Acc1);
map_text_tokens([{string_literal, SrcPos, Text}|Ts], Acc) ->
    Acc1 = [{string_literal, SrcPos, template_compiler_utils:unescape_string_literal(Text)}|Acc],
    map_text_tokens(Ts, Acc1);
map_text_tokens([T|Ts], Acc) ->
    map_text_tokens(Ts, [T|Acc]).

unescape_trim(Text) ->
    Unescaped = template_compiler_utils:unescape_string_literal(Text),
    z_string:trim(Unescaped).


expand_translation({trans_text, SrcPos, Text}, Runtime, Context) ->
    case Runtime:get_translations(Text, Context) of
        {trans, _} = Tr -> {trans_text, SrcPos, Tr};
        B when is_binary(B) -> {text, SrcPos, B}
    end;
expand_translation({trans_literal, SrcPos, Text}, Runtime, Context) ->
    case Runtime:get_translations(Text, Context) of
        {trans, _} = Tr -> {trans_literal, SrcPos, Tr};
        B when is_binary(B) -> {trans_literal, SrcPos, {trans, [{en, B}]}}
    end;
expand_translation(Token, _Runtime, _Context) ->
    Token.


%% @doc Fetch all translatable strings from the tokens
extract_translations(Tokens) ->
    lists:foldl(
        fun
            ({trans_text, SrcPos, Text}, Acc) ->
                [ {Text, [], SrcPos} | Acc ];
            ({trans_literal, SrcPos, Text}, Acc) ->
                [ {Text, [], SrcPos} | Acc ];
            (_Token, Acc) ->
                Acc
        end,
        [],
        Tokens).
