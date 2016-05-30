%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Main template compiler entry points.

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

-module(template_compiler).
-author('Marc Worrell <marc@worrell.nl>').

-export([
    render/4,
    lookup/3,
    flush_file/1,
    flush_context_name/1,
    compile_file/3,
    compile_binary/4,
    get_option/2
    ]).

-include_lib("syntax_tools/include/merl.hrl").
-include("template_compiler.hrl").

-type option() :: {runtime, atom()}.
-type options() :: list(option()).
-type template() :: binary()
                  | string()
                  | {cat, binary()|string()}
                  | {overrides, binary()|string(), Filename::filename:filename()}.
-type template_key() :: {ContextName::term(), Runtime::atom(), template()}.
-type render_result() :: binary() | string() | term() | list(render_result()).

-export_type([
        option/0,
        options/0,
        template/0,
        template_key/0
    ]).


%% @doc Render a template. This looks up the templates needed, ensures compilation and
%%      returns the rendering result.
%% @todo: map non-binary templates (cat, overrides) to a template name (use runtime routine)
-spec render(Template :: template(), Vars :: #{}, Options :: options(), Context :: term()) ->
        {ok, render_result()} | {error, term()}.
render(Template0, Vars, Options, Context) ->
    Template = normalize_template(Template0), 
    % Check _admin to map template to module (compile if needed)
    case block_lookup(Template, #{}, [], Options, Context) of
        {ok, BaseModule, ExtendsStack, BlockMap} ->
            % Start with the render function of the "base" template
            % Optionally add the unique prefix for this rendering.
            Vars1 = case BaseModule:is_autoid() 
                        orelse lists:any(fun(M) -> M:is_autoid() end, ExtendsStack)
                    of
                        true ->
                            Vars#{
                                '$autoid' => template_compiler_runtime_internal:unique()
                            };
                        false ->
                            Vars
                    end,
            {ok, BaseModule:render(Vars1, BlockMap, Context)};
        {error, _} = Error ->
            Error
    end.

%% @doc Map all string() template names to binary().
-spec normalize_template(template()) -> template().
normalize_template(Template) when is_binary(Template) ->
    Template;
normalize_template({cat, Template} = T) when is_binary(Template) ->
    T;
normalize_template({overrides, Template, _Filename} = T) when is_binary(Template) -> 
    T;
normalize_template(Template) when is_list(Template) ->
    unicode:characters_to_binary(Template);
normalize_template({cat, Template}) when is_list(Template) ->
    {cat, unicode:characters_to_binary(Template)};
normalize_template({overrides, Template, Filename}) when is_list(Template) -> 
    {overrides, unicode:characters_to_binary(Template), Filename}.

%% @doc Recursive lookup of blocks via the extends-chain of a template.
block_lookup(Template, BlockMap, ExtendsStack, Options, Context) ->
    case template_compiler_admin:lookup(Template, Options, Context) of
        {ok, Module} ->
            case lists:member(Module, ExtendsStack) of
                true ->
                    {error, {recursion, [Module:filename() | [ M:filename() || M <- ExtendsStack ]]}};
                false ->
                    % Check extended/overruled templates (build block map)
                    BlockMap1 = add_blocks(Module:blocks(), Module, BlockMap),
                    case Module:extends() of
                        undefined ->
                            {ok, Module, ExtendsStack, BlockMap1};
                        overrides ->
                            Next = {overrides, Template, Module:filename()},
                            block_lookup(Next, BlockMap1, [Module|ExtendsStack], Options, Context);
                        Extends when is_binary(Extends) ->
                            block_lookup(Extends, BlockMap1, [Module|ExtendsStack], Options, Context)
                    end
            end;
        {error, _} = Error ->
            Error
    end.

add_blocks([], _Module, BlockMap) ->
    BlockMap;
add_blocks([Block|Blocks], Module, BlockMap) ->
    List = maps:get(Block, BlockMap, []),
    BlockMap1 = BlockMap#{ Block => List ++ [Module]},
    add_blocks(Blocks, Module, BlockMap1).


%% @doc Extract the runtime to be used from the options.
-spec get_option(Option :: atom(), Options :: options()) -> term().
get_option(runtime, Options) ->
    proplists:get_value(runtime, Options, template_compiler_runtime).


%% @doc Find the module of a compiled template, if not yet compiled then
%% compile the template.
-spec lookup(binary(), options(), term()) -> {ok, atom()} | {error, term()}.
lookup(Filename, Options, Context) ->
    template_compiler_admin:lookup(Filename, Options, Context).


%% @doc Ping that a template has been changed
-spec flush_file(filename:filename()) -> ok.
flush_file(Filename) ->
    template_compiler_admin:flush_file(Filename).

%% @doc Ping that a template has been changed
-spec flush_context_name(ContextName::term()) -> ok.
flush_context_name(ContextName) ->
    template_compiler_admin:flush_context_name(ContextName).


%% @doc Compile a template to a module. The template is the path of the
%% template to be compiled.
-spec compile_file(filename:filename(), options(), term()) -> {ok, atom()} | {error, term()}.
compile_file(Filename, Options, Context) ->
    case file:read_file(Filename) of
        {ok, Tpl} ->
            compile_binary(Tpl, Filename, Options, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Compile a in-memory template to a module.
-spec compile_binary(binary(), filename:filename(), options(), term()) -> {ok, atom()} | {error, term()}.
compile_binary(Tpl, Filename, Options, Context) when is_binary(Tpl) ->
    case template_compiler_scanner:scan(Filename, Tpl) of
        {ok, Tokens} ->
            Runtime = get_option(runtime, Options),
            Tokens1 = maybe_drop_text(Tokens, Tokens),
            Tokens2 = expand_translations(Tokens1, Runtime, Context),
            Module = module_name(Runtime, Tokens2),
            case erlang:module_loaded(Module) of
                true ->
                    {ok, Module};
                false ->
                    case compile_tokens(template_compiler_parser:parse(Tokens2), cs(Module, Filename, Options, Context)) of
                        {ok, {Extends, BlockAsts, TemplateAst, IsAutoid}} ->
                            Forms = template_compiler_module:compile(
                                                Module, Filename, IsAutoid, Runtime, 
                                                Extends, BlockAsts, TemplateAst),
                            compile_forms(Filename, Forms);
                        {error, _} = Error ->
                            Error
                    end
            end;
        {error, _} = Error ->
            Error
    end.

%%%% --------------------------------- Internal ----------------------------------

module_name(Runtime, Tokens) ->
    TokenChecksum = crypto:hash(sha, term_to_binary({?COMPILER_VERSION, Runtime, Tokens})),
    Hex = z_string:to_lower(z_url:hex_encode(TokenChecksum)),
    binary_to_atom(iolist_to_binary(["tpl_",Hex]), 'utf8').

compile_forms(Filename, Forms) ->
    % case compile:forms(Forms, [nowarn_shadow_vars]) of
    Forms1 = [ erl_syntax:revert(Form) || Form <- Forms ],
    case compile:forms(Forms1, [report_errors]) of
        Compiled when element(1, Compiled) =:= ok ->
            [ok, Module, Bin | _Info] = tuple_to_list(Compiled),
            code:purge(Module),
            case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin) of
                {module, Module} ->
                    {ok, Module};
                Error ->
                    lager:error("Error loading compiling forms for ~p: ~p",
                                [Filename, Error]),
                    Error
            end;
        error ->
            lager:error("Error compiling forms for ~p", [Filename]),
            {error, {compile, []}};
        {error, Es, Ws} ->
            lager:error("Errors compiling ~p: ~p  (warnings ~p)",
                        [Filename, Es, Ws]),
            {error, {compile, Es, Ws}}
    end.

cs(Module, Filename, Options, Context) ->
    #cs{
        filename=Filename,
        module=Module,
        runtime=proplists:get_value(runtime, Options, template_compiler_runtime),
        context=Context
    }.

compile_tokens({ok, {extends, {string_literal, _, Extend}, Elements}}, CState) ->
    Blocks = find_blocks(Elements),
    {Ws, BlockAsts} = compile_blocks(Blocks, CState),
    {ok, {Extend, BlockAsts, undefined, Ws#ws.is_autoid_var}};
compile_tokens({ok, {overrides, Elements}}, CState) ->
    Blocks = find_blocks(Elements),
    {Ws, BlockAsts} = compile_blocks(Blocks, CState),
    {ok, {overrides, BlockAsts, undefined, Ws#ws.is_autoid_var}};
compile_tokens({ok, {base, Elements}}, CState) ->
    Blocks = find_blocks(Elements),
    {Ws, BlockAsts} = compile_blocks(Blocks, CState),
    CStateElts = CState#cs{blocks = BlockAsts},
    {Ws1, TemplateAsts} = template_compiler_element:compile(Elements, CStateElts, Ws),
    {ok, {undefined, BlockAsts, TemplateAsts, Ws1#ws.is_autoid_var}};
compile_tokens({error, _} = Error, _CState) ->
    Error.

-spec compile_blocks([block_element()], #cs{}) -> {#ws{}, [{atom(), erl_syntax:syntaxTree()}]}.
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
    BlockName = template_compiler_utils:to_atom(Name),
    {Ws1, Body} = template_compiler_element:compile(Elts, CState#cs{block=BlockName}, reset_block_ws(Ws)),
    {Ws1, {BlockName, Body, Ws1}}.

reset_block_ws(Ws) ->
    Ws#ws{is_forloop_var=false}.


%% @doc Extract all block definitions from the parse tree, returns deepest nested blocks first
find_blocks(Elements) ->
    find_blocks(Elements, []).

find_blocks(List, Acc) when is_list(List) ->
    lists:foldl(fun find_blocks/2, Acc, List);
find_blocks({block, _Name, Elements} = Block, Acc) ->
    find_blocks(Elements, [Block|Acc]);
find_blocks(Element, Acc) ->
    find_blocks(block_elements(Element), Acc).

block_elements({for, _, Loop, Empty}) -> [Loop,Empty];
block_elements({'if', _, If, Else}) -> [If, Else];
block_elements({spaceless, Elts}) -> Elts;
block_elements({autoescape, _, Elts}) -> Elts;
block_elements({with, _, Elts}) -> Elts;
block_elements({cache, _, Elts}) -> Elts;
block_elements({javascript, Elts}) -> Elts;
block_elements({filter, _, Elts}) -> Elts;
block_elements(_) -> [].


%% @doc Optionally drop text before {% extends %} or {% overrides %}.
maybe_drop_text([{text, _SrcRef, _Text}|Rest], OrgTks) ->
    maybe_drop_text(Rest, OrgTks);
maybe_drop_text([{comment, _Text}|Rest], OrgTks) ->
    maybe_drop_text(Rest, OrgTks);
maybe_drop_text([{open_tag, _, _}, {extends_keyword, _, _}|_] = Tks, _OrgTks) ->
    Tks;
maybe_drop_text([{open_tag, _, _}, {overrides_keyword, _, _}|_] = Tks, _OrgTks) ->
    Tks;
maybe_drop_text(_, [{open_tag, SrcRef, _}|_] = OrgTks) ->
    [{text, SrcRef, <<>>}|OrgTks];
maybe_drop_text(_, OrgTks) ->
    OrgTks.


%% @doc Expand all translations in the tokens. Translations are always looked up at compile time.
expand_translations(Tokens, Runtime, Context) ->
    [ expand_translation(Token, Runtime, Context) || Token <- Tokens ].

expand_translation({trans_text, SrcPos, Text}, Runtime, Context) ->
    Unescaped = template_compiler_utils:unescape_string_literal(Text),
    Trimmed = z_string:trim(Unescaped),
    case Runtime:get_translations(Trimmed, Context) of
        {trans, _} = Tr -> {trans_text, SrcPos, Tr};
        B when is_binary(B) -> {text, SrcPos, B}
    end;
expand_translation({trans_literal, SrcPos, Text}, Runtime, Context) ->
    Unescaped = template_compiler_utils:unescape_string_literal(Text),
    case Runtime:get_translations(Unescaped, Context) of
        {trans, _} = Tr -> {trans_literal, SrcPos, Tr};
        B when is_binary(B) -> {text, SrcPos, B}
    end;
expand_translation({string_literal, SrcPos, Text}, _Runtime, _Context) ->
    Text1 = template_compiler_utils:unescape_string_literal(Text),
    {string_literal, SrcPos, Text1};
expand_translation(Token, _Runtime, _Context) ->
    Token.
