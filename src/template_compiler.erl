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
    lookup/3,
    compile_file/3,
    compile_binary/4
    ]).

-include_lib("syntax_tools/include/merl.hrl").
-include("template_compiler.hrl").

-type option() :: {runtime, atom()}.

-type options() :: list(option()).

-export_type([
        option/0,
        options/0
    ]).


%% @doc Find the module of a compiled template, if not yet compiled then
%% compile the template.
-spec lookup(binary(), options(), any()) -> {ok, atom()} | {error, any()}.
lookup(Filename, Options, Context) ->
    template_compiler_admin:lookup(Filename, Options, Context).

%% @doc Compile a template to a module. The template is the path of the
%% template to be compiled.
-spec compile_file(binary(), options(), any()) -> {ok, atom()} | {error, any()}.
compile_file(Filename, Options, Context) ->
    case file:read_file(Filename) of
        {ok, Tpl} ->
            compile_binary(Tpl, Filename, Options, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Compile a in-memory template to a module.
-spec compile_binary(binary(), binary(), options(), any()) -> {ok, atom()} | {error, any()}.
compile_binary(Tpl, Filename, Options, Context) ->
    case template_compiler_scanner:scan(Filename, Tpl) of
        {ok, Tokens} ->
            Tokens1 = maybe_drop_text(Tokens, Tokens),
            Tokens2 = expand_translations(Tokens1, proplists:get_value(runtime, Options, template_compiler_runtime), Context),
            Module = module_name(Tokens2),
            case erlang:module_loaded(Module) of
                true ->
                    {ok, Module};
                false ->
                    case compile_tokens(template_compiler_parser:parse(Tokens2), cs(Filename, Options, Context)) of
                        {ok, {Extends, BlockAsts, TemplateAst}} ->
                            Forms = template_compiler_module:compile(Module, Filename, Extends, BlockAsts, TemplateAst),
                            compile_forms(Filename, Forms);
                        {error, _} = Error ->
                            Error
                    end
            end;
        {error, _} = Error ->
            Error
    end.

%%%% --------------------------------- Internal ----------------------------------

module_name(Tokens) ->
    TokenChecksum = crypto:hash(sha, term_to_binary({?COMPILER_VERSION, Tokens})),
    Hex = z_string:to_lower(z_url:hex_encode(TokenChecksum)),
    binary_to_atom(iolist_to_binary(["tpl_",Hex]), 'utf8').

compile_forms(Filename, Forms) ->
    % case compile:forms(Forms, [nowarn_shadow_vars]) of
    Forms1 = [ erl_syntax:revert(Form) || Form <- Forms ],
    io:format("~p", [Forms1]),
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

cs(Filename, Options, Context) ->
    #cs{
        filename=Filename,
        runtime=proplists:get_value(runtime, Options, template_compiler_runtime),
        context=Context
    }.

compile_tokens({ok, {extends, Extend, Elements}}, CState) ->
    Blocks = find_blocks(Elements),
    {_Ws, BlockAsts} = compile_blocks(Blocks, CState),
    {ok, {Extend, BlockAsts, undefined}};
compile_tokens({ok, {overrides, Elements}}, CState) ->
    Blocks = find_blocks(Elements),
    {_Ws, BlockAsts} = compile_blocks(Blocks, CState),
    {ok, {overrides, BlockAsts, undefined}};
compile_tokens({ok, {base, Elements}}, CState) ->
    Blocks = find_blocks(Elements),
    {Ws, BlockAsts} = compile_blocks(Blocks, CState),
    {_Ws, TemplateAsts} = template_compiler_element:compile(Elements, CState, Ws),
    {ok, {undefined, BlockAsts, TemplateAsts}};
compile_tokens({error, _} = Error, _CState) ->
    Error.

-spec compile_blocks([block_element()], #cs{}) -> {#ws{}, [{atom(), erl_syntax:syntaxTree()}]}.
compile_blocks(Blocks, CState) ->
    Ws = #ws{},
    lists:foldl(
        fun(Block, {WsAcc, BlockAcc}) ->
            {WsAcc1, B} = compile_block(Block, CState, WsAcc),
            {WsAcc1, [B|BlockAcc]}
        end,
        {Ws,[]},
        Blocks).


%% @doc Compile a block definition to a function name and its body elements.
-spec compile_block(block_element(), #cs{}, #ws{}) -> {#ws{}, {atom(), erl_syntax:syntaxTree()}}.
compile_block({block, {identifier, _Pos, Name}, Elts}, CState, Ws) ->
    {Ws1, Body} = template_compiler_element:compile(Elts, CState, Ws),
    {Ws1, {template_compiler_utils:to_atom(Name), Body}}.


%% @doc Extract all block definitions from the parse tree, keep the tree as-is.
find_blocks(Elements) ->
    find_blocks(Elements, []).

find_blocks(List, Acc) when is_list(List) ->
    lists:foldl(fun find_blocks/2, Acc, List);
find_blocks({block, _Name, Elements} = Block, Acc) ->
    [ Block| find_blocks(Elements, Acc) ];
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
