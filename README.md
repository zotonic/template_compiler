Template Compiler for Erlang / Elixir
=====================================

[![Build Status](https://travis-ci.org/zotonic/template_compiler.svg?branch=master)](https://travis-ci.org/zotonic/template_compiler)
[![Join the chat at https://gitter.im/zotonic/zotonic](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/zotonic/zotonic?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This is the template compiler used by [The Erlang Content Management Framework and CMS Zotonic](http://zotonic.com/).

This compiler is a complete rewrite of the erlydtl fork used in Zotonic. In contrast with ErlyDTL the template_compiler generates small Erlang modules which are shared between different templates and sites.

Template Language
-----------------

The template language is largely the same as Django Template Language with some additions:

 * Runtime includes, all templates are compiled to separate BEAM modules
 * `overrules` to override same-named templates
 * translation tags `{_ my translatable text _}` and `{% include "a.tpl" arg=_"my translatable text" %}`


Runtime Just in Time compilation
---------------------------------

The template_compiler is a runtime compiler. It compiles templates from source files to in-memory BEAM modules.
Templates are re-compiled if the source file changes or new translation strings are loaded.

Templates are compiled to functions containinge entry point to all blocks.

A template like:

    Hello
    {% block a %}this is block a{% endblock %}
    World
    {% block b %}this is block b{% endblock %}

Will be compiled to an Erlang module with the following structure:

    -module(tpl_7bae8076a5771865123be7112468b79e9d78a640).
    -export([
        render/3,
        render_block/4,
        timestamp/0,
        blocks/0,
        module/0,
        extends/0,
        filename/0,
        mtime/0,
        is_autoid/0,
        runtime/0
    ]).

    %% The main render function.
    render(Args, Vars, Context) -> [ ... ].

    %% Render functions per block, the 'Blocks' is a block trace used internally.
    render_block(a, Vars, Blocks, Context) -> [ ... ];
    render_block(b, Vars, Blocks, Context) -> [ ... ];
    render_block(_, _Vars, _Blocks, _Context) -> <<>>.

    %% Timestamp on module compilation (os:timestamp/0)
    timestamp() -> {1574,685843,340548}.

    %% Block defined in this template.
    blocks() -> [ a, b ].

    %% The module name
    module() -> tpl_7bae8076a5771865123be7112468b79e9d78a640.

    %% The template that this template extends on.
    extends() -> undefined.

    %% The filename of this template
    filename -> <<"foor/bar/a.tpl">>.

    %% The modification time of the template file on compilation
    mtime() - {{2019,1,31},{11,51,49}}.

    %% Flag if the autoid ("#id") construct is used in this template.
    is_autoid() -> false.

    %% Runtime module this template is linked against.
    runtime() -> template_compiler_runtime.


The module includes debug information so that stack traces show the correct template file and line number.


Runtime Module
--------------

The template_compiler uses a runtime module which implements template lookup and vatiable resolution methods.
There is a default runtime module in `template_compiler_runtime`.


How to use
----------

Render a template to an iolist:

    Vars = #{ a => 1 },                 % Template variables
    Options = [],                       % Render and compilation options
    Context = your_request_context,     % Context passed to the runtime module and filters
    {ok, IOList} = template_compiler:render("hello.tpl", Vars, Options, Context).

The `render/4` function looks up the template, ensures it is compiled, and then calls the compiled render
function of the template or the templates it extends (which are also compiled etc.).


Use `compile_file/3` and `compile_binary/4` to compile a template file or in-memory binary to a module:

    {ok, Module} = template_compiler:compile_binary(<<"...">>, Filename, Options, Context).

The `Filename` is used to reference the compiled binary.


Force recompilation
...................

Sometimes (for example when template lookups or translations are changed) it is necessary to check all templates if they
need to be recompiled.

For this are the flush functions:

 * `template_compiler:flush()` forces a check on all templates.
 * `template_compiler:flush_file(TemplateFile)` forces check on the given template.
 * `template_compiler:flush_context(Context)` forces check on templates compiled within the given request context.

The template_compiler parses the templates till their _Form_ format and then calculates a checksum. If the checksum
is not changed from the previous compilation then the _Form_ is not compiled, sparing valuable processing time.


Template language and Tags
==========================

... TODO: explain the language constructs ...



License
=======

The template_compiler is released under the APLv2 license. See the file LICENSE for the exact terms.
