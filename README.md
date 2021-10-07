Template Compiler for Erlang / Elixir
=====================================

![Test](https://github.com/zotonic/template_compiler/workflows/Test/badge.svg)
[![Join the chat at https://gitter.im/zotonic/zotonic](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/zotonic/zotonic?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This is the template compiler used by [The Erlang Content Management Framework and CMS Zotonic](http://zotonic.com/).

This compiler is a complete rewrite of the erlydtl fork used in Zotonic. In contrast with ErlyDTL the template_compiler generates small Erlang modules which are shared between different templates and sites.

Building
--------

Run `make`.
To test, run: `make test`

To use in your project with rebar3, use the [Hex package](https://hex.pm/packages/template_compiler):

```erlang
{deps, [
    template_compiler
]}.
```

Or directly from git:

```erlang
{deps, [
    {template_compiler, {git, "https://github.com/zotonic/template_compiler", {branch, "master"}}}.
]}.
```

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

```django
Hello
{% block a %}this is block a{% endblock %}
World
{% block b %}this is block b{% endblock %}
```

Will be compiled to an Erlang module with the following structure:


```erlang
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
render(Args, BlockMap, Context) -> [ ... ].

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
```

The module includes debug information so that stack traces show the correct template file and line number.


Runtime Module
--------------

The template_compiler uses a runtime module which implements template lookup and vatiable resolution methods.
There is a default runtime module in `template_compiler_runtime`.


How to use
----------

Render a template to an iolist:

```erlang
Vars = #{ <<"a">> => 1 },                 % Template variables, use a map
Options = [],                       % Render and compilation options
Context = your_request_context,     % Context passed to the runtime module and filters
{ok, IOList} = template_compiler:render("hello.tpl", Vars, Options, Context).
```

The `render/4` function looks up the template, ensures it is compiled, and then calls the compiled render
function of the template or the templates it extends (which are also compiled etc.).


Use `compile_file/3` and `compile_binary/4` to compile a template file or in-memory binary to a module:

```erlang
{ok, Module} = template_compiler:compile_binary(<<"...">>, Filename, Options, Context).
```

The `Filename` is used to reference the compiled binary.


#### Force recompilation

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

**For more complete documentaion see Zotonic: http://docs.zotonic.com/en/latest/developer-guide/templates.html**

Below is a short list of tags and explanation of template include / extend.

The `template_compiler` package doesn't include filters.

Variables
---------

Variables are surrounded by `{{` and `}}` (double braces):

```django
Hello, I’m `{{ first_name }} {{ last_name }}`.
```

When rendering this template, you need to pass the variables to it. If you pass “James” for
`first_name` and “Bond” for `last_name`, the template renders to:

```
Hello, I’m James Bond.
```

Instead of strings, variables can also be objects that contain attributes. To access the attributes, use dot notation:

```django
{{ article.title }} was created by {{ article.author.last_name }}
```

The `article` could have been passed as a proplist or map in the _Vars_ for the template render function:

```erlang
#{
    <<"article">> => #{
        <<"title">> => <<"My title"/utf8>>,
        <<"author">> => #{
            <<"id">> => 1234,
            <<"last_name">> => <<"Janssen">>
        }
    }
}.
```

Values and expressions
----------------------

Expressions can use different values types:

 * Variables (see above)
 * Number: `123`
 * String: `"hello"` or `'hello'`
 * Translatable string: `_"Hello"` (see below)
 * List:  `[ 1, 2, 3 ]`
 * Map, using Elixir syntax: `%{ a: 1, b: 2 }` where the keys will become binary strings
   equivalent to the Erlang map: `#{ <<"a">> => 1, <<"b">> => 2 }`
 * Map, using strings as keys: `%{ "foaf:name":"Hello" }`
 * Erlang atom: <tt>&grave;a&grave;</tt> (quoted using backticks)
 * Tagged value list: `{mytag a=1 b=2}`, this translates to Erlang `{mytag, [{a,1}, {b,2}]}`
 * Unique generated id: `#foo` - an unique prefix is used for each template


Translatable texts
------------------

There are three ways translatable texts can be used.

As a tag embedded in the text of the template:

```django
{_ This text is translatable _}
```

As double quoted string prefixed with a '_', where you could also use a string:

```django
{% include "_a.tpl" title=_"Translatable text" %}
```

As a text with arguments:

```django
{% trans "Hello {foo}, Bye" foo=author.name_full %}
```

Note: to show a `{` or `}` in a `trans` tag text then double it to `{{`.

The `trans` tag is compiled to code so very efficient.


#### List translatable strings

To get a list of all translatable texts in a template use:

```erlang
template_compiler:translations(TemplateFilename).
```

This returns a list of all strings, which could be used to generate a .po file:

```erlang
[
    {<<"Hello {foo}, Bye">>, [], {<<"foo/bar.tpl">>, 1234, 5}}
].
```

Template include / extends
--------------------------

Templates can be combined to re-use parts and keep everything manageable.

#### Include

First a template can be included in another template:

```django
Hello {% include "_name.tpl" id=foobar %}
```

The template (`_name.tpl` in this case) will be included at the called spot.
All variables known at the spot of the include will be passed, with the addition of
the arguments of the include tags.

Note: in ErlyDTL the included template is compiled inline into the surrounding
template. In template_compiler the included template is compiled as a separate module.

#### Extends

You can also have a _base_ template which can be used as the basis of other templates.
The base template should define some _blocks_ that can be changed in the template
that extends the base template:

```django
{% extends "base.tpl" %}
{% block name %}Piet!{% endblock %}
```

Where the `base.tpl` could be:

```django
Hello {% block name %}...{% endblock %} world.
```

The `{% extends "..." %}` tag _must_ be the first tag in the template. Also any text
outside the block tags will be dropped. So be sure to surround replacable parts in your
base templates with block tags.

There is a variation on `extends` where the template extends on a same-named template
of a lower priority. This is heavily used in Zotonic to extend templates in other modules.

```django
{% overrules %}
{% block name %}...{% endblock %}
```

The `overrules` is used to make it explicit that this template overrules (or extends)
another template with the same name.

#### Block

This defines a named portion of a template that can be replaced in a template that extends
(or overrules) this template:

```django
Some text
{% block myname %}
    Some text in the block that might be replaced
{% endblock %}
And text after the block
```

Blocks can be nested:

```django
Some text
{% block myname %}
    Some text in the block that might be replaced
    {% block nestedblock %}
        Text in the nested block
    {% endblock %}
    And more text in the outer block
{% endblock %}
And text after the block
```

The blocks are compiled to separate functions. The template compiler uses the template
extends chain to figure out which block-function to render from which extending template.

#### Inherit

The `{% inherit %}` tag van be used inside a block to render the same-named block in
the extended (or overruled) template.

If base.tpl is like:

```django
this is {% block a %}the base{% endblock %} template
```

And a.tpl is like:

```django
{% extends "base.tpl" %}
{% block a %}hello {% inherit %} world{% endblock %}
```

Then `a.tpl` renders like:

```
this is hello the base world template
```

#### If tag

Conditionally show or hide parts of a template:

```django
{% if somevar %}
    True
{% elif othervar %}
    Other var
{% else %}
    False
{% endif %}
```

The `elif` can also be written as `elseif`

#### With tag

Define a variable to be used within an enclosed part of the template:

```django
{% with someexpr as v %}
    Here v can be used as any other variable
{% endwith %}
```

This is useful for using the result of a complicated expression multiple times
or if a `forloop` (see below) iterator needs to be used in overruled blocks or
included templates.

#### For tag

Loop over a list of values, printing a comma separated list:

```django
{% for k in [ 1, 2, 3, 4 ] %}
    {{ k }}{% if nor forloop.last %},{% endif %}
{% endfor %}
```

Loop over a list of tuples (eg. `[ {a, 1}, {b, 2} ]`):

```django
<table>
{% for key, value in someproplist %}
    <tr>
        <th>{{ key|escape }}</th>
        <td>{{ value|escape }}</td>
    </tr>
{% empty %}
    <tr>
        <td>{_ Sorry, no values _}</td>
    </tr>
{% endfor %}
</table>
```

Within a forloop there is an iterator called `forloop` with the following
properties:

 * `forloop.first` true if this is the first iteration
 * `forloop.last` true if this is the last iteration
 * `forloop.counter` iteration counter, starts at `1`
 * `forloop.counter0` iteratiomn counter, starts at `0`
 * `forloop.revcounter` iteration counter, starts at `N`, ends at `1`
 * `forloop.revcounter0` iteration counter, starts at `N-1`, ends at `0`
 * `forloop.parentloop` the `forloop` paramater of the enclosing _for_ loop (if any)

**NOTE** the `forloop` iterator is only available in the template that contains
the `for` loop, it is not passed to any included or extending templates.
If you want to use it in a block or included template then explicitly assign it to
a variable using `with` or include arguments.

If the template compiler does sees that the `forloop` is not used inside the template
where the forloop is defined then the code for tracking the forloop iterators is not
generated.


#### Comment tag

Everything surrounded by the tag is excluded:

```django
{% comment %}
    Some explanation that will not be part of the compiled template
{% endcomment %}
```

Useful to disable parts of a template or add some explanatory texts.


#### Raw tag

To echo some part without rendering it:

```django
{% raw %}
    {{ hello }} {% this is }} %} echo'd as-is
{% endraw %}
```

#### Spaceless tag

Remove spaces between HTML tags:

```django
a-{% spaceless %} <a> x<span>xxx </span> </a> {% endspaceless %}-b
```

Renders as:

```html
a-<a> x<span>xxx </span></a>-b
```

#### Print tag

Print a value in `<pre>` tags, used to inspect variables or dump some value
when you are still writing the template:

```django
{% print somexpr %}
```

The expression value will be printed using `io_lib:format("~p", [ SomExpr ])`, then
escaped and surrounded with `<pre>...</pre>`.


License
=======

The template_compiler is released under the APLv2 license. See the file LICENSE for the exact terms.
