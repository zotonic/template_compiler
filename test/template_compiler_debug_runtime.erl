-module(template_compiler_debug_runtime).

-export([
    map_template/3,
    map_template_all/3,
    is_modified/3,
    compile_map_nested_value/3,
    find_nested_value/3,
    find_nested_value/4,
    find_value/4,
    get_context_name/1,
    set_context_vars/2,
    get_translations/2,
    lookup_translation/3,
    model_call/4,
    custom_tag/4,
    builtin_tag/5,
    cache_tag/6,
    javascript_tag/3,
    spaceless_tag/3,
    to_bool/2,
    to_list/2,
    to_simple_value/2,
    to_render_result/3,
    escape/2,
    trace_compile/4,
    trace_render/3,
    trace_debug/3,
    trace_block/4
]).

map_template(Template, Vars, Context) ->
    template_compiler_runtime:map_template(Template, Vars, Context).

map_template_all(Template, Vars, Context) ->
    template_compiler_runtime:map_template_all(Template, Vars, Context).

is_modified(Filename, Mtime, Context) ->
    template_compiler_runtime:is_modified(Filename, Mtime, Context).

compile_map_nested_value(Tokens, ContextVar, Context) ->
    template_compiler_runtime:compile_map_nested_value(Tokens, ContextVar, Context).

find_nested_value(Keys, TplVars, Context) ->
    template_compiler_runtime:find_nested_value(Keys, TplVars, Context).

find_nested_value(BaseValue, Keys, TplVars, Context) ->
    template_compiler_runtime:find_nested_value(BaseValue, Keys, TplVars, Context).

find_value(Key, Vars, TplVars, Context) ->
    template_compiler_runtime:find_value(Key, Vars, TplVars, Context).

get_context_name(Context) ->
    template_compiler_runtime:get_context_name(Context).

set_context_vars(Vars, Context) ->
    template_compiler_runtime:set_context_vars(Vars, Context).

get_translations(Text, Context) ->
    template_compiler_runtime:get_translations(Text, Context).

lookup_translation(Tr, TplVars, Context) ->
    template_compiler_runtime:lookup_translation(Tr, TplVars, Context).

model_call(Model, Path, Payload, Context) ->
    template_compiler_runtime:model_call(Model, Path, Payload, Context).

custom_tag(Module, Args, TplVars, Context) ->
    template_compiler_runtime:custom_tag(Module, Args, TplVars, Context).

builtin_tag(Tag, Expr, Args, TplVars, Context) ->
    template_compiler_runtime:builtin_tag(Tag, Expr, Args, TplVars, Context).

cache_tag(Seconds, Name, Args, Fun, TplVars, Context) ->
    template_compiler_runtime:cache_tag(Seconds, Name, Args, Fun, TplVars, Context).

javascript_tag(Value, TplVars, Context) ->
    template_compiler_runtime:javascript_tag(Value, TplVars, Context).

spaceless_tag(Value, TplVars, Context) ->
    template_compiler_runtime:spaceless_tag(Value, TplVars, Context).

to_bool(Value, Context) ->
    template_compiler_runtime:to_bool(Value, Context).

to_list(Value, Context) ->
    template_compiler_runtime:to_list(Value, Context).

to_simple_value(Value, Context) ->
    template_compiler_runtime:to_simple_value(Value, Context).

to_render_result(Value, TplVars, Context) ->
    template_compiler_runtime:to_render_result(Value, TplVars, Context).

escape(Value, Context) ->
    template_compiler_runtime:escape(Value, Context).

trace_compile(Module, Filename, Options, Context) ->
    template_compiler_runtime:trace_compile(Module, Filename, Options, Context).

trace_render(Filename, Options, Context) ->
    template_compiler_runtime:trace_render(Filename, Options, Context).

trace_debug(SrcPos, Vars, Context) ->
    case Context of
        #{trace_pid := Pid} when is_pid(Pid) ->
            Pid ! {trace_debug, SrcPos, Vars};
        _ ->
            ok
    end,
    ok.

trace_block(SrcPos, Name, Module, Context) ->
    template_compiler_runtime:trace_block(SrcPos, Name, Module, Context).
