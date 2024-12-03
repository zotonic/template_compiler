{{ true | template_compiler_test:"w" }}
{{ false | template_compiler_test:"w" }}
{{ undefined | template_compiler_test:"w" }}
{{ 42 | template_compiler_test:"w" }}
{{ 1.61803 | template_compiler_test:"w" }}
{{ "hello world" | template_compiler_test:"w" }}
{{ `atom` | template_compiler_test:"w" }}
{{ [`a`, `b`, `c`] | template_compiler_test:"w" }}
{{ %{ a : 1, b: 2, c: 3.14159 } | template_compiler_test:"w" }}
