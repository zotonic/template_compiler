{# Same as 4, but without a comma in the map field list
#}{% with %{ "a":1 b:2 } as x %}{{ x['a'] }}{% endwith %}