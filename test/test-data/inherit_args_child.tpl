{% extends "inherit_args_parent.tpl" %}{% block hello %}Bye {% inherit with who="World" %}{% endblock %}
