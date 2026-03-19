{% block main %}
{% filter template_compiler_test %}
{% block main %}oops{% endblock %}
{% endfilter %}
{% endblock %}
