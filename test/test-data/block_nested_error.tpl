{% block main %}
    Hallo
    {% block test %}
        Daar
        {% block main %}
            oops - a block with the same name
        {% endblock %}
    {% endblock %}
{% endblock%}
