{% fragment greeting %}Hello {{ name }}{% endfragment %}{% block main %}{% use greeting name="parent" %}{% endblock %}
