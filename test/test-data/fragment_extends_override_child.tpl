{% extends "fragment_extends_parent.tpl" %}{% fragment greeting %}Hi {{ name }}{% endfragment %}{% block main %}{% use greeting name="child" %}{% endblock %}
