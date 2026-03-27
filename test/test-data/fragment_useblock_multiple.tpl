{% fragment panel_a %}<section>{% block title %}Base A{% endblock %}<div>{{ _body }}</div></section>{% endfragment %}
{% fragment panel_b %}<section>{% block title %}Base B{% endblock %}<div>{{ _body }}</div></section>{% endfragment %}
{% useblock panel_a %}{% block title %}A1 {% inherit %}{% endblock %}Body A{% enduseblock %}{% useblock panel_b %}{% block title %}B1 {% inherit %}{% endblock %}Body B{% enduseblock %}
