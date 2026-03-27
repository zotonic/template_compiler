{% fragment item %}[{{ value }}]{% endfragment %}{% fragment panel %}<div>{{ value }}:{{ _body }}</div>{% endfragment %}{% use item %}{% useblock panel %}Body{% enduseblock %}
