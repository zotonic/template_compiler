{% fragment mytest %}{% if arg %}true{% elseif not arg %}false{% endif %}{% endfragment %}{% use mytest arg=true %},{% use mytest arg=false %}
