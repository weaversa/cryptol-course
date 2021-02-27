{% macro spaced(x) -%}
{{ ' {} '.format(x) if x else '' }}
{%- endmacro -%}

{%- macro navlink(x) %}
{%- if x -%}
[ {{ ('{label} {icon}' if x.after else '{icon} {label}').format(**x) }} ]({{ x.path }})
{%- endif -%}
{%- endmacro -%}

{%- macro navbar(center, left=None, right=None) -%}
|{{ spaced(left) }}|{{ spaced(center) }}|{{ spaced(right) }}|
{%- endmacro -%}

# From here, you can go somewhere!

||||
|-:|:-:|-|
{%- if up %}
{{ navbar(navlink(up)) }}
{%- endif -%}
{%- if current %}
{{ navbar('**{}**'.format(current), navlink(previous), navlink(next_)) }}
{%- endif -%}
{%- if start %}
{{ navbar(navlink(start)) }}
{%- endif -%}
{%- if answer_or_exercise %}
{{ navbar(navlink(answer_or_exercise)) }}
{%- endif -%}
{%- for branch in branches %}
{{ navbar(navlink(branch)) }}
{%- endfor -%}
