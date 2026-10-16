# Templates (`Fss.Template`): legacy

A small subset of [Jinja2](https://jinja.palletsprojects.com/), part of the legacy `fss` package. It works
without the web server, for anything that needs text generated from data.

> **Output is not HTML-escaped.** `{{ x }}` inserts `x` verbatim. Encode untrusted text
> (`WebUtility.HtmlEncode`) before you render it into HTML.

## Rendering

```fsharp
open Fss.Template

// From a string
let t = Template("Hello {{ name }}, you have {{ count }} messages")
let page = t.Render([| ("name", box "Ada") ; ("count", box 3) |])

// From a folder (needed for include / extends)
let ts = TemplateServer("templates")
let page2 = ts.Get("index.html").Render([| ("people", box people) |])
```

`Render` takes `(name, value)` pairs, or a record/object whose fields become the variables.

## Syntax

| Construct | Example |
|---|---|
| Variable | `{{ name }}` |
| Field / property | `{{ person.name }}`, also chained: `{{ order.customer.city }}` |
| Array index | `{{ rows[0] }}`, `{{ grid[i][j] }}` |
| Arithmetic | `{{ a + b * 2 }}` (`+ - * / %`, parentheses) |
| If / else | `{% if count > 0 %}...{% elseif count == 0 %}...{% else %}...{% endif %}` |
| Comparison | `==`, `!=`, `<`, `<=`, `>`, `>=` on numbers and strings (`'single'` or `"double"` quotes) |
| Boolean | `and`, `or`, `not`, parentheses. A variable is false only if it is `false`, `""` or an empty array; numbers (including `0`) are true. See also the note on missing variables below. |
| For loop | `{% for p in people %}{{ p.name }}{% endfor %}`. Arrays and .NET collections work; F# `list` doesn't, so pass an array. |
| Range | `{% for i in range(1,10) %}` (inclusive). With a step: `range(start, step, end)`, e.g. `range(1,2,9)` gives 1 3 5 7 9. |
| Include | `{% include "header.html" %}` (needs `TemplateServer`) |
| Inheritance | Child: `{% extends "base.html" %}` and `{% block content %}...{% endblock %}`. Base: `{% block content %}default{% endblock %}`. |
| Raw | `{% raw %}{{ not interpreted }}{% endraw %}` |

## Differences from Jinja2

- No filters (`{{ x | upper }}`), macros, `set`, or tests (`is defined`).
- No auto-escaping (see the warning above).
- `range` is inclusive, and with three arguments the order is `(start, step, end)`.
- Floats render with 6 decimal places, and arrays render `;`-separated. Format values in F# first if
  you need control.
- A missing variable renders as the text `missing value 'name'` instead of failing. **In an `if` it
  counts as true**, so pass every variable a template tests (e.g. `("isAdmin", box false)`).
