# fss (legacy web server and templates)

A small F# HTTP server with regex routing, plus a Jinja2-style templating engine.

**Legacy:** it gets maintenance fixes only. For new web projects use ASP.NET Core (Giraffe, Falco). Read the
security notes before exposing it to a network. In particular, templates do not HTML-escape output and
there is no TLS: https://github.com/daz10000/fss/blob/master/docs/web-server.md

Looking for the database wrappers? They are separate packages: `fss_duckdb`, `fss_postgres`, `fss_sqlite`.
