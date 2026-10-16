# AGENTS.md

Read [MAINTAINERS.md](MAINTAINERS.md) first: it has the architecture, invariants, build/test commands and conventions.

Quick checks before finishing any change:

```sh
dotnet build fss.slnx      # expect 0 warnings
dotnet test fss.slnx       # expect 0 failures (Postgres/MySQL skip unless FSS_TEST_POSTGRES / FSS_TEST_MYSQL are set)
```
