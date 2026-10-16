# MAINTAINERS.md

Orientation for maintainers and AI coding agents. Read this before changing code. It records what
isn't obvious from the code: the architecture, the invariants, the traps, and the reasons behind
decisions. User-facing documentation lives in [`docs/`](docs/).

## What this repo is

**fss** is a set of small F# libraries, published to NuGet:

| Package | Source | Role | Status |
|---|---|---|---|
| `fss_duckdb` | `src/fss_duckdb/db_duckdb.fs` | DuckDB dialect | **Primary use case** |
| `fss_postgres` | `src/fss_postgres/db_pg.fs` | Postgres dialect (Npgsql) | Maintained |
| `fss_sqlite` | `src/fss_sqlite/db_sqlite.fs` | SQLite dialect (Microsoft.Data.Sqlite) | Maintained |
| `fss_mysql` | `src/fss_mysql/` + `src/fss/db_common_mysql.fs` | Old fork of the core for MySQL | **Retired**: builds, not packed or published |
| `fss` | `src/fss/*.fs` except `db_common*.fs` | HTTP server, Jinja-like templates, JSON parser, multipart | **Legacy**: maintenance and security fixes only |

The database packages are a "micro-ORM": SQL in, F# records out (`Query<'T>`), and F# records in
(`InsertOne`/`InsertMany`), with no mapping code. They are independent of the `fss` web package.

## Repo map

```
Directory.Build.props      shared MSBuild settings, *the release version* (<VersionPrefix>), SourceLink, NuGetAudit
Directory.Packages.props   NuGet Central Package Management: every package version lives here
global.json                .NET SDK 10 (rollForward latestFeature)
fss.slnx                   solution (XML .slnx format)
src/Directory.Build.props  packaging metadata for the libraries (license, icon, readme, symbols)
src/fss/                   pool.fs + db_common.fs (THE SHARED DB CORE) and the legacy web/template code
src/fss_<db>/              one dialect file each + README.md (the NuGet package readme)
tests/fss.Tests/           NUnit 5. One file per area; test_db_duckdb.fs is the best model for new tests
samples/                   runnable console apps; CI runs them as smoke tests
docs/                      user docs (markdown); docs/releasing.md is the release runbook
.github/workflows/         ci.yml (build/test/samples/pack), release.yml (tag -> NuGet via Trusted Publishing)
```

## Architecture of the database packages

**The shared core is compiled into each package; it isn't a package of its own.** Each dialect
`.fsproj` compiles `../fss/pool.fs` and `../fss/db_common.fs` as linked source files. So `fss_duckdb.dll`,
`fss_postgres.dll` and `fss_sqlite.dll` each contain their *own copy* of the `Fss.Pool` and
`Fss.Data.Common` types. Consequences:

- A change to `db_common.fs` ships in every dialect. Test it against all of them (DuckDB and SQLite
  always run; Postgres needs `FSS_TEST_POSTGRES`).
- Referencing two dialects in one project works, as `samples/SqliteToDuckDb` shows, because users
  reach the types through the module abbreviations (`Fss.Data.DuckDB.DynamicSqlConnection`). If a user
  `open`s `Fss.Data.Common` directly with two dialects referenced, the types are ambiguous. A
  `fss_data_core` package would fix this, but it means one more package to version and publish, so it
  hasn't been done.

**Flow:**

- `DynamicSqlConnection<'Conn,'Parameter,'Customizer>(connStr, poolSize)` (in `db_common.fs`):
  - Construction creates a `Pool<DbConnection>`. The pool calls `customizer.openConnection(connStr, first)`
    `poolSize` times, eagerly. Then it loads metadata: `loadColDetail` gives
    `Map<SchematizedTable, ColDetail[]>`, and `getSearchPath` gives the schemas used for bare table names.
  - `Query<'T>(sql, ?parameters, ?transProvided)` is a lazy `seq { }`. Inside it, it borrows a command
    (and so a pooled connection), executes, and uses `makeRowBuilder` to decide how to build each row:
    tuple by position, record/class by constructor parameter name, or scalar from column 0. Each value
    goes through `valueConverter` (NULL to option/Nullable handling) and then `coerce` (type
    conversion). The command is disposed, releasing the connection, when enumeration ends.
  - `InsertMany` resolves the table (`findTable`, which refreshes metadata once on a miss and accepts
    `schema.table`), checks that record fields match columns and that option-ness matches nullability,
    builds `insert into schema.table(cols) values(<prefix>p1..) [returning pk]`, and runs one prepared
    command per row inside a transaction. It uses its own transaction, or the caller's `transProvided`.
    If it created the transaction, it disposes it in `finally`, which rolls back on error.
- `DynamicSqlTransaction` holds one pooled connection from construction until `Dispose`. Its methods
  delegate to the connection's methods, passing `transProvided = this`.
- `ISqlConnection` is implemented by both, so user code can run inside or outside a transaction.
- `Customization<'P,'C>` is the dialect interface: `openConnection`, `loadColDetail`, `getSearchPath`,
  `sequenceMechanism` (`RETURNS_CLAUSE` or `SQL_STMT sql`), `parameterPrefix`, `needsKeepAlive`,
  `reopenConnection`, `reloadTypes`.

**Dialect specifics:**

| | DuckDB | Postgres | SQLite |
|---|---|---|---|
| Metadata | `duckdb_columns()`, `duckdb_constraints()` | `pg_class`/`pg_attribute`/`pg_constraint` | `sqlite_master` + `pragma_table_info` |
| Keys | `RETURNING` | `RETURNING` | `select last_insert_rowid()` |
| Param prefix | `$` | `@` | `@` |
| Pool connections | first opened; the rest `Duplicate()` (in-memory) or reopen (file) | via a registered `NpgsqlDataSource` if any (`registerDataSource`), else `new NpgsqlConnection` | `new SqliteConnection` |
| Keep-alive thread | no | yes | no |
| Name case | schema/table lower-cased in metadata | as stored (Postgres folds unquoted names to lower) | lower-cased |

## Invariants and traps (read these)

1. **Every `Pool.Take` must be matched by exactly one `Release`.** `Release` of an item that isn't
   checked out throws `InvalidOperationException`. Use `use cmd = db.Command ...`, or `try/finally`.
   A leaked connection eventually deadlocks callers.
2. **`Query` must stay lazy, and must acquire its connection inside the `seq { }`.** Acquiring it
   outside leaks a connection whenever the sequence isn't enumerated. Regression test:
   `UnenumeratedQueryDoesNotHoldConnection`.
3. **DuckDB `:memory:` needs `Duplicate()`.** Opening the connection string again gives a separate empty
   database. `Duplicate()` returns an *unopened* connection and throws `NotSupportedException` for file
   databases. Regression test: `InMemoryDatabaseIsSharedAcrossPooledConnections`.
4. **DuckDB rejects `:name` parameters.** It needs `$name`. Postgres and SQLite take `@name`. Generated
   SQL uses `customizer.parameterPrefix()`. `DynamicSqlCommand.AddParameter` strips any marker from the
   name, because DuckDB.NET won't match `$x` to `x`.
5. **Microsoft.Data.Sqlite does not implement `GetSchema("Columns")`.** The fss_sqlite breakage of
   2025 came from assuming it did. Don't reintroduce `GetSchema`.
6. **Record mapping uses the constructor with the most parameters, matched by parameter name.** F#
   records and anonymous records name constructor parameters after their fields. Classes work if their
   constructor parameter names match the columns.
7. **`coerce` is the single place for type conversion.** Add new driver/F# type pairs there, with a test
   in `test_db_duckdb.fs`. `None` is represented as `null`, both in `obj[]` constructor arguments and in
   `valueConverter` results.
8. **Metadata is cached.** `findTable` refreshes it once when a table isn't found. `Reload()` refreshes
   it explicitly, and Postgres types too. Column type changes (as opposed to new tables) still need
   `Reload()`.
9. **The keep-alive thread must stay a background thread.** It holds `disposeLock` while pinging.
10. **Npgsql 7+ has no global type mapping.** Enum and plugin mappings must go through
    `Postgres.registerDataSource` before the connection is constructed.
11. **`fss` (web) security debt is documented, not fixed.** Templates don't HTML-escape, a missing
    template variable is truthy, `Cookie.checkCookie` is forgeable, and there are no request size or
    time limits. Fixing escaping changes rendered output for existing users. That needs a major
    version, or an opt-in. See "Backlog".
12. **Line endings are mixed** (some files CRLF, some LF). Preserve each file's existing style when you
    edit it. Scripted edits that write LF into a CRLF file produce noisy diffs.

## Build, test, run

```sh
dotnet build fss.slnx
dotnet test fss.slnx                          # DuckDB + SQLite + pool/template/json/server tests
dotnet run --project samples/DuckDbQuickStart
dotnet pack fss.slnx -c Release -o artifacts  # fss, fss_duckdb, fss_postgres, fss_sqlite (+ .snupkg)
```

**Server-backed tests** are skipped (NUnit `Ignore`) unless configured. `tests/fss.Tests/shared.fs`
reads, in order: the environment variable, then a legacy file (`connection_*.txt`) in the test
directory or up to four parents.

| Database | Env var | Legacy file |
|---|---|---|
| Postgres | `FSS_TEST_POSTGRES` | `connection_postgres.txt` |
| MySQL | `FSS_TEST_MYSQL` | `connection_mysql.txt` |
| SQLite (optional) | `FSS_TEST_SQLITE` | `connection_sqlite.txt` (default: a temp file) |

A throwaway local Postgres, without Docker or touching any installed server, using the PostgreSQL
binaries:

```sh
initdb -D /tmp/fsspg -U fss -A trust -E UTF8 --no-locale
pg_ctl -D /tmp/fsspg -o "-p 55432 -c listen_addresses=localhost" -l /tmp/fsspg.log start
psql -h localhost -p 55432 -U fss -d postgres -c "create database testfss"
FSS_TEST_POSTGRES="Host=localhost;Port=55432;Username=fss;Database=testfss" dotnet test fss.slnx
pg_ctl -D /tmp/fsspg stop     # then delete /tmp/fsspg
```

Or with Docker: `docker run -d -p 5432:5432 -e POSTGRES_USER=fss -e POSTGRES_PASSWORD=fss -e POSTGRES_DB=fsstest postgres:17`.

**Expected state:** 0 warnings and 0 failed tests. With Postgres configured, only the MySQL tests are
skipped (about 20).

## Conventions

- **F# style:** match the surrounding code. The core uses explicit type annotations on public members
  and `///` XML docs on anything public. The legacy files have their own older style; don't reformat
  them wholesale.
- **Commit and release-note prefixes** (from project history): `FEA` feature, `BUG` fix, `SEC`
  security, `MNT` maintenance, `DOC`, `TST`, `CHK` checkpoint, `MIN` minor.
- **Every bug fix gets a regression test**, named for the behaviour (see `test_db_duckdb.fs`).
- **Branches:** the default branch is **`master`** (a stale `main` branch also exists). Topic branches
  look like `yy/mm/PREFIX-description` (e.g. `26/10/MNT-update_to_cpm_better_docs`).
- **Package versions** live only in `Directory.Packages.props`. Never put `Version=` on a
  `PackageReference`.
- **FSharp.Core is pinned low (8.0.x) on purpose**, so library consumers aren't forced to upgrade. It's
  referenced explicitly from `Directory.Build.props` (`DisableImplicitFSharpCoreReference`), and
  Dependabot ignores it.
- **`NuGetAudit`** runs on restore (all transitive packages, every severity). A new advisory shows up as a
  restore warning. Fix it by bumping the package, or by pinning the transitive package in
  `Directory.Packages.props` (transitive pinning is enabled).

## Releasing

Summary: bump `<VersionPrefix>`, date the `RELEASE_NOTES.md` heading, merge, then
`git tag vX.Y.Z && git push origin vX.Y.Z`. GitHub Actions publishes with NuGet Trusted Publishing
(OIDC, no stored API key). Full runbook, including the one-time nuget.org and GitHub setup:
[docs/releasing.md](docs/releasing.md).

## Decision log

| Decision | Why |
|---|---|
| .NET 10 only (no netstandard) | DuckDB.NET and Npgsql 10 target modern .NET. One TFM keeps CI and the package simple. |
| Paket and FAKE removed (2026-10) | Central Package Management covers what Paket did. The FAKE script was a stale template (wrong description, Paket pack). Plain `dotnet` CLI + GitHub Actions is enough. |
| One version for all packages | Easier to reason about. The packages share source files. |
| fss_mysql retired, not deleted | Existing users keep 2.0.0. The code still builds, so a future port is cheap. |
| `Customization` grew `openConnection` and `parameterPrefix` (2.1) | Needed to fix DuckDB pooling and parameters. Only dialect implementers are affected. |
| Web package kept, not split out | Existing users. Security notes in docs instead of behaviour changes. |
| NUnit 5 with `Assert.That` only | The classic `Assert.AreEqual` API was removed. The constraint model works across NUnit versions. |

## Backlog / known issues (prioritised)

1. **Web:** template auto-escaping (opt-in flag, then default in 3.0); missing template variable should
   be false in `{% if %}`; request and header size limits and socket timeouts; anchored route matching
   in `SecUD`; an HMAC-signed cookie helper to replace `checkCookie`.
2. **DB:** `InsertMany` is one statement per row. Add a multi-row `VALUES` batch path, or the DuckDB
   appender, for large loads.
3. **DB:** composite primary keys return nothing from inserts. Could return a tuple.
4. **DB:** consider a `fss_data_core` package so the shared types exist once (see Architecture).
5. **DB:** `Customization` could gain `quoteIdentifier`. Generated SQL doesn't quote names today, so
   mixed-case or reserved-word table and column names may fail on insert.
6. **JSON (`Fss.Json`):** doesn't handle escapes, mis-parses fractions with leading zeros (`1.05` → 1.5),
   and has no objects, true/false/null, or whitespace. Recommend System.Text.Json, and consider removing
   `Fss.Json` in 3.0.
7. **MySQL:** port to the shared core on MySqlConnector, or delete in 3.0.
8. Consider `EnablePackageValidation` with a 2.1.0 baseline once 2.1.0 ships, to catch accidental API
   breaks.
