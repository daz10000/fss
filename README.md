# fss

[![CI](https://github.com/daz10000/fss/actions/workflows/ci.yml/badge.svg)](https://github.com/daz10000/fss/actions/workflows/ci.yml)
[![NuGet fss_duckdb](https://img.shields.io/nuget/v/fss_duckdb.svg?label=fss_duckdb)](https://www.nuget.org/packages/fss_duckdb)

**Typed SQL for F# with no mapping code.** Write SQL, get F# records back; hand it F# records and it
inserts them. fss works with **DuckDB**, **PostgreSQL** and **SQLite**.

```fsharp
open Fss.Data.DuckDB

type Sale = { region : string ; product : string ; revenue : decimal }

use db = new DynamicSqlConnection("Data Source=:memory:")

// Query a Parquet file (or CSV, JSON, or a table) straight into records
let top =
    db.Query<Sale>("""
        select region, product, sum(qty * price) as revenue
        from 'sales/*.parquet'
        where region = $region
        group by all order by revenue desc limit 10""", [ "region", box "west" ])
    |> List.ofSeq
```

## Packages

| Package | Database | Driver | Status |
|---|---|---|---|
| [`fss_duckdb`](https://www.nuget.org/packages/fss_duckdb) | DuckDB (in-process analytics) | DuckDB.NET | **Primary.** |
| [`fss_postgres`](https://www.nuget.org/packages/fss_postgres) | PostgreSQL | Npgsql | Maintained |
| [`fss_sqlite`](https://www.nuget.org/packages/fss_sqlite) | SQLite | Microsoft.Data.Sqlite | Maintained |
| [`fss_mysql`](https://www.nuget.org/packages/fss_mysql) | MySQL | MySql.Data | Retired (no new releases) |
| [`fss`](https://www.nuget.org/packages/fss) | — | — | Legacy HTTP server and Jinja-style templates |

The database packages are independent of each other and of `fss`. Reference only the one you need:

```sh
dotnet add package fss_duckdb
```

All packages target **.NET 10**.

## What it does

- **Query to records:** `db.Query<'T>(sql)` maps columns to record fields by name (case-insensitive).
  `option` fields take NULLs. It also maps to tuples (columns by position) and to single values
  (`Query<int64> "select count(*) ..."`).
- **Insert records:** `db.InsertOne(record)` and `db.InsertMany(records)` build the `INSERT` from the
  table's metadata and return the generated primary keys.
- **Parameters:** pass `[ "name", box value ]` to `Query`, `ExecuteScalar` and `ExecuteNonQuery`.
  Never splice user input into SQL.
- **Transactions:** `use t = db.StartTrans()` exposes the same API. Call `t.Commit()` to commit;
  disposing without committing rolls back.
- **Type conversion:** handles the usual driver mismatches, such as SQLite's `int64` → `int`/`bool`,
  DuckDB's `HUGEINT` → `int64`, `DATE` → `DateOnly`, text → F# enum, and `uuid` → `Guid`.
- **Pooling:** each `DynamicSqlConnection` owns a small pool of connections and is safe to share
  between threads.

## Documentation

- [Getting started](docs/getting-started.md): the core concepts, for every database
- [DuckDB guide](docs/duckdb.md): files, in-memory databases, types and bulk loading
- [PostgreSQL guide](docs/postgres.md): schemas, enums, search path and keep-alive
- [SQLite guide](docs/sqlite.md)
- [API reference](docs/api-reference.md)
- Legacy: [web server](docs/web-server.md) · [templates](docs/templates.md) · [MySQL](docs/mysql.md)
- [Release notes](RELEASE_NOTES.md)

## Samples

Runnable projects live in [`samples/`](samples/README.md). The DuckDB samples are a good place to start:

```sh
dotnet run --project samples/DuckDbQuickStart
dotnet run --project samples/DuckDbAnalytics
```

## Building from source

You need the .NET 10 SDK.

```sh
dotnet build fss.slnx
dotnet test  fss.slnx                      # DuckDB + SQLite tests always run
FSS_TEST_POSTGRES="Host=localhost;Username=postgres;Password=...;Database=fsstest" dotnet test fss.slnx
```

Postgres and MySQL tests are skipped unless `FSS_TEST_POSTGRES` / `FSS_TEST_MYSQL` are set. Contributors and
maintainers should read [MAINTAINERS.md](MAINTAINERS.md).

## License

Apache 2.0. See [LICENSE](LICENSE).
