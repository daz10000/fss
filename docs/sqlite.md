# SQLite guide (`fss_sqlite`)

Read [Getting started](getting-started.md) first; this page covers SQLite specifics.

```sh
dotnet add package fss_sqlite
```

```fsharp
open Fss.Data.SQLite
use db = new DynamicSqlConnection("Data Source=app.db")
```

`fss_sqlite` is built on `Microsoft.Data.Sqlite`, which bundles a native SQLite for every platform. Its
[connection string reference](https://learn.microsoft.com/dotnet/standard/data/sqlite/connection-strings)
lists the options (`Mode=ReadOnly`, `Cache=Shared`, `Password=` with SQLCipher, and so on).

## In-memory databases

Each pooled connection to `Data Source=:memory:` is its own private, empty database. With a pool, that
almost never does what you want. Use a named shared-cache in-memory database instead:

```fsharp
use db = new DynamicSqlConnection("Data Source=scratch;Mode=Memory;Cache=Shared")
```

The database lives as long as at least one connection to it is open, which is as long as `db`.

## Parameters

SQLite accepts `@name`, `:name` and `$name` in SQL.

## Types

SQLite stores values as INTEGER, REAL, TEXT or BLOB. fss converts them to your record's types:

| You declare | SQLite stores | fss gives you |
|---|---|---|
| `int`, `int16`, `int64` | INTEGER | the field type (overflow throws) |
| `bool` | INTEGER 0/1 | `bool` |
| `float`, `decimal` | REAL | the field type |
| `DateTime` | TEXT (ISO-8601) | `DateTime` (parsed) |
| `Guid` | TEXT | `Guid` |
| `string` | TEXT | `string` |

## Keys

`InsertOne`/`InsertMany` return `last_insert_rowid()`, an `int64`, converted to the type you ask for.
Use `id integer primary key` to have SQLite assign ids. Leave the `id` field out of your insert record,
or list it in `ignoredColumns`.

SQLite reports an `integer primary key` column as nullable. To read it into a non-option field, use
`id : int64` (which is fine because the value is never NULL in practice). To *insert* through a record
that includes `id`, make it `id : int64 option` and pass `None`.

## Concurrency

SQLite allows one writer at a time. For multi-threaded writers, enable WAL mode once per database:

```fsharp
db.ExecuteNonQuery "pragma journal_mode=wal" |> ignore
```

and wrap batches of writes in a transaction (`db.StartTrans()`). Bulk inserts also run much faster
inside one transaction.
