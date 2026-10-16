# PostgreSQL guide (`fss_postgres`)

Read [Getting started](getting-started.md) first; this page covers Postgres specifics.

```sh
dotnet add package fss_postgres
```

```fsharp
open Fss.Data.Postgres
use db = new DynamicSqlConnection("Host=localhost;Port=5432;Username=app;Password=...;Database=app")
```

The connection string goes straight to [Npgsql](https://www.npgsql.org/doc/connection-string-parameters.html),
so SSL, timeouts and the rest work as Npgsql documents them. Keep passwords out of source code: read the
string from configuration or the environment.

## Parameters

Write `@name` in SQL:

```fsharp
db.Query<Order>("select * from orders where customer_id = @c and total > @min", [ "c", box 42 ; "min", box 10m ])
```

## Generated keys

`serial`, `bigserial` and `generated ... as identity` primary keys come back from `InsertOne`/`InsertMany`
through `insert ... returning`:

```fsharp
type NewCustomer = { email : string ; name : string }
let id : int = db.InsertOne({ email = "a@b.c" ; name = "Ann" }, table = "customer")
```

## Schemas and the search path

When the connection is created, fss reads `search_path`, expanding `$user` to the current user. Bare
table names in inserts are looked up through it, in order. A table in any other schema takes a
qualified name:

```fsharp
db.InsertMany(rows, table = "staging.events")
```

Queries are ordinary SQL, so qualify names there as usual.

## Enums and other custom types

Npgsql 7+ configures type mappings on an `NpgsqlDataSource`; global mapping is gone. Register a
configured data source for your connection string **before** creating the `DynamicSqlConnection`:

```fsharp
open NpgsqlTypes

type Mood =
    | [<PgName("happy")>] Happy = 0
    | [<PgName("sad")>]   Sad = 1

Postgres.registerDataSource connStr (fun b -> b.MapEnum<Mood>("mood") |> ignore) |> ignore
use db = new DynamicSqlConnection(connStr)        // pooled connections now come from that data source

type Person = { id : int ; name : string ; mood : Mood }
db.Query<Person> "select * from person"           // mood maps to the F# enum
```

The same hook covers anything else configured on `NpgsqlDataSourceBuilder`, such as
`UseNodaTime()`, `UseNetTopologySuite()` and JSON options.

If you create a type (e.g. `CREATE TYPE ... AS ENUM`) after the connection opened, call `db.Reload()`
so Npgsql reloads its type information.

Without a mapping, read enums as text (`select mood::text as mood`) into a `string` field or an F# enum
field. fss converts text to F# enums by case-insensitive name.

## Keep-alive

`fss_postgres` runs a background thread that runs `select 1` every 5 seconds and reopens pooled
connections if the server has dropped them (after a restart or network blip, say). Set
`db.KeepAlive <- false` to stop it. It's a background thread, so it never prevents process exit. Still,
dispose the connection at shutdown.

## Type notes

| Postgres | F# |
|---|---|
| `integer` / `bigint` / `smallint` | `int` / `int64` / `int16` (converted between, overflow throws) |
| `numeric` | `decimal` |
| `real` / `double precision` | `float32` / `float` |
| `text` / `varchar` | `string` |
| `boolean` | `bool` |
| `timestamp` | `DateTime` |
| `timestamptz` | `DateTime` (UTC, `Kind=Utc`) or `DateTimeOffset` |
| `date` | `DateOnly` or `DateTime` |
| `uuid` | `Guid` |
| `jsonb` / `json` | `string` |
| arrays | `T[]` (e.g. `int[]`, `string[]`) |

Npgsql is strict about `DateTime.Kind` when *writing* `timestamptz` parameters: pass `DateTimeKind.Utc`
values (or `DateTimeOffset`).
