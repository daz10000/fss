# DuckDB guide (`fss_duckdb`)

[DuckDB](https://duckdb.org) is an in-process analytical database. There is no server to install. It
can query CSV, Parquet and JSON files in place, and it's very fast at aggregations. Together with fss,
it gives you typed F# records out of analytical SQL with no ceremony.

Read [Getting started](getting-started.md) first; this page covers DuckDB specifics.

```sh
dotnet add package fss_duckdb
```

`fss_duckdb` depends on `DuckDB.NET.Data.Full`, which bundles the native DuckDB library for Windows,
Linux and macOS (x64 and arm64).

## Connection strings

| Connection string | Meaning |
|---|---|
| `Data Source=:memory:` | A private in-memory database, discarded on Dispose |
| `Data Source=analytics.duckdb` | A database file, created if missing |
| `Data Source=analytics.duckdb;ACCESS_MODE=READ_ONLY` | Read-only. Several processes can open a file this way. |

**In-memory databases are shared across the pool.** fss creates the first pooled connection from the
connection string and *duplicates* it for the rest, so all of them see the same database. (Opening
`:memory:` twice would otherwise give two unrelated empty databases.) Two separate
`DynamicSqlConnection("Data Source=:memory:")` objects are still two separate databases.

**Files:** only one *process* can open a DuckDB file read-write at a time. Within one process, any
number of pooled connections can share it.

## Querying files directly

```fsharp
type Trip = { vendor : string ; distance : float ; fare : decimal ; pickup : DateTime }

let trips =
    db.Query<Trip> """
        select vendor_id as vendor, trip_distance as distance, fare_amount as fare, pickup_datetime as pickup
        from read_parquet('data/trips/*.parquet')
        where fare_amount > 0"""
    |> Array.ofSeq

// CSV with header detection and type inference
db.Query<{| a : int64 ; b : int64 ; c : string |}> "select * from 'animals.csv'"

// JSON
db.Query<Event> "select * from read_json_auto('events.ndjson')"
```

File paths are SQL string literals, so they can't be parameters. Build them in code, and **never from
untrusted input**. Forward slashes work on every OS. If you're turning a Windows path into SQL, use
`path.Replace('\\', '/')`.

A view gives a file a name for later queries:

```fsharp
db.ExecuteNonQuery "create view sales as select * from read_csv_auto('sales.csv')" |> ignore
db.Query<RegionTotal> "select region, sum(amount) as total from sales group by region"
```

Remote files (S3, HTTP) need the `httpfs` extension:

```fsharp
db.ExecuteNonQuery "install httpfs; load httpfs;" |> ignore
db.Query<Row> "select * from 's3://bucket/path/*.parquet'"
```

## Parameters

DuckDB parameters are written `$name`:

```fsharp
db.Query<Trip>("select * from trips where vendor = $v and fare > $min", [ "v", box "CMT" ; "min", box 10m ])
```

## Generated keys

DuckDB has no `serial` type. Use a sequence:

```sql
create sequence person_id;
create table person (
    id    integer primary key default nextval('person_id'),
    name  varchar not null
);
```

```fsharp
type NewPerson = { name : string }
let id : int = db.InsertOne({ name = "Ada" }, table = "person")   // uses insert ... returning id
```

`InsertOne` and `InsertMany` return the primary key when the table has a single-column primary key.
Without one, ask for `obj` and ignore it: `db.InsertMany<Row,obj>(rows)`.

## Type mapping

What DuckDB.NET returns, and which F# field types fss will accept:

| DuckDB type | Natural F# type | Also accepted (converted) |
|---|---|---|
| `BOOLEAN` | `bool` | |
| `TINYINT` / `SMALLINT` / `INTEGER` / `BIGINT` | `sbyte` / `int16` / `int` / `int64` | any wider or narrower integer, `float`, `decimal` (overflow throws) |
| `UTINYINT` … `UBIGINT` | `byte` … `uint64` | other integer types |
| `HUGEINT` | `System.Numerics.BigInteger` | `int64`, `decimal` (within ±7.9e28) |
| `DECIMAL(p,s)` | `decimal` | `float` |
| `REAL` / `DOUBLE` | `float32` / `float` | `decimal` |
| `VARCHAR` | `string` | `Guid`, F# enum (by name, case-insensitive) |
| `UUID` | `Guid` | |
| `DATE` | `DateOnly` | `DateTime` |
| `TIME` | `TimeOnly` | `TimeSpan` |
| `TIMESTAMP` | `DateTime` | |
| `TIMESTAMPTZ` | `DateTime` | `DateTimeOffset` |
| `INTERVAL` | `TimeSpan` | |
| `BLOB` | `Stream` | `byte[]` (copied out) |
| `LIST` / `ARRAY` | `System.Collections.Generic.List<T>` | (F# `list` and arrays are not converted) |
| `STRUCT` / `MAP` | `Dictionary<string,obj>` / `Dictionary<K,V>` | |
| `ENUM` | `string` | F# enum |

Any column can be NULL: use an `option` field (or `Nullable<_>`) when it might be.

For `LIST` and `STRUCT` columns, you'll usually find it easier to `unnest` or pick fields in SQL
(`select s.field as field ...`) than to map nested values.

## Bulk loading

`InsertMany` sends one parameterised `INSERT` per row inside a single transaction. For up to tens of
thousands of rows that's fine. For bigger loads, let DuckDB do the work:

```fsharp
// From files: fastest by far
db.ExecuteNonQuery "insert into sales select * from read_csv_auto('big.csv')" |> ignore
db.ExecuteNonQuery "copy sales from 'big.parquet'" |> ignore

// From other databases: DuckDB can attach Postgres / SQLite / MySQL directly
db.ExecuteNonQuery "install postgres; load postgres; attach 'dbname=app host=localhost' as pg (type postgres, read_only)" |> ignore
db.ExecuteNonQuery "create table orders as select * from pg.public.orders" |> ignore
```

To bulk-load in-memory F# data at the highest speed, the DuckDB.NET appender
(`DuckDBConnection.CreateAppender`) is the tool. It works directly on a `DuckDBConnection` and isn't
wrapped by fss.

## Writing results out

```fsharp
db.ExecuteNonQuery "copy (select * from summary) to 'summary.parquet' (format parquet)" |> ignore
db.ExecuteNonQuery "copy (select * from summary) to 'summary.csv' (header)" |> ignore
```

## Concurrency

- Many readers run in parallel across pooled connections.
- Writers use optimistic concurrency. Two transactions updating the same rows: one fails with a
  conflict error, so retry it.
- DuckDB is built for analytics. For a high-rate stream of small transactional writes, Postgres or
  SQLite is a better fit (or batch the writes).

## Schemas and attached databases

Bare table names in inserts resolve via the current schema (`main`) and the DuckDB `search_path`
setting. Other schemas take a qualified name: `table = "staging.events"`. Metadata covers the
connection's current database only. To insert into an `ATTACH`ed database, use plain SQL
(`insert into other.tbl select ...`).

## Troubleshooting

| Symptom | Cause / fix |
|---|---|
| `Parser Error: syntax error at or near ":"` | You used a Postgres-style `:name` parameter. DuckDB uses `$name`. |
| `Could not set lock on file` | Another process has the file open read-write. Close it, or open with `ACCESS_MODE=READ_ONLY`. |
| `Binder Error: ... must appear in the GROUP BY clause` | A SQL problem, not an fss one. `group by all` is handy. |
| Table created in one `DynamicSqlConnection("Data Source=:memory:")` isn't visible in another | Each one is its own in-memory database. Share one connection object, or use a file. |
