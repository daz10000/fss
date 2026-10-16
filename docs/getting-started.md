# Getting started

This guide covers the concepts shared by `fss_duckdb`, `fss_postgres` and `fss_sqlite`. They have the
same API: only the module you open and the connection string differ.

| Package | `open` | Connection string example | Parameter syntax in SQL |
|---|---|---|---|
| fss_duckdb | `Fss.Data.DuckDB` | `Data Source=:memory:` or `Data Source=my.duckdb` | `$name` |
| fss_postgres | `Fss.Data.Postgres` | `Host=localhost;Username=me;Password=...;Database=app` | `@name` |
| fss_sqlite | `Fss.Data.SQLite` | `Data Source=app.db` | `@name` (also `:name`, `$name`) |

The examples below use DuckDB. They work the same on the other databases if you change the `open`
and the parameter markers.

## Connecting

```fsharp
open Fss.Data.DuckDB

use db = new DynamicSqlConnection("Data Source=:memory:")       // pool of 10 connections
use db2 = new DynamicSqlConnection("Data Source=app.duckdb", 4)  // explicit pool size
```

A `DynamicSqlConnection` is a **pool** of database connections. Create one per database, share it
across your application (it is thread-safe), and dispose it at shutdown. When it is created, it opens
the pooled connections and reads the database's table and column metadata.

## Mapping rows to records

```fsharp
type Person = { id : int ; name : string ; age : int ; email : string option }

let people : Person list = db.Query<Person> "select * from person order by name" |> List.ofSeq
```

The mapping rules are:

- **Column names match field names**, case-insensitively. Use SQL aliases to rename
  (`select full_name as name ...`).
- **Every column must have a field to go to.** An extra column is an error, so select only what you need.
- **Every non-option field must be returned** by the query. A missing `option` field becomes `None`.
- **NULL needs `option`** (or `Nullable<_>`). NULL in a non-option value-type field fails with an
  error naming the column. NULL in a `string` field becomes `null`.
- **Types are converted when they differ but are compatible.** Examples: `int64` → `int`, integer → `bool`,
  `HUGEINT` (`BigInteger`) → `int64`/`decimal`, `DateTime` ↔ `DateOnly`, `TimeSpan` ↔ `TimeOnly`, text → `Guid`,
  text or integer → F# enum. Narrowing that overflows throws.

`Query` also maps rows to things that aren't records:

```fsharp
let n      = db.Query<int64> "select count(*) from person" |> Seq.exactlyOne   // single value: first column
let names  = db.Query<string> "select name from person" |> List.ofSeq
let pairs  = db.Query<string * int> "select name, age from person" |> List.ofSeq // tuple: columns by position
let maybe  = db.Query<string option> "select email from person" |> List.ofSeq
let anon   = db.Query<{| name : string ; age : int |}> "select name, age from person" |> List.ofSeq
```

### Laziness

`Query` returns a lazy `seq`. The SQL runs, and a pooled connection is borrowed, only while you
enumerate it. The connection goes back to the pool when enumeration finishes or the enumerator is
disposed. So:

- Materialise results (`List.ofSeq`, `Array.ofSeq`) when you need them more than once or after the
  connection might be gone.
- Don't hold a half-enumerated sequence for long. It keeps a pooled connection busy.

## Parameters

Pass parameters as `(name, value)` pairs. **Never build SQL by concatenating untrusted input.**

```fsharp
db.Query<Person>("select * from person where age >= $min and name like $pat",
                 [ "min", box 18 ; "pat", box "A%" ])

db.ExecuteNonQuery("update person set email = $e where id = $id", [ "e", box (Some "a@b.c") ; "id", box 7 ])
```

- In SQL, write the marker your database uses (see the table above).
- The name may be given with or without the marker: `"min"` and `"$min"` are both accepted.
- `option` values are unwrapped: `Some x` becomes `x`, and `None` or `null` becomes SQL NULL.

## Inserting records

```fsharp
type NewPerson = { name : string ; age : int ; email : string option }

let id : int = db.InsertOne({ name = "Ada" ; age = 36 ; email = None }, table = "person")
let ids : int[] = db.InsertMany(newPeople, table = "person")
```

How `InsertOne` and `InsertMany` work:

- **The table** defaults to the record type's name, lower-cased (`NewPerson` → `newperson`). Pass
  `table =` to override it. The name may be schema-qualified (`table = "sales.orders"`); a bare name is
  resolved through the schema search path. A table created after the connection opened is found
  automatically, because the metadata refreshes once on a miss.
- **Each record field must match a column.** Columns you leave out get their database default. That is
  how you let the database generate ids: use a record type without `id`, or pass
  `ignoredColumns = ["id"]`.
- **Nullability must agree**: a nullable column needs an `option` field, and a NOT NULL column needs a
  non-option field. A mismatch raises an error up front instead of failing halfway through the insert.
- **Return value**: the generated primary key of each row, converted to the type you ask for
  (`int`, `int64`, `string`, ...). If the table has no single-column primary key, ask for `obj` or `unit`
  and ignore the result. DuckDB and Postgres use `INSERT ... RETURNING`; SQLite uses `last_insert_rowid()`.
- **Atomicity**: `InsertMany` runs inside a transaction. If any row fails, nothing is inserted and the
  connection goes back to the pool.

`InsertMany` sends one `INSERT` per row on a single prepared command. That's fine for thousands of rows.
For millions, use the database's bulk path (DuckDB `COPY`/`read_csv`, Postgres `COPY`). See the
[DuckDB guide](duckdb.md#bulk-loading).

## Other statements

```fsharp
db.ExecuteNonQuery "create table person (...)" |> ignore                // returns rows affected
let total = db.ExecuteScalar "select sum(age) from person" :?> decimal  // first column of first row, as obj
```

Several statements separated by `;` can run in one `ExecuteNonQuery` call.

For full control, `db.Command sql` gives you a command on a pooled connection. Dispose it to return
the connection:

```fsharp
use cmd = db.Command "select name, age from person where id = @id"
cmd?id <- 7                         // dynamic parameter setter
use r = cmd.ExecuteReader()
while r.Read() do
    let name : string = r?name      // dynamic column getter, with the same conversions as Query
    let age : int option = r?age
    printfn "%s %A" name age
```

## Transactions

```fsharp
do
    use t = db.StartTrans()
    let id : int = t.InsertOne(order, table = "orders")
    t.ExecuteNonQuery("update stock set qty = qty - $n where sku = $sku", [ "n", box 1 ; "sku", box "X1" ]) |> ignore
    t.Commit()
// leaving the `use` scope without Commit() rolls back
```

A transaction holds one pooled connection until it is disposed. It has the same `Query`, `InsertOne`,
`InsertMany`, `ExecuteScalar` and `ExecuteNonQuery` members as the connection.

### Writing code once for both: `ISqlConnection`

`DynamicSqlConnection` and `DynamicSqlTransaction` both implement `ISqlConnection`. That lets a function
run inside or outside a transaction:

```fsharp
let addPerson (db:ISqlConnection) (p:NewPerson) : int = db.InsertOne(p, table = "person")

addPerson db p                      // autocommit
use t = db.StartTrans()
addPerson t p ; addPerson t q       // both or neither
t.Commit()
```

## Logging

```fsharp
db.Logfile <- "sql.log"      // enable logging to a file
db.LogQueries <- true        // log every statement with its duration (ms)
db.LogLongerThan <- 250.0    // or only statements slower than 250 ms
db.LogConnUse <- true        // log pool take/release (debugging pool exhaustion)
```

## Errors you might see

| Message | Meaning |
|---|---|
| `name mapping, SQL name 'x' not found in target Record T` | The query returned column `x` but `T` has no field `x`. Remove it from the select or alias it. |
| `record T field(s) a,b not returned by the query` | Non-option fields of `T` are missing from the result. |
| `column 'x' is NULL but field type Int32 is not an option` | Make the field `int option`, or `coalesce` in SQL. |
| `cannot map column 'x' (String) onto field x : Int32` | The value couldn't be converted. Check the SQL type or cast in SQL. |
| `no such table 'x' (searched schemas: ...)` | Check the name or schema, or pass `table = "schema.name"`. |
| `field x is option but not nullable in db` / `is not option but is nullable in db` | The record and column nullability disagree on insert. |
| `record field 'x' does not match a table column` | The insert record has a field the table doesn't. Use `ignoredColumns` or a different record type. |

## Threading and pooling

- Every operation borrows a connection and returns it when done. When all are busy, callers wait.
- Pool size defaults to 10. Size it to your expected concurrency. A transaction or a half-read
  `Query` sequence holds a connection for its whole lifetime.
- Each `DynamicSqlConnection` is independent. Two of them on the same connection string means two pools.
