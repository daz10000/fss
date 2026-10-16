# API reference: database packages

`fss_duckdb`, `fss_postgres` and `fss_sqlite` share one implementation (`src/fss/db_common.fs`). Each
package exposes type abbreviations in its own module:

| Module | `DynamicSqlConnection` | `DynamicSqlTransaction` | Extras |
|---|---|---|---|
| `Fss.Data.DuckDB` | ✓ | ✓ | |
| `Fss.Data.Postgres` | ✓ | ✓ | `registerDataSource` |
| `Fss.Data.SQLite` | ✓ | ✓ | |

Each module also re-exports `ISqlConnection`. Optional arguments below are F# optional parameters:
pass them by name (`table = "x"`) or leave them out.

## `DynamicSqlConnection`

```fsharp
new DynamicSqlConnection(connStr: string)                  // pool size 10
new DynamicSqlConnection(connStr: string, poolSize: int)
```

Opens `poolSize` connections and loads table metadata. Implements `IDisposable` and `ISqlConnection`.

### Querying

| Member | Description |
|---|---|
| `Query<'T>(sql, ?parameters, ?transProvided) : seq<'T>` | Run `sql` and map each row to `'T`: a record or class (by column name), a tuple (by position), or a single value (first column). The sequence is lazy; see [laziness](getting-started.md#laziness). |
| `ExecuteScalar(sql, ?parameters) : obj` | First column of the first row (`null`/`DBNull` if none). |
| `ExecuteNonQuery(sql, ?parameters) : int` | Run a statement and return the number of rows affected. |

`parameters : seq<string * obj>`. The name may include or omit the SQL marker (`"id"`, `"@id"`, `"$id"`).
`option` values are unwrapped and `None` becomes NULL.

### Inserting

| Member | Description |
|---|---|
| `InsertOne<'T,'R>(item, ?table, ?transProvided, ?ignoredColumns) : 'R` | Insert one record. Returns its generated primary key as `'R`. |
| `InsertMany<'T,'R>(items, ?table, ?transProvided, ?ignoredColumns) : 'R[]` | Insert records in one transaction (its own, or `transProvided`). Returns the keys in order. |

- `table : string` defaults to `typeof<'T>.Name.ToLower()`. It may be `"schema.table"`.
- `ignoredColumns : seq<string>` names record fields *not* to insert, so the database supplies them.
- `'R` is converted from what the driver returns (e.g. `int64` → `int`). Use `obj` when the table has
  no single-column primary key.

### Transactions and commands

| Member | Description |
|---|---|
| `StartTrans() : DynamicSqlTransaction` | Begin a transaction on a dedicated pooled connection. |
| `Command(sql) : DynamicSqlCommand` | A raw command on a pooled connection. **Dispose it** to release the connection. |
| `cc(sql)` | Alias for `Command`. |
| `conn?procName` | Command for a stored procedure (`CommandType.StoredProcedure`). |
| `Take() / Release(conn)` | Borrow and return a raw `DbConnection` from the pool. Advanced; always pair them. |

### Maintenance and diagnostics

| Member | Description |
|---|---|
| `Reload()` | Reload driver type info (e.g. Postgres enums) on every pooled connection, and re-read table metadata. |
| `KeepAlive : bool` | Postgres only (on by default): whether the background ping/reconnect thread runs. |
| `Logfile : string` | Set to a path to start logging to that file. |
| `LogQueries : bool` | Log every statement with its duration. |
| `LogLongerThan : float` | Log statements slower than this many milliseconds (default: effectively never). |
| `LogConnUse : bool` | Log pool take and release. |
| `Log(msg)` | Write a line to the log file. |

## `DynamicSqlTransaction`

Created by `StartTrans()`. Implements `IDisposable` and `ISqlConnection`.

| Member | Description |
|---|---|
| `Query<'T>(sql, ?parameters)` | As on the connection, inside the transaction. |
| `InsertOne<'T,'R>(item, ?table, ?ignoredColumns)` / `InsertMany<'T,'R>(items, ?table, ?ignoredColumns)` | As on the connection, inside the transaction. |
| `ExecuteScalar(sql, ?parameters)` / `ExecuteNonQuery(sql, ?parameters)` | As on the connection. |
| `Commit()` / `Rollback()` | End the transaction. |
| `cc(sql)` | A raw command bound to this transaction. |
| `Dispose()` | Rolls back if not committed, and returns the connection to the pool. |

## `ISqlConnection`

The members common to a connection and a transaction. Use it to write functions that work either way.

```fsharp
type ISqlConnection =
    abstract Query           : sql:string * ?parameters:seq<string*obj> -> seq<'T>
    abstract InsertMany      : items:seq<'T> * ?table:string * ?ignoredColumns:seq<string> -> 'R[]
    abstract InsertOne       : item:'T * ?table:string * ?ignoredColumns:seq<string> -> 'R
    abstract ExecuteScalar   : sql:string * ?parameters:seq<string*obj> -> obj
    abstract ExecuteNonQuery : sql:string * ?parameters:seq<string*obj> -> int
```

## `DynamicSqlCommand` and `DynamicSqlDataReader`

Low-level wrappers for when `Query` isn't enough (multiple result sets, streaming very large results with
custom handling, and so on).

```fsharp
use cmd = db.Command "select * from t where id = @id"
cmd?id <- 7                         // or cmd.AddParameter("id", box 7)
use r = cmd.ExecuteReader()         // DynamicSqlDataReader
while r.Read() do
    let name : string = r?name      // converts like Query does; option for NULLs
    ()
// also: cmd.ExecuteScalar(), cmd.ExecuteNonQuery(), cmd.Parameters, r.Reader (the raw DbDataReader)
```

## Postgres: `registerDataSource`

```fsharp
Postgres.registerDataSource : connStr:string -> configure:(NpgsqlDataSourceBuilder -> unit) -> NpgsqlDataSource
```

Every `DynamicSqlConnection` created afterwards with exactly `connStr` opens its connections from the
registered data source. That's how Npgsql enum, composite and plugin mappings are applied. See the
[Postgres guide](postgres.md#enums-and-other-custom-types).

## Extending to another database: `Customization`

A dialect is a class implementing `Fss.Data.Common.Customization<'Parameter,'Conn>`, plus three type
abbreviations. `src/fss_sqlite/db_sqlite.fs` is the shortest example.

| Member | Purpose |
|---|---|
| `openConnection(connStr, first)` | Create and open a pooled connection. `first` is the pool's first connection (DuckDB duplicates it). |
| `loadColDetail conn` | Column metadata for every table: `Map<SchematizedTable, ColDetail[]>`. Table and schema names lower-case. |
| `getSearchPath conn` | Schemas to search, in order, for bare table names. |
| `sequenceMechanism()` | `RETURNS_CLAUSE` (`insert ... returning pk`) or `SQL_STMT "select ..."` run after each insert. |
| `parameterPrefix()` | Marker for generated parameters: `"@"` or `"$"`. |
| `needsKeepAlive()` | Run the ping/reconnect thread? |
| `reopenConnection conn` / `reloadTypes conn` | Used by keep-alive and `Reload()`. |
