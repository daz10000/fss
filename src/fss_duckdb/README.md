# fss_duckdb

Typed SQL for F# on [DuckDB](https://duckdb.org): query tables, CSV, Parquet and JSON straight into F# records, and insert records with no mapping code. In-process, with nothing to install. See the [DuckDB guide](https://github.com/daz10000/fss/blob/master/docs/duckdb.md).

```fsharp
open Fss.Data.DuckDB

type Person = { id : int ; name : string ; email : string option }
type NewPerson = { name : string ; email : string option }

use db = new DynamicSqlConnection("Data Source=:memory:")

// Insert records; get generated keys back
let id : int = db.InsertOne({ name = "Ada" ; email = None }, table = "person")

// Query straight into records (columns match fields by name; NULL -> option)
let people = db.Query<Person>("select * from person where name = $name", [ "name", box "Ada" ]) |> List.ofSeq

// Tuples and scalars too
let n = db.Query<int64> "select count(*) from person" |> Seq.exactlyOne

// Transactions
do
    use t = db.StartTrans()
    t.InsertMany<NewPerson,int>(morePeople, table = "person") |> ignore
    t.Commit()
```

Documentation: https://github.com/daz10000/fss#documentation · Samples: https://github.com/daz10000/fss/tree/master/samples
