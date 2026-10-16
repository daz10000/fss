# fss_postgres

Typed SQL for F# on PostgreSQL (via Npgsql): query straight into F# records, and insert records with no mapping code. See the [PostgreSQL guide](https://github.com/daz10000/fss/blob/master/docs/postgres.md).

```fsharp
open Fss.Data.Postgres

type Person = { id : int ; name : string ; email : string option }
type NewPerson = { name : string ; email : string option }

use db = new DynamicSqlConnection("Host=localhost;Username=app;Password=...;Database=app")

// Insert records; get generated keys back
let id : int = db.InsertOne({ name = "Ada" ; email = None }, table = "person")

// Query straight into records (columns match fields by name; NULL -> option)
let people = db.Query<Person>("select * from person where name = @name", [ "name", box "Ada" ]) |> List.ofSeq

// Tuples and scalars too
let n = db.Query<int64> "select count(*) from person" |> Seq.exactlyOne

// Transactions
do
    use t = db.StartTrans()
    t.InsertMany<NewPerson,int>(morePeople, table = "person") |> ignore
    t.Commit()
```

Documentation: https://github.com/daz10000/fss#documentation · Samples: https://github.com/daz10000/fss/tree/master/samples
