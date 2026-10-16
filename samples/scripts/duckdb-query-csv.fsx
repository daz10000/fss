// Query a CSV file into F# records from an F# script.
//
//   dotnet fsi samples/scripts/duckdb-query-csv.fsx
//
// Uses the published package. To try local changes instead, build the repo and replace the
// first line with:  #r "../../src/fss_duckdb/bin/Release/net10.0/fss_duckdb.dll"
//                   #r "nuget: DuckDB.NET.Data.Full"
#r "nuget: fss_duckdb, 2.1.0"

open System.IO
open Fss.Data.DuckDB

type Animal = { a : int64 ; b : int64 ; c : string }

let csv = Path.Combine(__SOURCE_DIRECTORY__, "animals.csv").Replace('\\', '/')

let db = new DynamicSqlConnection("Data Source=:memory:")
for r in db.Query<Animal>(sprintf "select * from '%s' order by a" csv) do
    printfn $"{r.a} {r.b} {r.c}"

// Single values work too
let total : int64 = db.Query<int64>(sprintf "select sum(b) from '%s'" csv) |> Seq.exactlyOne
printfn $"total b = {total}"
