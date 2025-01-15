#r "../bin/fss_duckdb/net8.0/fss_duckdb.dll"
#r "nuget:DuckDB.NET.Data.Full"

open Fss.Data.Common
open Fss.Data.DuckDB


type Record = { a:int64 ; b:int64 ; c:string}
let conn = new DynamicSqlConnection("Data Source=:memory:")

let result:Record[] = conn.Query "select * from 'test.csv'" |> Array.ofSeq
for r in result do
    printfn $"{r.a} {r.b} {r.c}"
