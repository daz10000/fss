// Moving data between engines with one record type: read from SQLite (an operational
// store), bulk-load into DuckDB (for analytics), then analyse.
//
//   dotnet run --project samples/SqliteToDuckDb
//
// The same pattern works for Postgres -> DuckDB with fss_postgres.

open System
open System.IO

// Both packages expose a `DynamicSqlConnection`; alias the modules to keep them apart.
module Lite = Fss.Data.SQLite
module Duck = Fss.Data.DuckDB

/// One record type serves both databases
type Reading = { sensor : string ; taken_at : DateTime ; celsius : float ; note : string option }

type SensorStats = { sensor : string ; n : int64 ; min_c : float ; max_c : float ; avg_c : float }

[<EntryPoint>]
let main _ =
    let sqlitePath = Path.Combine(Path.GetTempPath(), sprintf "fss-readings-%s.db" (Guid.NewGuid().ToString "N"))
    try
        // ---------------------------------------------------------------------------
        // Source: a SQLite database with some sensor readings
        // ---------------------------------------------------------------------------
        do
            use src = new Lite.DynamicSqlConnection(sprintf "Data Source=%s" sqlitePath)
            src.ExecuteNonQuery """
                create table reading (sensor text not null, taken_at text not null,
                                      celsius real not null, note text)""" |> ignore
            let rng = Random 42
            let start = DateTime(2026, 1, 1)
            let readings =
                [ for i in 0 .. 4999 ->
                    { sensor = sprintf "s%02d" (i % 8)
                      taken_at = start.AddMinutes(float i * 5.0)
                      celsius = 15.0 + 10.0 * rng.NextDouble()
                      note = if i % 500 = 0 then Some "calibration" else None } ]
            // One transaction around the whole batch keeps SQLite inserts fast
            use t = src.StartTrans()
            t.InsertMany<Reading,int64>(readings) |> ignore
            t.Commit()
            printfn "SQLite: wrote %d readings to %s" readings.Length sqlitePath

        // ---------------------------------------------------------------------------
        // Extract from SQLite into records, load into DuckDB
        // ---------------------------------------------------------------------------
        use src = new Lite.DynamicSqlConnection(sprintf "Data Source=%s" sqlitePath)
        // SQLite stores the DateTime as text; fss converts it back to DateTime for the record
        let rows = src.Query<Reading> "select * from reading" |> Array.ofSeq

        use duck = new Duck.DynamicSqlConnection("Data Source=:memory:")
        duck.ExecuteNonQuery """
            create table reading (sensor varchar not null, taken_at timestamp not null,
                                  celsius double not null, note varchar)""" |> ignore
        duck.InsertMany<Reading,obj>(rows) |> ignore
        printfn "DuckDB: loaded %d rows" rows.Length

        // ---------------------------------------------------------------------------
        // Analyse in DuckDB
        // ---------------------------------------------------------------------------
        let stats =
            duck.Query<SensorStats> """
                select sensor, count(*) as n, min(celsius) as min_c, max(celsius) as max_c, avg(celsius) as avg_c
                from reading group by sensor order by sensor"""
            |> Array.ofSeq
        for s in stats do
            printfn "  %s  n=%d  min=%.1f  max=%.1f  avg=%.2f" s.sensor s.n s.min_c s.max_c s.avg_c

        let calibrations = duck.Query<Reading> "select * from reading where note is not null order by taken_at" |> Array.ofSeq
        printfn "%d calibration readings, first at %O" calibrations.Length calibrations.[0].taken_at

        if stats.Length <> 8 || calibrations.Length <> 10 then failwith "unexpected results"
        0
    finally
        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools()
        if File.Exists sqlitePath then File.Delete sqlitePath
