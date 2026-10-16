// fss_duckdb analytics: query CSV and Parquet files in place, aggregate into F# records,
// and persist results to a DuckDB database file.
//
//   dotnet run --project samples/DuckDbAnalytics [output-folder]
//
// The sample generates its own input data, so it runs anywhere.

open System
open System.IO
open Fss.Data.DuckDB

/// One row of the raw sales export
type Sale = { order_id : int64 ; region : string ; product : string ; qty : int ; unit_price : decimal ; sold_on : DateTime }

/// Aggregates map onto records just like table rows do: name the columns to match the fields
type RegionSummary = { region : string ; orders : int64 ; revenue : decimal ; avg_order : float }

type TopProduct = { region : string ; product : string ; revenue : decimal ; rank_in_region : int64 }

type MonthlyTrend = { month : DateOnly ; revenue : decimal ; running_total : decimal }

[<EntryPoint>]
let main argv =
    let outDir =
        match argv with
        | [| d |] -> d
        | _ -> Path.Combine(Path.GetTempPath(), "fss-duckdb-analytics")
    Directory.CreateDirectory outDir |> ignore
    // DuckDB file paths in SQL: forward slashes work on every OS
    let file name = Path.Combine(outDir, name).Replace('\\', '/')
    let salesCsv, productsParquet, dbFile = file "sales.csv", file "products.parquet", file "analytics.duckdb"

    // -----------------------------------------------------------------------------------
    // Generate some input files (in real life these come from elsewhere)
    // -----------------------------------------------------------------------------------
    do
        use gen = new DynamicSqlConnection("Data Source=:memory:")
        gen.ExecuteNonQuery(sprintf """
            copy (
                select i as order_id,
                       ['north','south','east','west'][1 + i %% 4] as region,
                       'P' || lpad(((i * 7) %% 12)::varchar, 2, '0') as product,
                       1 + (i * 13) %% 9 as qty,
                       date '2025-01-01' + to_days((i * 37) %% 365) as sold_on
                from range(1, 20001) t(i)
            ) to '%s' (header, delimiter ',')""" salesCsv) |> ignore
        gen.ExecuteNonQuery(sprintf """
            copy (
                select 'P' || lpad(i::varchar, 2, '0') as product,
                       round(5 + (i * 3.75), 2)::decimal(10,2) as unit_price
                from range(0, 12) t(i)
            ) to '%s' (format parquet)""" productsParquet) |> ignore
    printfn "Input files written to %s" outDir

    // -----------------------------------------------------------------------------------
    // Query the files directly. A view gives the joined data a name for later queries.
    // -----------------------------------------------------------------------------------
    use db = new DynamicSqlConnection("Data Source=:memory:")
    db.ExecuteNonQuery(sprintf """
        create view sales as
        select s.order_id, s.region, s.product, s.qty, p.unit_price, s.sold_on::timestamp as sold_on
        from read_csv_auto('%s') s join read_parquet('%s') p using (product)""" salesCsv productsParquet) |> ignore

    let sample = db.Query<Sale> "select * from sales order by order_id limit 3" |> List.ofSeq
    printfn "\nFirst rows:"
    for s in sample do printfn "  %A" s

    let regions =
        db.Query<RegionSummary> """
            select region, count(*) as orders, sum(qty * unit_price) as revenue, avg(qty * unit_price) as avg_order
            from sales group by region order by revenue desc"""
        |> Array.ofSeq
    printfn "\nRevenue by region:"
    for r in regions do printfn "  %-6s %6d orders  %12M  (avg %.2f)" r.region r.orders r.revenue r.avg_order

    // Window functions, filtered with a parameter
    let topN = 2
    let top =
        db.Query<TopProduct>("""
            select * from (
                select region, product, sum(qty * unit_price) as revenue,
                       row_number() over (partition by region order by sum(qty * unit_price) desc) as rank_in_region
                from sales group by region, product)
            where rank_in_region <= $n
            order by region, rank_in_region""", [ "n", box topN ])
        |> Array.ofSeq
    printfn "\nTop %d products per region:" topN
    for t in top do printfn "  %-6s #%d %s %12M" t.region t.rank_in_region t.product t.revenue

    let trend =
        db.Query<MonthlyTrend> """
            with monthly as (
                select date_trunc('month', sold_on)::date as month, sum(qty * unit_price) as revenue
                from sales group by 1)
            select month, revenue, sum(revenue) over (order by month) as running_total
            from monthly order by month"""
        |> Array.ofSeq
    printfn "\nMonthly trend (first 3): %A" (trend |> Array.truncate 3)

    // -----------------------------------------------------------------------------------
    // Persist: attach a database file and store the summary there via InsertMany.
    // -----------------------------------------------------------------------------------
    if File.Exists dbFile then File.Delete dbFile
    do
        use store = new DynamicSqlConnection(sprintf "Data Source=%s" dbFile)
        store.ExecuteNonQuery """
            create table regionsummary (region varchar primary key, orders bigint not null,
                                        revenue decimal(18,2) not null, avg_order double not null)""" |> ignore
        let keys : string[] = store.InsertMany(regions)  // table name defaults to the record type name
        printfn "\nSaved summary for %A to %s" keys dbFile

    // Re-open the file to show it persisted
    use reopened = new DynamicSqlConnection(sprintf "Data Source=%s" dbFile)
    let saved = reopened.Query<RegionSummary> "select * from regionsummary order by region" |> Array.ofSeq
    printfn "Re-read %d rows from disk" saved.Length

    if saved.Length <> 4 || regions |> Array.sumBy (fun r -> r.orders) <> 20000L then failwith "unexpected results"
    0
