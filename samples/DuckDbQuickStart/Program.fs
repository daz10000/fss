// fss_duckdb quick start: the whole API in one file.
//
//   dotnet run --project samples/DuckDbQuickStart
//
// Uses an in-memory database, so there is nothing to install or clean up.

open System
open Fss.Data.DuckDB

// ---------------------------------------------------------------------------------------
// 1. Describe rows as plain F# records. Field names match column names (case-insensitive).
//    Nullable columns must be `option` fields; NOT NULL columns must not be.
// ---------------------------------------------------------------------------------------
type Employee =
    { id : int
      name : string
      dept : string
      salary : decimal
      manager_id : int option
      hired : DateOnly }

/// The same shape without `id`: the database generates it, so we don't send it on insert.
type NewEmployee =
    { name : string
      dept : string
      salary : decimal
      manager_id : int option
      hired : DateOnly }

[<EntryPoint>]
let main _ =
    // -----------------------------------------------------------------------------------
    // 2. Open a connection. "Data Source=:memory:" is an in-memory database; use a file
    //    path (e.g. "Data Source=company.duckdb") to persist. The connection owns a small
    //    pool (default 10) of DuckDB connections that all see the same database.
    // -----------------------------------------------------------------------------------
    use db = new DynamicSqlConnection("Data Source=:memory:")

    db.ExecuteNonQuery """
        create sequence employee_id;
        create table employee (
            id          integer primary key default nextval('employee_id'),
            name        varchar not null,
            dept        varchar not null,
            salary      decimal(12,2) not null,
            manager_id  integer,
            hired       date not null
        )""" |> ignore

    // -----------------------------------------------------------------------------------
    // 3. Insert records. InsertOne/InsertMany return the generated primary key, converted
    //    to whatever type you ask for (here `int`). The table defaults to the record type
    //    name, lower-cased; pass `table =` when they differ.
    // -----------------------------------------------------------------------------------
    let ceoId : int =
        db.InsertOne({ name = "Ada" ; dept = "Exec" ; salary = 250000m ; manager_id = None ; hired = DateOnly(2015,1,5) },
                     table = "employee")

    let staff =
        [ { name = "Grace"  ; dept = "Eng"   ; salary = 180000m ; manager_id = Some ceoId ; hired = DateOnly(2017,3,1) }
          { name = "Linus"  ; dept = "Eng"   ; salary = 165000m ; manager_id = Some ceoId ; hired = DateOnly(2019,6,17) }
          { name = "Barbara"; dept = "Sales" ; salary = 120000m ; manager_id = Some ceoId ; hired = DateOnly(2021,9,30) } ]

    let ids : int[] = db.InsertMany(staff, table = "employee")
    printfn "Inserted ids %A" ids

    // -----------------------------------------------------------------------------------
    // 4. Query into records. The result is a lazy seq: the query runs when you enumerate it.
    // -----------------------------------------------------------------------------------
    printfn "\nAll employees:"
    for e in db.Query<Employee> "select * from employee order by id" do
        let mgr = e.manager_id |> Option.map string |> Option.defaultValue "-"
        printfn "  %-2d %-8s %-6s %10M  mgr=%s  hired=%O" e.id e.name e.dept e.salary mgr e.hired

    // -----------------------------------------------------------------------------------
    // 5. Parameters: never build SQL by string concatenation with user input.
    //    DuckDB parameters are written $name in SQL; pass (name, value) pairs.
    // -----------------------------------------------------------------------------------
    let engineers =
        db.Query<Employee>("select * from employee where dept = $dept and salary >= $min order by name",
                           [ "dept", box "Eng" ; "min", box 170000m ])
        |> List.ofSeq
    printfn "\nWell-paid engineers: %A" (engineers |> List.map (fun e -> e.name))

    // -----------------------------------------------------------------------------------
    // 6. Not everything needs a record type: scalars and tuples work too.
    // -----------------------------------------------------------------------------------
    let headcount = db.Query<int64> "select count(*) from employee" |> Seq.exactlyOne
    let byDept = db.Query<string * decimal> "select dept, sum(salary) from employee group by dept order by dept" |> List.ofSeq
    printfn "\nHeadcount %d; payroll by dept %A" headcount byDept

    // Or use ExecuteScalar for a single value (returns obj)
    let maxSalary = db.ExecuteScalar "select max(salary) from employee" :?> decimal
    printfn "Max salary %M" maxSalary

    // -----------------------------------------------------------------------------------
    // 7. Transactions: everything through `t` is atomic. Dispose without Commit = rollback.
    // -----------------------------------------------------------------------------------
    do
        use t = db.StartTrans()
        t.ExecuteNonQuery("update employee set salary = salary * 1.10 where dept = $d", [ "d", box "Eng" ]) |> ignore
        t.InsertOne<NewEmployee,int>({ name = "Ken" ; dept = "Eng" ; salary = 150000m ; manager_id = Some ceoId ; hired = DateOnly(2024,1,8) },
                                     table = "employee") |> ignore
        t.Commit()

    do
        use t = db.StartTrans()
        t.ExecuteNonQuery "delete from employee" |> ignore
        // no Commit: rolled back when `t` is disposed
        ()

    let finalCount = db.Query<int64> "select count(*) from employee" |> Seq.exactlyOne
    printfn "\nAfter commit + rollback there are %d employees" finalCount

    // -----------------------------------------------------------------------------------
    // 8. Write code once for "inside or outside a transaction" using ISqlConnection.
    // -----------------------------------------------------------------------------------
    let payroll (conn:ISqlConnection) = conn.ExecuteScalar "select sum(salary) from employee" :?> decimal
    printfn "Payroll: %M" (payroll db)

    if finalCount <> 5L then failwith "unexpected row count"
    0
