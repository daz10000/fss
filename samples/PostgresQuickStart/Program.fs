// fss_postgres quick start.
//
//   FSS_PG="Host=localhost;Port=5432;Username=me;Password=secret;Database=scratch" \
//       dotnet run --project samples/PostgresQuickStart
//
// Creates (and drops) its own schema `fss_sample`, so it is safe to point at a scratch database.
// Without FSS_PG set it prints instructions and exits successfully (so CI can always run it).

open System
open Fss.Data.Postgres

type Customer = { id : int ; email : string ; name : string ; signed_up : DateTime ; referrer_id : int option }
type NewCustomer = { email : string ; name : string ; signed_up : DateTime ; referrer_id : int option }

type Order = { id : int64 ; customer_id : int ; total : decimal ; placed_at : DateTime }

[<EntryPoint>]
let main _ =
    match Environment.GetEnvironmentVariable "FSS_PG" with
    | null | "" ->
        printfn "Set FSS_PG to a Npgsql connection string to run this sample, e.g."
        printfn "  FSS_PG=\"Host=localhost;Username=postgres;Password=...;Database=postgres\""
        0
    | connStr ->
    use db = new DynamicSqlConnection(connStr)

    // Everything lives in its own schema. Inserts accept schema-qualified table names;
    // bare names are resolved through the connection's search_path.
    db.ExecuteNonQuery """
        drop schema if exists fss_sample cascade;
        create schema fss_sample;
        create table fss_sample.customer (
            id          serial primary key,
            email       text not null unique,
            name        text not null,
            signed_up   timestamp not null,
            referrer_id integer references fss_sample.customer(id)
        );
        create table fss_sample.orders (
            id          bigserial primary key,
            customer_id integer not null references fss_sample.customer(id),
            total       numeric(12,2) not null,
            placed_at   timestamp not null default now()
        );""" |> ignore

    try
        // InsertOne returns the serial primary key. Tables created after the connection
        // opened are picked up automatically.
        use t = db.StartTrans()
        let alice : int = t.InsertOne({ email = "alice@example.com" ; name = "Alice" ; signed_up = DateTime(2026,1,2) ; referrer_id = None }, table = "fss_sample.customer")
        let bob : int = t.InsertOne({ email = "bob@example.com" ; name = "Bob" ; signed_up = DateTime(2026,2,3) ; referrer_id = Some alice }, table = "fss_sample.customer")
        for (c, total) in [ alice, 19.99m ; alice, 5.00m ; bob, 120.00m ] do
            t.ExecuteNonQuery("insert into fss_sample.orders(customer_id, total) values (@c, @t)", [ "c", box c ; "t", box total ]) |> ignore
        t.Commit()

        let customers = db.Query<Customer> "select * from fss_sample.customer order by id" |> List.ofSeq
        for c in customers do printfn "%A" c

        // Postgres parameters are written @name
        let big =
            db.Query<Order>("select * from fss_sample.orders where total > @min order by total desc", [ "min", box 10m ])
            |> List.ofSeq
        printfn "Orders over 10: %A" (big |> List.map (fun o -> o.total))

        let spend = db.Query<string * decimal> """
                        select c.name, sum(o.total) from fss_sample.customer c
                        join fss_sample.orders o on o.customer_id = c.id group by c.name order by c.name"""
                    |> List.ofSeq
        printfn "Spend by customer: %A" spend
        if customers.Length <> 2 || big.Length <> 2 then failwith "unexpected results"
        0
    finally
        db.ExecuteNonQuery "drop schema if exists fss_sample cascade" |> ignore
