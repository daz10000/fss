open System
//open Fss.Template
open System.Net
//open Fss.Pool
open System.Threading
open System.IO

//let foo v =
//    match v with
//        | Option<'T> v ->
//            ()
//        | _ ->
//            ()


//open Fss.Data.SQLite
open Fss.Data.Postgres

(*
open NpgsqlTypes
type Mood =
   | [<PgName("happy")>]happy =0
   | [<PgName("ok")>]ok =1
   | [<PgName("sad")>]sad = 2

//5Npgsql.NpgsqlConnection.MapEnumGlobally<Mood>()

type Test4 = { id : int ; age : int option ; first : string option ; last : string option; rate : float option ; happy : bool option}
type Test1 = { id : int ; age : int ; first : string ; last : string ; rate : float}
type Test3 = { id : int ; age : int ; first : string ; last : string option ; rate : float}
type Test6 = { id : int ; first : string ; temperment : Mood}

let t6a = { id= 1 ; first = "mary" ; temperment = Mood.ok}

type capability_request_status =
   | [<PgName("requested")>]requested =0
   | [<PgName("approved")>]approved =1
   | [<PgName("rejected")>]rejected =2
   | [<PgName("deleted")>]deleted =3

Npgsql.NpgsqlConnection.MapEnumGlobally<capability_request_status>()

type CapabilityRequest = {
    task_id : int ;
    capability_id : int ;
    amount : decimal ; 
    status : capability_request_status ;
}
*)

[<EntryPoint>]
let main argv =
    let gc() = new DynamicSqlConnection("Server=127.0.0.1;Port=5432;User Id=fsstest;Password=flusalmon2ird;Database=fsstest;Pooling=false")
    //let gc() = new DynamicSqlConnection("Data Source=playground.db;Version = 3;")
    
    //let t4a = { id = -1 ; age = Some(40) ; first = Some "wilma" ; last = Some "flintstone" ; rate = Some 1.256 ; happy = Some true}
    //let t4b = { id = -1 ; age = None ; first = None ; last = None ; rate = None ; happy = None}

    let getConnString() =
        if not (File.Exists("connection.txt")) then
            failwithf "ERROR: expected connection.txt file with connstring"
        else 
            System.IO.File.ReadAllText("connection.txt")


    // reusable primitives for testing
   (*
    let drop table (conn:DynamicSqlConnection)  = table |> sprintf "drop table if exists %s" 
                                                    |> conn.ExecuteScalar |> ignore
    let createT1SQL = """
    create table Test1 (
        id integer NOT NULL,
        age   integer NOT NULL,
        first	varchar(100) NOT NULL,
        last   varchar(100) NOT NULL,
        rate   float NOT NULL
    )"""
    *)

    let conn = gc()
    conn.Logfile<- @"c:\tmp\t.txt"
    conn.LogQueries<- true // @"c:\tmp\t.txt"
    conn.LogLongerThan <- 0.0
    conn.LogConnUse<-true
    while true do
        try
            try
                let count = conn.ExecuteScalar ("select count(*) from test2") :?> int64
                printfn "%s Count is %d" (System.DateTime.Now.ToLongTimeString()) count
            with :?Npgsql.NpgsqlException as x ->
                printfn "%s exception: %s" (System.DateTime.Now.ToLongTimeString()) x.Message
        with :? System.InvalidOperationException as x ->
            printfn "%s conn not open : %s" (System.DateTime.Now.ToLongTimeString()) x.Message
            
        System.Threading.Thread.Sleep(5000)

    (*
    let t1a = { id = 1 ; age = 30 ; first = "fred" ; last = "flintstone" ; rate = 1.2} : Test1

    conn.InsertOne(t1a)


    let createT2SQL = """
    create table Test2 (
        id serial,
        age   integer  NOT NULL,
        first	varchar(100) NOT NULL,
        last   varchar(100) NOT NULL,
        rate   float NOT NULL default 999.0
    )"""


    let createT3SQL = """
    create table Test3 (
        id serial,
        age   integer NOT NULL,
        first	varchar(100) NOT NULL,
        last   varchar(100),
        rate   float NOT NULL
    )"""

//    // requires enum1
//    let createT6SQL = """
//    create table Test6 (
//        id              int,
//        first	        varchar(100)  not null,
//        temperment      mood not null,
//        constraint pk_t6 primary key(id)
//    )"""


    let t3a = { id = 1 ; age = 30 ; first = "fred" ; last = Some "flintstone" ; rate = 1.2} : Test3
    let t3b = { id = -1 ; age = 4 ; first = "dino" ; last = None ; rate = 1.2} : Test3

    let createT4SQL = """
    create table Test4 (
        id serial,
        age   integer,
        first	varchar(100),
        last   varchar(100),
        rate   float,
        happy  boolean,
        CONSTRAINT pk_proj PRIMARY KEY (id)
    )"""

    let createT4 (conn:DynamicSqlConnection) = conn.ExecuteScalar(createT4SQL) |> ignore



    let setupT4 (conn:DynamicSqlConnection) = drop "test4" conn ; createT4 conn

    
    printfn "make connection"
    use conn = gc()

    printfn "drop table6"
    conn.ExecuteScalar """drop table  if exists test6""" |> ignore

    printfn "create table 1"
    conn.ExecuteScalar """ create table Test6 (
        id              int,
        first	        varchar(100)  not null,
        temperment      mood not null,
        constraint pk_t6 primary key(id)
    )""" |> ignore

    printfn "insert into table 6"
    conn.Reload()
    conn.InsertOne(t6a) |> ignore

    //let comm = conn.Command """insert into test6 (id,first,temperment) values (4,'alex','sad')"""
    //comm.ExecuteNonQuery() |> ignore
    printfn "Read from test6"
    let results : Test6 [] = conn.Query "select id,first,temperment from test6" |> Array.ofSeq
    //let results : Test6 [] = conn.Query "select * from test6" |> Array.ofSeq

    printfn "Results: %A" results

    while true do
        printfn "press enter"
        System.Console.ReadLine() |> ignore
        let count = conn.ExecuteScalar "select count(*) from test6" :?> int64
        printfn "found %d rows" count 
//    do
//        use conn = gc()
//        conn.Reload()
//        use comm = conn.Command "select * from test6"
//        use reader = comm.ExecuteReader()
//        while reader.Read() do
//            let x : int32  = reader?id
//            let y : string  = reader?temperment
//            printfn "%d %s" x y

    (*
    let records : Test6[] = conn.Query "select * from test6 order by id asc" |> Array.ofSeq

    let p = Npgsql.NpgsqlParameter("mood","sad")
    p.NpgsqlDbType<-NpgsqlTypes.NpgsqlDbType.Unknown
    
    comm.Parameters.Add(p) |> ignore
    comm.ExecuteNonQuery()  |> ignore

    *)
    (*
    let p = new Npgsql.NpgsqlParameter()
    p.NpgsqlDbType<- NpgsqlTypes.NpgsqlDbType.Enum

*)
//do
//    use conn = gc()
//    conn.LogQueries<-true
//    conn.Logfile <- "dblog.txt"
//    setupT4 conn
//    let t = conn.InsertOne<Test4,int>(t4a,ignoredColumns=["id"])
//    ()
//    //conn.InsertOne(t4a,ignoredColumns=["id"]) |> ignore


//let t = Template("{{x[2][4]}}")
//let page = t.Render([| ("x",
//                        box [|
//                            [|1;2;3;4;5;6|];
//                            [|2;4;6;8;10;12|];
//                            [|3;6;9;12;15;18|];
//                            [|4;8;12;16;20;24|]
//                        |])  |])
//printf "page=%s" page
//
//printf "done"
//stdin.ReadLine() |> ignore
    *)

    0