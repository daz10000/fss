open System
open Fss.Template
open System.Net
open Fss.Pool
open System.Threading
open System.IO

open Fss.Data.Postgres
let gc() = new DynamicSqlConnection("Server=127.0.0.1;Port=5432;User Id=fsstest;Password=moon77racer;Database=fsstest")
type Test4 = { id : int ; age : int option ; first : string option ; last : string option; rate : float option ; happy : bool option}

let t4a = { id = -1 ; age = Some(40) ; first = Some "wilma" ; last = Some "flintstone" ; rate = Some 1.256 ; happy = Some true}
let t4b = { id = -1 ; age = None ; first = None ; last = None ; rate = None ; happy = None}

let getConnString() =
    if not (File.Exists("connection.txt")) then
        failwithf "ERROR: expected connection.txt file with connstring"
    else 
        System.IO.File.ReadAllText("connection.txt")

// reusable primitives for testing
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

type Test1 = { id : int ; age : int ; first : string ; last : string ; rate : float}

let t1a = { id = 1 ; age = 30 ; first = "fred" ; last = "flintstone" ; rate = 1.2}

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

type Test3 = { id : int ; age : int ; first : string ; last : string option ; rate : float}

let t3a = { id = 1 ; age = 30 ; first = "fred" ; last = Some "flintstone" ; rate = 1.2}
let t3b = { id = -1 ; age = 4 ; first = "dino" ; last = None ; rate = 1.2}

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
do
    use conn = gc()
    conn.LogQueries<-true
    conn.Logfile <- "dblog.txt"
    setupT4 conn
    let t = conn.InsertOne<Test4,int>(t4a,ignoredColumns=["id"])
    ()
    //conn.InsertOne(t4a,ignoredColumns=["id"]) |> ignore


let t = Template("{{x[2][4]}}")
let page = t.Render([| ("x",
                        box [|
                            [|1;2;3;4;5;6|];
                            [|2;4;6;8;10;12|];
                            [|3;6;9;12;15;18|];
                            [|4;8;12;16;20;24|]
                        |])  |])
printf "page=%s" page

printf "done"
stdin.ReadLine() |> ignore

