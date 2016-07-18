module test_db
open NUnit.Framework

open Fss.Data.Postgres
open System.IO
open System

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
let t1b = { id = 2 ; age = 245 ; first = "wilma" ; last = "flintstone" ; rate = 1.0}
let t1c = { id = 100 ; age = 32 ; first = "Barney" ; last = "rubble" ; rate = 0.6}
let t1d = { id = 1000 ; age = 3 ; first = "pebbles" ; last = "flintstone" ; rate = 1.9}

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
    constraint pk_t4 primary key(id)
)"""

/// rarer types
let createT5SQL = """
create table Test5 (
    a	        int2 not null,
    b           decimal not null
)"""


// requires enum1
let createT6SQL = """
create table Test6 (
    id              int,
    first	        varchar(100)  not null,
    temperment      mood not null,
    constraint pk_t6 primary key(id)
)"""

let createT7SQL = """
create table Test7 (
    id serial,
    age   integer, -- Like Test 3 but with nullable age
    first	varchar(100) NOT NULL,
    last   varchar(100),
    rate   float NOT NULL
)"""

type Test7 = { id : int ; age : Nullable<int> ; first : string ; last : string option ; rate : float}

let t7a = { id = 1 ; age = Nullable(30) ; first = "fred" ; last = Some "flintstone" ; rate = 1.2}
let t7b = { id = -1 ; age = Nullable() ; first = "dino" ; last = None ; rate = 1.2}



open NpgsqlTypes;

type Mood =
   | [<PgName("happy")>]happy =0
   | [<PgName("good")>]good =1
   | [<PgName("sad")>]sad = 2

let createMoodEnum = "CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy')"
let dropMoodEnum = "drop type if exists mood "

type Test4 = { id : int ; age : int option ; first : string option ; last : string option; rate : float option ; happy : bool option}
type Test5 = { a : int16 ; b : decimal}
type Test6 = { id : int ; first : string ; temperment : Mood}

let t4a = { id = -1 ; age = Some(40) ; first = Some "wilma" ; last = Some "flintstone" ; rate = Some 1.256 ; happy = Some true}
let t4b = { id = -1 ; age = None ; first = None ; last = None ; rate = None ; happy = None}
let t5a = { a = 15s ; b = 1.2M}

let t6a = { id =1 ; first = "wilma" ; temperment = Mood.happy}
let t6b = { id =2 ; first = "fred" ; temperment = Mood.sad}

let getConnString() =
    if not (File.Exists("connection_postgres.txt")) then
        failwithf "ERROR: expected connection_postgres.txt file with connstring"
    else 
        System.IO.File.ReadAllText("connection_postgres.txt")

// reusable primitives for testing
let gc() = new DynamicSqlConnection(getConnString(),4)
let drop table (conn:DynamicSqlConnection)  = table |> sprintf "drop table if exists %s" 
                                                |> conn.ExecuteScalar |> ignore

let createT1 (conn:DynamicSqlConnection) = conn.ExecuteScalar(createT1SQL) |> ignore
let createT2 (conn:DynamicSqlConnection) = conn.ExecuteScalar(createT2SQL) |> ignore
let createT3 (conn:DynamicSqlConnection) = conn.ExecuteScalar(createT3SQL) |> ignore
let createT4 (conn:DynamicSqlConnection) = conn.ExecuteScalar(createT4SQL) |> ignore
let createT5 (conn:DynamicSqlConnection) = conn.ExecuteScalar(createT5SQL) |> ignore
let createT6 (conn:DynamicSqlConnection) = conn.ExecuteScalar(createT6SQL) |> ignore
let createT7 (conn:DynamicSqlConnection) = conn.ExecuteScalar(createT7SQL) |> ignore

let createEnum1 (conn:DynamicSqlConnection) = conn.ExecuteScalar(createMoodEnum) |> ignore
let dropEnum1 (conn:DynamicSqlConnection) = conn.ExecuteScalar(dropMoodEnum) |> ignore

let setupT1 (conn:DynamicSqlConnection) = drop "test1" conn ; createT1 conn
let setupT2 (conn:DynamicSqlConnection) = drop "test2" conn ; createT2 conn
let setupT3 (conn:DynamicSqlConnection) = drop "test3" conn ; createT3 conn
let setupT4 (conn:DynamicSqlConnection) = drop "test4" conn ; createT4 conn
let setupT5 (conn:DynamicSqlConnection) = drop "test5" conn ; createT5 conn
let setupT6 (conn:DynamicSqlConnection) = drop "test6" conn ; createT6 conn
let setupT7 (conn:DynamicSqlConnection) = drop "test7" conn ; createT7 conn
let setupE1 (conn:DynamicSqlConnection) = dropEnum1 conn ; createEnum1 conn


[<TestFixture>]
type TestPGDbBasic() = class     
    

    [<Test>]
    member x.Test001ConnectionDotTxtPresent() =
        Assert.IsTrue(File.Exists("connection_postgres.txt"))        

    [<Test>]
    member x.Test002GetConnString() =
        getConnString() |> ignore

    [<Test>]
    member x.Test003Connection() =
        use conn = gc()
        ()

    [<Test>]
    member x.Test004CondDropTable() =
        use conn = gc()
        drop "test1" conn

    [<Test>]
    member x.Test005DropCreate() =
        use conn = gc()
        drop "test1" conn
        createT1 conn
        drop "test1" conn

    [<Test>]
    member x.Test006SingleSimpleInsert() =
        use conn = gc()
        setupT1 conn
        conn.Reload()
        conn.InsertOne(t1a) 
        ()

    [<Test>]
    member x.Test007SingleSimpleInsertWithCheck() =
        use conn = gc()
        setupT1 conn
        conn.Reload()
        conn.InsertOne(t1a)
        Assert.IsTrue(conn.ExecuteScalar "select count(*) from test1" :?> int64 = 1L) |> ignore
        ()

    [<Test>]
    member x.Test008SingleSimpleInsertWithCheckOddTypes() =
        use conn = gc()
        setupT5 conn
        
        conn.InsertOne(t5a)
        Assert.IsTrue(conn.ExecuteScalar "select count(*) from test5" :?> int64 = 1L) |> ignore
        ()


    /// Test serial and default fields are provided by db
    [<Test>]
    member x.Test010SingleSerialInsertWithCheck() =
        use conn = gc()
        setupT2 conn
        conn.InsertOne(t1a,"test2",ignoredColumns=["id" ; "rate"])
        Assert.GreaterOrEqual(conn.ExecuteScalar "select rate from test2 where first = 'fred'" :?> float,998.9)

    /// Insert a record that contains nullable fields which are defined
    [<Test>]
    member x.Test030InsertNullableSimple() =
        use conn = gc()
        setupT3 conn
        conn.InsertOne(t3a,ignoredColumns=["id"]) |> ignore

    /// Insert Test3 example with nulled field last
    [<Test>]
    member x.Test031InsertNullableNull() =
        use conn = gc()
        setupT3 conn
        conn.InsertOne(t3b) |> ignore

    [<Test>]
    member x.Test032InsertManySomeNull() =
        use conn = gc()
        setupT3 conn
        conn.InsertMany([ t3a; t3b]) |>  ignore

    [<Test>]
    member x.Test033QuerySomeNull() =
        use conn = gc()
        setupT3 conn
        conn.InsertMany([ t3a; t3b]) |> ignore
        let results : Test3 [] = conn.Query "select * from test3"  |> Array.ofSeq

        let (someLast,noneLast) = results |> Array.partition (fun r -> r.last.IsSome)
        Assert.IsTrue(someLast.Length=1) |> ignore
        Assert.IsTrue(noneLast.Length=1) |> ignore

    [<Test>]
    member x.Test034InsertQueryAllNull() =
        use conn = gc()
        setupT4 conn
        conn.InsertMany ([ t4a ; t4b ],ignoredColumns=["id"]) |> ignore
        let results : Test4 [] = conn.Query "select * from test4 order by id asc"  |> Array.ofSeq
        let wilma = results.[0]
        let original = {t4a with id = wilma.id}
        let matches = wilma = original
        Assert.IsTrue(matches)

        let original2 = {t4b with id = results.[1].id}
        let matches2 = original2 = results.[1]
        Assert.IsTrue(matches2)

    [<Test>]
    member x.Test050InsertOneLogged() =
        use conn = gc()
        conn.LogQueries<-true
        conn.Logfile <- "dblog.txt"
        setupT4 conn
        conn.InsertOne<Test4,int>(t4a,ignoredColumns=["id"]) |> ignore

    [<Test>]
    member x.Test051QueryNullFieldWithOperatorArg1() =
        use conn = gc()
        setupT3 conn
        conn.InsertMany([ t3a; t3b]) |>  ignore
        use comm = conn.Command "select age from test3 where last = :name"
        comm?name <- "flintstone"
        let age:int = comm.ExecuteScalar() :?> int32
        Assert.AreEqual(age,30)

    [<Test>]
    member x.Test051QueryNullFieldWithOperator2() =
        use conn = gc()
        setupT3 conn
        conn.InsertMany([ t3a; t3b]) |>  ignore
        use comm = conn.Command "select last from test3 where first = 'fred'"
        use r = comm.ExecuteReader()
        r.Read() |> ignore
        let last : string option = r?last
        Assert.AreEqual(last,Some "flintstone")

    [<Test>]
    member x.Test051QueryNullFieldWithOperator3() =
        use conn = gc()
        setupT3 conn
        conn.InsertMany([ t3a; t3b]) |>  ignore
        use comm = conn.Command "select last from test3 where first = 'dino'"
        use r = comm.ExecuteReader()
        r.Read() |> ignore
        let last : string option = r?last
        Assert.AreEqual(last,None)
       

    [<Test>]
    member x.Test052QueryNullFieldWithNullable1() =
        use conn = gc()
        setupT7 conn
        do
            use comm = conn.Command "insert into test7(age,first,last,rate) values (:age,'dino',null,1.0)"
            comm?age<-Nullable(4)
            comm.ExecuteNonQuery() |> ignore

        do
            use comm = conn.Command "select age from test7 where first = 'dino'"
            let age = comm.ExecuteScalar() :?> int32
            Assert.AreEqual(age,4)

    [<Test>]
    member x.Test052QueryNullFieldWithNullable2() =
        use conn = gc()
        setupT7 conn
        do
            use comm = conn.Command "insert into test7(age,first,last,rate) values (:age,'dino',null,1.0)"
            comm?age<-Nullable()
            comm.ExecuteNonQuery() |> ignore

        do
            use comm = conn.Command "select first from test7 where age is null"
            let first = comm.ExecuteScalar() :?> string
            Assert.AreEqual(first,"dino")

end

(*
[<TestFixture>]
type TestEnums() = class
    let conn = gc()

    /// setup enum and table
    let setupET() =
        drop "test6" conn
        dropEnum1 conn
        setupE1 conn
        createT6 conn
        conn.Reload()
    /// drop enum and table
    let dropET() =
        drop "test6" conn
        dropEnum1 conn

    do
        try
            dropEnum1 conn
            ()
        with _ ->
            ()

        drop "test6" conn
        dropEnum1 conn

    [<Test>]
    member x.Test001Enum1() =
        // can we create an enum
        drop "test6" conn // just in case this is still here and dependent
        setupE1 conn
        dropET()

    [<Test>]
    member x.Test002TableWithEnum() =
        setupET()
        dropET()

    [<Test>]
    member x.Test003Insert()=
        setupET()

        let ids : int [] = conn.InsertMany [|  t6a ; t6b |] 

        dropET()

    [<Test>]
    member x.Test003InsertAndQuery() =
        setupE1 conn
        createT6 conn

        let ids : int [] = conn.InsertMany [|  t6a ; t6b |] 

        let mood :string = sprintf "select temperment::text from test6 where id = %d" (ids.[0]) |> conn.ExecuteScalar :?> string

        Assert.AreEqual (mood, "happy")
        drop "test6" conn
        dropEnum1 conn

    [<Test>]
    member x.Playground() =
        setupET()

        let ids : int [] = conn.InsertMany [|  t6a ; t6b |] 

        use conn2 = gc()
        use comm = conn2.Command "select * from test6"
        use reader = comm.ExecuteReader()
        while reader.Read() do
            let x : int32  = reader?id
            let y : string  = reader?temperment
            printfn "%d %s" x y
    [<Test>]
    member x.Test005EnumByOperator() =
        setupET()

        let ids : int [] = conn.InsertMany [|  t6a ; t6b |] 

        use comm = conn.Command "select * from test6 where first='wilma'"
        use reader = comm.ExecuteReader()
        reader.Read()|> ignore
        let id : int = reader?id
        let y : string  = reader?temperment
        Assert.AreEqual(y,"happy")

    [<Test>]
    member x.Test005EnumByOperator2() =
        setupET()
        conn.RegisterEnum<Mood>()

        let ids : int [] = conn.InsertMany [|  t6a ; t6b |] 

        use comm = conn.Command "select * from test6 where temperment=:mood"
        comm?mood<-Mood.happy
        use reader = comm.ExecuteReader()
        reader.Read()|> ignore
        let id : int = reader?id
        let y : string  = reader?first
        Assert.AreEqual(y,"wilma")

    [<Test>]
    member x.Test004Query() =
        setupET()

        let ids : int [] = conn.InsertMany [|  t6a ; t6b |] 


        let records : Test6[] = conn.Query "select * from test6 order by id asc" |> Array.ofSeq

        Assert.AreEqual(records.Length,2)
        Assert.AreEqual("wilma",records.[0].first)
        Assert.AreEqual("fred",records.[1].first)
        Assert.AreEqual("happy",records.[0].temperment)
        Assert.AreEqual("sad",records.[1].temperment)

        drop "test6" conn
        dropEnum1 conn
        
    interface IDisposable with
        member x.Dispose() =
            drop "test6" conn
            dropEnum1 conn
            (conn :> IDisposable).Dispose()

end

*)

[<TestFixture>]
type TestTransactions() = class
    let conn = gc()
    do
        use conn = gc()
        setupT1 conn
        
    let cleanTable() =
        conn.ExecuteScalar("delete from test1") |> ignore

    [<Test>]
    /// Insert a row then commit and check it's in there properly
    member x.Test001SingleInsertCommit() =
        // Clean up table
        cleanTable()
        use trans = conn.StartTrans()
        conn.InsertOne(t1a,transProvided=trans)

        trans.Commit()

        Assert.IsTrue(conn.ExecuteScalar "select count(*) from test1" :?> int64 = 1L) 
        cleanTable()
    
    [<Test>]
    /// Insert a row then roll transaction back to ensure it's gone
    member x.Test002SingleInsertRollback() =
        // Clean up table
        cleanTable()
        use trans = conn.StartTrans()
        conn.InsertOne(t1a,transProvided=trans)

        trans.Rollback()

        Assert.IsTrue(conn.ExecuteScalar "select count(*) from test1" :?> int64 = 0L) 
        cleanTable()

    [<Test>]
    /// Insert many rows then commit and check they're in there properly
    member x.Test003InsertManyCommit() =
        // Clean up table
        cleanTable()
        use trans = conn.StartTrans()
        conn.InsertMany([| t1a ; t1b; t1c; t1d |],transProvided=trans) |> ignore

        trans.Commit()

        Assert.IsTrue(conn.ExecuteScalar "select count(*) from test1" :?> int64 = 4L)
        cleanTable()
    
    [<Test>]
    /// Insert a row then roll transaction back to ensure it's gone
    member x.Test004InsertManyRollback() =
        // Clean up table
        cleanTable()
        use trans = conn.StartTrans()
        conn.InsertMany([| t1a ; t1b; t1c; t1d |],transProvided=trans) |> ignore

        trans.Rollback()

        Assert.IsTrue(conn.ExecuteScalar "select count(*) from test1" :?> int64 = 0L) 
        cleanTable()

    [<Test>]
    /// Insert many rows then commit and check they're in there properly
    member x.Test005InsertManyTwoStepsCommit() =
        // Clean up table
        cleanTable()
        use trans = conn.StartTrans()
        conn.InsertMany([| t1a ; t1b |],transProvided=trans) |> ignore
        conn.InsertMany([| t1c ; t1d |],transProvided=trans) |> ignore

        trans.Commit()

        Assert.IsTrue(conn.ExecuteScalar "select count(*) from test1" :?> int64 = 4L)
        cleanTable()
    
    [<Test>]
    /// Insert a row then roll transaction back to ensure it's gone
    member x.Test006InsertManyRollback() =
        // Clean up table
        cleanTable()
        use trans = conn.StartTrans()
        conn.InsertMany([| t1a ; t1b |],transProvided=trans) |> ignore
        conn.InsertMany([| t1c ; t1d |],transProvided=trans) |> ignore


        trans.Rollback()

        Assert.IsTrue(conn.ExecuteScalar "select count(*) from test1" :?> int64 = 0L) 
        cleanTable()
    [<Test>]
    /// Insert a row then commit and check it's in there properly
    member x.Test007SingleInsertViaTransCommit() =
        // Clean up table
        cleanTable()
        use trans = conn.StartTrans()
        trans.InsertOne(t1a)
        trans.Commit()

        Assert.IsTrue(conn.ExecuteScalar "select count(*) from test1" :?> int64 = 1L) 
        cleanTable()
    
    [<Test>]
    /// Insert a row then roll transaction back to ensure it's gone
    member x.Test008SingleInsertViaTransRollback() =
        // Clean up table
        cleanTable()
        use trans = conn.StartTrans()
        trans.InsertOne(t1a)

        trans.Rollback()

        Assert.IsTrue(conn.ExecuteScalar "select count(*) from test1" :?> int64 = 0L) 
        cleanTable()

    [<Test>]
    /// Insert a row then roll transaction back to ensure it's gone
    member x.Test009SingleInsertViaTransRollback() =
        // Clean up table
        cleanTable()
        use trans = conn.StartTrans()
        trans.InsertOne(t1a)

        trans.Rollback()

        Assert.IsTrue(conn.ExecuteScalar "select count(*) from test1" :?> int64 = 0L) 
        cleanTable()

    [<Test>]
    // Insert several rows then roll back; 
    // confirm transaction isolation
    // test trans.ExecuteScalar
    member x.Test010InsertManyViaTrans() =
        cleanTable()
        // insert one bit outside the transaction
        conn.InsertOne(t1a)
        use trans = conn.StartTrans()
        trans.InsertMany([| t1b ; t1c; t1d |]) |> ignore
        // confirm that the transaction is isolated
        Assert.IsTrue(conn.ExecuteScalar "SELECT COUNT(*) FROM test1" :?> int64 = 1L)
        Assert.IsTrue(trans.ExecuteScalar "SELECT COUNT(*) FROM test1" :?> int64 = 4L)
        trans.Rollback()
        Assert.IsTrue(conn.ExecuteScalar "SELECT COUNT(*) FROM test1" :?> int64 = 1L)
        cleanTable()

    [<Test>]
    // Insert several rows then roll back; 
    // confirm transaction isolation
    // test trans.ExecuteScalar
    // test that trans
    member x.Test011MultipleSimultaneousTransactions() =
        cleanTable()
        use trans = conn.StartTrans()
        use trans2 = conn.StartTrans()
        trans.InsertMany([| t1b ; t1c; t1d |]) |> ignore
        trans2.InsertOne(t1a) |> ignore
        // confirm that the transaction is isolated
        Assert.IsTrue(conn.ExecuteScalar "SELECT COUNT(*) FROM test1" :?> int64 = 0L)

        let results : Test1 [] = trans.Query "select * from test1"  |> Array.ofSeq
        Assert.IsTrue(results.Length=3)
        Assert.IsTrue(trans.ExecuteScalar "SELECT COUNT(*) FROM test1" :?> int64 = 3L)

        trans2.Commit()

        // the conn only sees 1 result, because only trans2 has been committed
        let results2: Test1 [] = conn.Query "select * from test1"  |> Array.ofSeq
        Assert.IsTrue(results2.Length=1)
        Assert.IsTrue(conn.ExecuteScalar "SELECT COUNT(*) FROM test1" :?> int64 = 1L)
        // trans see's 4, because READCOMMIT allows us to see the commit
        // from trans2 as well as our own.
        Assert.IsTrue(trans.ExecuteScalar "SELECT COUNT(*) FROM test1" :?> int64 = 4L)
        trans.Commit()
        // everyone should see 4 now.
        Assert.IsTrue(conn.ExecuteScalar "SELECT COUNT(*) FROM test1" :?> int64 = 4L)
        cleanTable()


    interface IDisposable with
        member x.Dispose() =
            drop "test1" conn
            (conn :> IDisposable).Dispose()
end
