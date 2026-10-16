module test_db_duckdb

open System
open System.IO
open NUnit.Framework
open Fss.Data.DuckDB

// DuckDB runs in-process, so these tests always run: no server or connection string needed.

type Person = { id : int ; name : string ; age : int ; email : string option }
type NewPerson = { name : string ; age : int ; email : string option }

type Mood = Happy = 0 | Sad = 1

type Wide =
    { i8 : int16 ; i64 : int64 ; big : int64 ; ubig : uint64 ; d : decimal ; f : float
      b : bool ; day : DateTime ; dayOnly : DateOnly ; s : string ; g : Guid ; mood : Mood }

let createPeople = """
create sequence person_seq;
create table person (
    id     integer primary key default nextval('person_seq'),
    name   varchar not null,
    age    integer not null,
    email  varchar
)"""

let people =
    [ { name = "fred" ; age = 30 ; email = Some "fred@bedrock.org" }
      { name = "wilma" ; age = 29 ; email = None }
      { name = "barney" ; age = 32 ; email = Some "barney@bedrock.org" } ]

/// Fresh in-memory database with the person table
let withPeople (f : DynamicSqlConnection -> unit) =
    use conn = new DynamicSqlConnection("Data Source=:memory:")
    conn.ExecuteNonQuery createPeople |> ignore
    f conn

[<TestFixture>]
type TestDuckDbBasic() =

    [<Test>]
    member _.InMemoryDatabaseIsSharedAcrossPooledConnections() =
        // Regression: each pooled connection used to open its own private :memory: database
        use conn = new DynamicSqlConnection("Data Source=:memory:", 4)
        conn.ExecuteNonQuery "create table t(x integer)" |> ignore
        // Hold several pooled connections at once so every one is exercised
        let held = [ for _ in 1..3 -> conn.Command "select count(*) from t" ]
        for c in held do Assert.That(c.ExecuteScalar(), Is.EqualTo(0L))
        for c in held do (c :> IDisposable).Dispose()

    [<Test>]
    member _.InsertOneReturnsGeneratedKey() =
        withPeople (fun conn ->
            let id1 : int = conn.InsertOne(people.[0], table = "person")
            let id2 : int = conn.InsertOne(people.[1], table = "person")
            Assert.That(id1, Is.EqualTo 1)
            Assert.That(id2, Is.EqualTo 2))

    [<Test>]
    member _.InsertManyAndQueryRecords() =
        withPeople (fun conn ->
            let ids : int[] = conn.InsertMany(people, table = "person")
            Assert.That((ids = [| 1 ; 2 ; 3 |]))
            let back = conn.Query<Person> "select * from person order by id" |> Array.ofSeq
            Assert.That(back.Length, Is.EqualTo 3)
            Assert.That(back.[0], Is.EqualTo { id = 1 ; name = "fred" ; age = 30 ; email = Some "fred@bedrock.org" })
            Assert.That(back.[1].email, Is.EqualTo None))

    [<Test>]
    member _.InsertIntoTableNamedAfterRecordType() =
        use conn = new DynamicSqlConnection("Data Source=:memory:")
        conn.ExecuteNonQuery "create table NewPerson(name varchar not null, age integer not null, email varchar)" |> ignore
        conn.InsertOne<NewPerson,obj>(people.[0]) |> ignore
        Assert.That(conn.ExecuteScalar "select count(*) from newperson", Is.EqualTo 1L)

    [<Test>]
    member _.ParameterisedQuery() =
        withPeople (fun conn ->
            conn.InsertMany<NewPerson,int>(people, table = "person") |> ignore
            let older =
                conn.Query<Person>("select * from person where age > $minAge order by age", [ "minAge", box 29 ])
                |> List.ofSeq
            Assert.That((older |> List.map (fun p -> p.name) = [ "fred" ; "barney" ]))
            // marker prefix on the name is optional
            let n = conn.ExecuteScalar("select count(*) from person where name = $n", [ "$n", box "wilma" ])
            Assert.That(n, Is.EqualTo 1L))

    [<Test>]
    member _.ParameterValuesCanBeOptions() =
        withPeople (fun conn ->
            let affected =
                conn.ExecuteNonQuery("insert into person(name,age,email) values ($name,$age,$email)",
                                     [ "name", box "dino" ; "age", box 3 ; "email", box (None : string option) ])
            Assert.That(affected, Is.EqualTo 1)
            let e = conn.Query<string option>("select email from person where name = 'dino'") |> Seq.exactlyOne
            Assert.That(e, Is.EqualTo None))

    [<Test>]
    member _.ScalarAndTupleQueries() =
        withPeople (fun conn ->
            conn.InsertMany<NewPerson,int>(people, table = "person") |> ignore
            let names = conn.Query<string> "select name from person order by name" |> List.ofSeq
            Assert.That((names = [ "barney" ; "fred" ; "wilma" ]))
            let total = conn.Query<int64> "select sum(age) from person" |> Seq.exactlyOne
            Assert.That(total, Is.EqualTo 91L)
            let pairs = conn.Query<string * int> "select name, age from person order by age" |> List.ofSeq
            Assert.That((pairs = [ ("wilma",29) ; ("fred",30) ; ("barney",32) ])))

    [<Test>]
    member _.ColumnTypesAreCoercedToFieldTypes() =
        use conn = new DynamicSqlConnection("Data Source=:memory:")
        let w =
            conn.Query<Wide> """
                select 7::tinyint as i8, 42::integer as i64, 12345678901234::hugeint as big,
                       18446744073709551615::ubigint as ubig, 12.34::decimal(10,2) as d, 1.5::real as f,
                       true as b, date '2024-02-29' as day, date '2024-03-01' as dayOnly, 'x' as s,
                       '00000000-0000-0000-0000-000000000001'::uuid as g, 'Sad' as mood"""
            |> Seq.exactlyOne
        Assert.That(w.i8, Is.EqualTo 7s)
        Assert.That(w.big, Is.EqualTo 12345678901234L)
        Assert.That(w.ubig, Is.EqualTo UInt64.MaxValue)
        Assert.That(w.d, Is.EqualTo 12.34m)
        Assert.That(w.day, Is.EqualTo(DateTime(2024,2,29)))
        Assert.That(w.dayOnly, Is.EqualTo(DateOnly(2024,3,1)))
        Assert.That(w.g, Is.EqualTo(Guid("00000000-0000-0000-0000-000000000001")))
        Assert.That(w.mood, Is.EqualTo Mood.Sad)

    [<Test>]
    member _.QueryIntoSingleValuesOfAnyType() =
        // Issue #1: querying a single column into a primitive/struct type gave
        // "SQL name 'customer_id' not found in target Record"
        use conn = new DynamicSqlConnection("Data Source=:memory:")
        let ids : int[] = conn.Query "select 42 as customer_id union all select 7" |> Array.ofSeq
        Assert.That((Array.sort ids = [| 7 ; 42 |]))
        Assert.That(conn.Query<decimal> "select 1.50::decimal(5,2) as x" |> Seq.exactlyOne, Is.EqualTo 1.50m)
        Assert.That(conn.Query<DateTime> "select timestamp '2024-01-02' as x" |> Seq.exactlyOne, Is.EqualTo(DateTime(2024,1,2)))
        Assert.That(conn.Query<DateOnly> "select date '2024-01-02' as x" |> Seq.exactlyOne, Is.EqualTo(DateOnly(2024,1,2)))
        Assert.That(conn.Query<Guid> "select '00000000-0000-0000-0000-000000000001'::uuid as x" |> Seq.exactlyOne,
                    Is.EqualTo(Guid("00000000-0000-0000-0000-000000000001")))
        Assert.That(conn.Query<decimal option> "select null::decimal as x" |> Seq.exactlyOne, Is.EqualTo None)
        Assert.That(conn.Query<Nullable<int>> "select 5 as x" |> Seq.exactlyOne, Is.EqualTo(Nullable 5))

    [<Test>]
    member _.TimeAndBlobColumns() =
        use conn = new DynamicSqlConnection("Data Source=:memory:")
        let r = conn.Query<{| t : TimeOnly ; ts : TimeSpan ; bytes : byte[] |}>
                    "select time '12:34:56' as t, time '01:02:03' as ts, 'abc'::blob as bytes" |> Seq.exactlyOne
        Assert.That(r.t, Is.EqualTo(TimeOnly(12,34,56)))
        Assert.That(r.ts, Is.EqualTo(TimeSpan(1,2,3)))
        Assert.That((r.bytes = "abc"B))

    [<Test>]
    member _.NullIntoNonOptionFieldGivesClearError() =
        use conn = new DynamicSqlConnection("Data Source=:memory:")
        let ex = Assert.Throws<Exception>(fun () ->
                    conn.Query<{| x : int |}> "select null::integer as x" |> Seq.iter ignore)
        Assert.That(ex.Message, Does.Contain "not an option")

    [<Test>]
    member _.MissingRecordFieldGivesClearError() =
        withPeople (fun conn ->
            let ex = Assert.Throws<Exception>(fun () ->
                        conn.Query<Person> "select id, name from person" |> Seq.iter ignore)
            Assert.That(ex.Message, Does.Contain "age"))

    [<Test>]
    member _.QueryCsvFileDirectly() =
        let csv = Path.Combine(Path.GetTempPath(), sprintf "fss_%s.csv" (Guid.NewGuid().ToString "N"))
        File.WriteAllText(csv, "a,b,c\n1,2,cat\n2,3,dog\n3,100,mouse\n")
        try
            use conn = new DynamicSqlConnection("Data Source=:memory:")
            let rows = conn.Query<{| a : int64 ; b : int64 ; c : string |}>(sprintf "select * from '%s' order by a" csv) |> Array.ofSeq
            Assert.That(rows.Length, Is.EqualTo 3)
            Assert.That(rows.[2].c, Is.EqualTo "mouse")
        finally File.Delete csv

    [<Test>]
    member _.FileDatabaseWithPrimaryKeyReopens() =
        // Regression: reopening a database file containing a primary key used to throw an InvalidCastException
        let path = Path.Combine(Path.GetTempPath(), sprintf "fss_%s.duckdb" (Guid.NewGuid().ToString "N"))
        try
            do
                use conn = new DynamicSqlConnection(sprintf "Data Source=%s" path)
                conn.ExecuteNonQuery createPeople |> ignore
                conn.InsertMany<NewPerson,int>(people, table = "person") |> ignore
            use conn2 = new DynamicSqlConnection(sprintf "Data Source=%s" path)
            Assert.That(conn2.ExecuteScalar "select count(*) from person", Is.EqualTo 3L)
            let id : int = conn2.InsertOne({ name = "pebbles" ; age = 1 ; email = None }, table = "person")
            Assert.That(id, Is.EqualTo 4)
        finally
            for f in [ path ; path + ".wal" ] do if File.Exists f then File.Delete f

[<TestFixture>]
type TestDuckDbTransactions() =

    [<Test>]
    member _.CommitKeepsRows() =
        withPeople (fun conn ->
            do
                use t = conn.StartTrans()
                t.InsertMany<NewPerson,int>(people, table = "person") |> ignore
                t.Commit()
            Assert.That(conn.ExecuteScalar "select count(*) from person", Is.EqualTo 3L))

    [<Test>]
    member _.DisposeWithoutCommitRollsBack() =
        withPeople (fun conn ->
            do
                use t = conn.StartTrans()
                t.InsertMany<NewPerson,int>(people, table = "person") |> ignore
                Assert.That(t.ExecuteScalar "select count(*) from person", Is.EqualTo 3L)
            Assert.That(conn.ExecuteScalar "select count(*) from person", Is.EqualTo 0L))

    [<Test>]
    member _.FailedInsertManyRollsBackAndReleasesConnection() =
        // Regression: an exception mid-InsertMany leaked its pooled connection (pool of 1 => next call hangs)
        use conn = new DynamicSqlConnection("Data Source=:memory:", 1)
        conn.ExecuteNonQuery "create table p(name varchar not null, age integer not null check (age >= 0), email varchar)" |> ignore
        let bad = people @ [ { name = "bad" ; age = -1 ; email = None } ]
        Assert.Catch(fun () -> conn.InsertMany<NewPerson,obj>(bad, table = "p") |> ignore) |> ignore
        Assert.That(conn.ExecuteScalar "select count(*) from p", Is.EqualTo 0L)

    [<Test>]
    member _.UnenumeratedQueryDoesNotHoldConnection() =
        // Regression: Query used to take a pooled connection before the sequence was enumerated
        use conn = new DynamicSqlConnection("Data Source=:memory:", 1)
        for _ in 1..5 do conn.Query<int> "select 1" |> ignore
        Assert.That(conn.ExecuteScalar "select 42", Is.EqualTo 42)

    [<Test>]
    member _.QuerySequenceCanBeEnumeratedTwice() =
        use conn = new DynamicSqlConnection("Data Source=:memory:", 1)
        let q = conn.Query<int> "select * from range(3) t(x)"
        Assert.That(Seq.length q, Is.EqualTo 3)
        Assert.That(Seq.length q, Is.EqualTo 3)

    [<Test>]
    member _.CodeAgainstISqlConnectionWorksWithAndWithoutTransaction() =
        let countPeople (db:ISqlConnection) = db.ExecuteScalar "select count(*) from person" :?> int64
        withPeople (fun conn ->
            conn.InsertOne<NewPerson,int>(people.[0], table = "person") |> ignore
            use t = conn.StartTrans()
            Assert.That(countPeople conn, Is.EqualTo 1L)
            Assert.That(countPeople t, Is.EqualTo 1L))
