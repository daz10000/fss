module test_db_thumper
open NUnit.Framework

open Fss.Data.MySql
open System.IO
open System
open FSharp.Data

open Newtonsoft.Json
open Newtonsoft.Json.Converters

// field of interest
let foi = "id, insert_name, five_prime_link_code, three_prime_link_code, breed_code, direction, inventory_status_rollup"
// select
let select = "select "
// from
let from = " from rabit where id = 45022"

type Rabit = { 
    id : int ;
    insert_name:string;
    five_prime_link_code:string;
    three_prime_link_code:string;
    breed_code:string;
    direction:string;
    inventory_status_rollup:string;
    }

let getConnString() =
    if not (File.Exists("connection_mysql.txt")) then
        failwithf "ERROR: expected connection_mysql.txt file with connstring"
    else 
        System.IO.File.ReadAllText("connection_Thumper.txt")
let gc() = new DynamicSqlConnection(getConnString())

[<TestFixture>]
type TestThumperBasic() = class

    [<Test>]
    member x.Test001ConnectionDotTxtPresent() =
        Assert.IsTrue(File.Exists("connection_Thumper.txt"))        

    [<Test>]
    member x.Test002GetConnString() =
        getConnString() |> ignore

    [<Test>]
    member x.Test003Connection() =
        use conn = gc()
        ()

    [<Test>]
    member x.Test004SelectRabit() =
        use conn = gc()
        
        let query = select + foi + from
        let results : Rabit[] = conn.Query query |> Array.ofSeq
        let results_json = JsonConvert.SerializeObject(results.[0])
        Assert.IsTrue(results.Length=1)
        Assert.IsTrue(results.[0].insert_name="!dGAS4")
end