module testPool

open NUnit.Framework
open Fss.Pool
open System.Threading
open System

type Token(name:string) = class
    let mutable taken = false
    do
        ()
    member x.Take() = 
        assert(not taken)
        taken <- true

    member x.Release() =
        assert(taken)
        taken <- false

    member x.Name = name

    interface IDisposable with
        member x.Dispose() = ()

end
[<TestFixture>]
type TestPoolBasic() = class    

    [<Test>]
    member x.Test1() =
        let rng = System.Random()
        let test1Verbose = false
        let make (i:int) = new Token( [| "cat" ; "dog" ; "mouse" ; "bear" ; "giraffe" ; "amoeba" ; "snake" ; "elephant" ; "echindna" ; "kangaroo" |].[i])

        use p = new Pool<Token>(10,make)

        let t1 = 50.0 // hold time upper limit
        let t2 = 10.0 // inter hold time upper limit
        let nThreads = 50

        let doneSemaphore = new EventWaitHandle(false,EventResetMode.AutoReset)
        let active = ref nThreads
        let exitSema = "exitsema"
        let beater n =
            for i in {0..20} do
                //printfn "%-5d: taking" n
                let h = p.Take()
                h.Take()
                //printfn "%-5d: got %s" n h.Name

                Thread.Sleep(rng.NextDouble()*t1 |> int ) 

                //printfn "%-5d: releasing %s" n h.Name

                h.Release()
                p.Release(h)
                Thread.Sleep(rng.NextDouble()*t2 |> int ) 

            //printfn "%-5d: released %s" n h.Name
            lock exitSema (fun () -> active:=!active-1)
        
            doneSemaphore.Set() |> ignore

            if test1Verbose then printfn "%-5d: exitting! active=%d" n !active


        for i in {1..nThreads} do
            let t = new Thread( fun () -> beater i)
            t.Start()

        while !active > 0 do
            Thread.Sleep(1000)
            if test1Verbose then printfn "exit count %d left" !active

        //Thread.Sleep(5000)
        if test1Verbose then printf "done active=%d" !active

end 