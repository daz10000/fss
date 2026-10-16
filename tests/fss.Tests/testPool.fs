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
            for i in 0..20 do
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


        for i in 1..nThreads do
            let t = new Thread( fun () -> beater i)
            t.Start()

        while !active > 0 do
            Thread.Sleep(1000)
            if test1Verbose then printfn "exit count %d left" !active

        //Thread.Sleep(5000)
        if test1Verbose then printf "done active=%d" !active

end 
[<TestFixture>]
type TestPoolSafety() =

    [<Test>]
    member x.DoubleReleaseThrowsInsteadOfHanging() =
        use p = new Pool<Token>(2, fun i -> new Token(string i))
        let t = p.Take()
        p.Release t
        Assert.Throws<InvalidOperationException>(fun () -> p.Release t) |> ignore
        // pool still usable afterwards
        let t2 = p.Take()
        p.Release t2
        Assert.That(p.FreeCount, Is.EqualTo 2)

    [<Test>]
    member x.ReleaseWakesBlockedTaker() =
        use p = new Pool<Token>(1, fun i -> new Token(string i))
        let held = p.Take()
        let got = new ManualResetEventSlim(false)
        let t = Thread(fun () -> let h = p.Take() in got.Set(); p.Release h)
        t.Start()
        Assert.That(got.Wait(200), Is.False, "taker should block while pool is empty")
        p.Release held
        Assert.That(got.Wait(5000), Is.True, "taker should wake when an item is released")
        t.Join()

    [<Test>]
    member x.FailedConstructionDisposesCreatedItems() =
        let disposed = ref 0
        let make i =
            if i = 2 then failwith "boom"
            { new IDisposable with member _.Dispose() = disposed.Value <- disposed.Value + 1 }
        Assert.Throws<Exception>(fun () -> new Pool<IDisposable>(4, make) |> ignore) |> ignore
        Assert.That(disposed.Value, Is.EqualTo 2)
