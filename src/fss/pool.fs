namespace Fss

module Pool =
    /// ---- pool implementation ----------------------------------------
    /// manages a shared pool of resources, e.g. database handles
    open System
    open System.Threading

    type Pool<'T when 'T : equality and 'T :> IDisposable>(size:int,make:int->'T)  = class
        let pool = [| for i in {0..size-1} -> make i |]
        let free = [| for _ in {0..size-1} -> true |]
        let mutable freeCount = size
        let mutable last = 0
        let l1 = "general access"

        let ewh = new EventWaitHandle(false,EventResetMode.AutoReset)
        do
            ()
        member x.FreeCount with get() = freeCount
        member x.Take() = 
            let take1() =
                if freeCount = 0 then 
                    // Nothing available right now, release general
                    // lock and take a special lock to wait until a slot becomes
                    // available
                    None
                else
                    let rec find i =
                        if free.[i] then
                            freeCount<-freeCount - 1
                            free.[i]<- false
                            last <- (i+1) % size
                            Some(pool.[i])
                        else find ((i+1)%size)
                    lock l1 (fun() -> find last)
            let rec take2() =
                match (lock l1 (take1)) with
                    | None ->
                        ewh.WaitOne() |> ignore
                        take2()
                    | Some x -> x                
            take2()

        member x.Release(h:'T) =
        
            let rec find (i) steps =
                assert(steps<size) // must find it in one pass
                if (not (free.[i]) && pool.[i] = h) then
                    free.[i] <- true
                    freeCount <- freeCount + 1
                    //if freeCount = 1 then 
                    ewh.Set() |> ignore // in case anyone was waiting for a free slot
                    last <- i
                else
                    find ((i+1)%size) (steps+1)
            lock l1 (fun () -> find ((last+1)%size) 0)
        
        /// Apply function f to every pool member (without locking first)
        member x.Iter(f : 'T -> unit ) =
            for z in x.Pool do
                f(z)
        member private x.Pool = pool   
        interface IDisposable with
            member x.Dispose() = 
                        for z in x.Pool do
                            z.Dispose()
                        ewh.Dispose() 
    end