namespace Fss

module Pool =
    /// ---- pool implementation ----------------------------------------
    /// manages a shared pool of resources, e.g. database handles
    open System
    open System.Threading

    /// Fixed-size pool of reusable resources (e.g. database connections).
    /// All `size` items are created eagerly by calling `make 0 .. make (size-1)` in order.
    /// `Take` blocks until an item is free; every `Take` must be paired with exactly one `Release`.
    type Pool<'T when 'T : equality and 'T :> IDisposable>(size:int,make:int->'T)  = class
        do if size < 1 then invalidArg (nameof size) "Pool size must be at least 1"

        let pool =
            let created = ResizeArray<'T>()
            try
                for i in 0 .. size-1 do created.Add(make i)
                created.ToArray()
            with _ ->
                // Don't leak the items we did manage to create
                for c in created do (try c.Dispose() with _ -> ())
                reraise()

        let free = Array.create size true
        let mutable freeCount = size
        let sync = obj()
        /// Counts free slots; Take waits on it so a released slot always wakes exactly one waiter.
        let available = new SemaphoreSlim(size, size)

        member x.FreeCount with get() = freeCount

        member x.Take() =
            available.Wait()
            lock sync (fun () ->
                // The semaphore guarantees at least one slot is free
                let i = Array.IndexOf(free, true)
                free.[i] <- false
                freeCount <- freeCount - 1
                pool.[i])

        member x.Release(h:'T) =
            lock sync (fun () ->
                let mutable i = 0
                while i < size && not (not free.[i] && obj.ReferenceEquals(pool.[i], h)) do
                    i <- i + 1
                if i = size then
                    invalidOp "Pool.Release: item does not belong to this pool or has already been released"
                free.[i] <- true
                freeCount <- freeCount + 1)
            available.Release() |> ignore

        /// Apply function f to every pool member (without locking first)
        member x.Iter(f : 'T -> unit ) =
            for z in pool do
                f(z)

        interface IDisposable with
            member x.Dispose() =
                for z in pool do
                    z.Dispose()
                available.Dispose()
    end
