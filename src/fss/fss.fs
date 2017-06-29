(*
   FSS - F# Web Server
   Copyright 2012 Darren Platt

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

*)
namespace Fss
module Server =
    open System.Net.Sockets
    open System.IO
    open System
    open System.Text.RegularExpressions
    open System.Threading
    open System.Net
    open System.Collections.Specialized
    open System.Collections.Generic

    /// Fss release version
    let version = version.version

    // TODO - http/1.1  can reuse connection for extra speed.
    ///
    /// Simple http protocol implementation
    ///
    let protocolVersion = "HTTP/1.1"

    let explanation code =
        match code with
            | 200 -> "OK"
            | 302 -> "Found"
            | 303 -> "See Other"
            | 304 -> "Not Modified"
            | 307 -> "Temporary Redirect"
            | 404 -> "Not found"
            | 401 -> "Authorization required"
            | _ -> "unknown"
        
    /// Base response to a web request
    type Response(c:int,ct:string) = class
        let mutable code = c
        let mutable text = ""
        let mutable file = ""
        let mutable bytes : byte array = [||]
        let mutable cookie : string option = None
        let mutable contentType = ct
        let mutable contentEncoding :string option = None
        let mutable headers = []
        do
            ()
        member this.Code with get() = code and set(v) = code <- v
        member this.Text with get() = text and set(v) = text <- v
        member this.File with get() = file and set(v) = file <- v
        member this.Bytes with get() = bytes and set(v) = bytes <- v
        member this.Cookie with get() = cookie and set(v) = cookie <- v

        member this.ContentType with get() = contentType and set(v) = contentType<-v
        member this.ContentEncoding with get() = contentEncoding and set(v) = contentEncoding<-v

        /// Add a header name,value pair, e.g. "Content-Disposition" "attachment; filename=\"mystrain.zip\""
        member this.AddHeader(name:string,content:string) = headers <- (name,content)::headers

        /// Retrieve user added headers
        member this.GetUserHeaders() = List.rev headers
    
    end

    type Session = { sid : int ; user : string ; uid : int ; token : string ; ext : obj}

    /// Default html 200 response
    type HttpResponse(html:string) as this = class
        inherit Response(200,"text/html")
        do
            this.Text <- html
    end

    /// XML response handler
    type XMLResponse(x:string) as this = class
        inherit Response(200,"text/xml")
        do
            this.Text <- x
    end

    /// Returns a general application/octet-stream
    type BinResp(d:byte array) as this = class
        inherit Response(200,"application/octet-stream")
        do
            this.Bytes <- d
    end

    /// JPeg response handler
    type JPEGResponse(img:byte array) as this = class
        inherit Response(200,"image/jpeg")
        do
            this.Bytes <- img
    end
                    
    type Http404(html:string) as this = class
        inherit Response(404,"text/html")
        do
            this.Text <- html
    end    
    
    // support for 303 Redirect
    type Http303(location:string) as this = class
        inherit Response(303,"text/html")
        do
            this.AddHeader("Location",location)
    end

//    type MyLock() = class 
//        let x = 9
//        do 
//            ()
//    end

    type FSS(port:int) = class
        let mutable minLogLevel = 4
        let myLock = (1,2) // Need object not value for locking
        
        /// FIXFIX - not sure when the threads ever get removed from this data structure, if ever?
        let threadTracker = HashSet<Thread>()
        let lockThreadTracker() = Monitor.Enter(myLock)
        let unlockThreadTracker() = Monitor.Exit(myLock)
        let tcpListener = new TcpListener(System.Net.IPAddress.Any,port)

        ///
        let log level (s:string) =
            try
                if level >= minLogLevel then printf "%s" s
            with _ -> ()
        do
            ()

        member this.halt() =
            // End running threads
            this.Log 2 (sprintf "Closing %d threads...\n" (threadTracker.Count))
            this.Log 2 "    Closing listener\n"
            tcpListener.Stop()
            lockThreadTracker()
            for thread in threadTracker do
                this.Log 2 (sprintf "    Closing thread : %s...\n" thread.Name)
                thread.Abort()
            unlockThreadTracker()
            this.Log 2 "done closing threads\n"

        /// Main entry point to run web server    
        member this.run() =
            Thread.CurrentThread.Name <- "main run thread"
            lockThreadTracker()
            threadTracker.Add(Thread.CurrentThread) |> ignore
            unlockThreadTracker()
            /// Listening tcp process
            tcpListener.Start() // Initiate listening
            /// Wait for connections on main tcp socket,
            /// count the number of connections so far, spawning handlers for
            /// each connection
            let rec waitConnections n =
                sprintf "fssmain: top pre accept socket n=%d\n" n |> log 1
                use s = tcpListener.AcceptSocket()
                sprintf "fssmain: top post accept socket %d\n" n |> log 1

                let runner2(s:Socket) (id:int) =
                    sprintf "fss%d: top runner async, create network stream from socket\n" id |> log 1
                    use ns = new NetworkStream(s) 
                    sprintf "fss%d: top post network stream creation\n" id |> log 1
                    this.handle ns id 
                    sprintf "fss%d: top post handle, closing socket\n" id |> log 1
                    s.Close()
                    sprintf "fss%d: top post close closed socket\n" id |> log 1
                    lockThreadTracker()
                    threadTracker.Remove(Thread.CurrentThread) |> ignore
                    unlockThreadTracker()

                let x = fun () -> runner2 s n
                let t = new Thread(x)
                t.Name <- sprintf "runner2 %s" (DateTime.Now.ToLongTimeString())
                lockThreadTracker()
                threadTracker.Add(t) |> ignore
                unlockThreadTracker()
                sprintf "  tracking thread %s\n" t.Name  |> log 2
                t.Start()
                sprintf "fssmain: post async child fork\n" |> log 1
                stdout.Flush()
                waitConnections (n+1)
            waitConnections 0

        abstract doGet : int->StreamWriter->string->Map<string,string>-> Response
        abstract doPost : int->Stream->StreamWriter->string->Map<string,string>-> Response
    
        default this.doGet _(*handlerId*) _(*sr*) path _(*headers*) =
            sprintf "doGet default %s\n" path |> log 1
            HttpResponse("Hello world") :> Response
    
        /// Default post handler that just replies with hello world
        default this.doPost _(*handlerId*) _(*sr*) _(*sw*) path _(*headers*) =
            sprintf "doPost default %s\n" path |> log 1
            HttpResponse("Hello world") :> Response
   
        /// Emit log messages.  Level (int), Message (String).  Larger level N indicates less detailed higher level messages
        member this.Log (level:int) (s:string) = log level s
        member this.LogLevel
                 with get() = minLogLevel and
                      set(n) = minLogLevel <- n

        /// Handler for accepted connections.  Takes a network stream and its id as arguments
        member this.handle (ns:NetworkStream) (id:int)  =
            sprintf "fss%d: top of handle\n" id |> log 1

            /// Writer for sending reply to network stream
            let sw = new StreamWriter(ns)
            sprintf "fss%d: made StreamWriter\n" id |> log 1

            /// This is  for reading further from the network stream
            let bs = new BufferedStream(ns)
            sprintf "fss%d: made BufferedStream\n" id |> log 1

            let sendHeader k v = 
                sprintf "fss%d: ResponseHeader-->%s: %s\n" id k v |> log 1
                sw.Write(sprintf "%s: %s\r\n" k v)

            /// Send response header including the HTTP code and message associated
            let sendResp code message =
                sw.Write(sprintf "%s %d %s\r\n" protocolVersion code message)
                sendHeader "Date" (DateTime.Now |> string)
                sendHeader "Server" (sprintf "Fss %s" version)
                

            let endHeaders() = sw.Write("\r\n")
            let sendErr code msg =
                sendResp code msg
                sendHeader "Content-Type" "text/html"
        
            /// Handle a get or post request for a particular path with headers    
            let runCmd keepAlive cmd path headers =
                sprintf "fss%d: RunCmd: %s %s ***********\n" id cmd path |> log 2
                let resp = 
                    try
                        match cmd with
                            | "GET" -> this.doGet id sw path headers 
                            | "POST" -> 
                                    sprintf "fss%d: Executing post\n" id |> log 2
                                    this.doPost id bs sw path headers 
                            | _ -> Response(500,"text/html")
                    with x ->
                        let resp = Response(500,"text/html")
                        resp.Text <- sprintf "<PRE>\n%s\n%s\n</PRE>" x.Message x.StackTrace
                        resp
                               
                // Start constructing the reply starting with the response code   
                sprintf "fss%d: finished getting connection, sending response code\n" id |> log 1

                // Send http code and matching explanation for the code
                sendResp resp.Code (explanation resp.Code)

                // Optionally set a cookie if the response requested it
                match resp.Cookie with
                    | Some(c) -> sendHeader "Cookie" c
                    | None -> ()
                // Tell them we are ok with keeping connection alive if they requested it
                if keepAlive then sendHeader "Connection" "Keep-Alive"

                // Include any user added headers
                for k,v in resp.GetUserHeaders() do
                    sendHeader k v
                
                try
                    if resp.File <> "" then
                        // We need to send contents of a file
                        // TODO: .Net 4.0 System.IO.MemoryStream
                        //use fs = new FileStream(resp.File, FileMode.Open, FileAccess.Read, FileShare.Read)
                        //use t = new BinaryReader(fs)
                        //let sz = File.Get(resp.File)
                        //let l = sz
                        sprintf "fss%d: Sending file '%s'\n" id resp.File  |> log 2
                        let data = File.ReadAllBytes(resp.File)
                        sprintf "fss%d: Sending %d bytes\n" id (data.Length) |> log 1
                    
                        sendHeader "Content-Length" (string(data.Length))
                        sendHeader "Content-Type" (if resp.File.EndsWith(".html") then "text/html" else if resp.File.EndsWith(".css") then "text/css" else "text/plain")
                        endHeaders()
                    
                        sw.Flush()
                        // FIXIFIX - potential deadlock if can't send all the bytes out.  Needs timeout (threaded though, so at worst
                        // we get a buildup of stuck jobs, still needs an exit strategy
                        ns.Write(data,0,data.Length)
                        sprintf "fss%d: Sent %d bytes, flushing\n" id data.Length |> log 1
                        ns.Flush()
                        //sw.Flush()
                        if not keepAlive then 
                            sprintf "fss%d: closing socket since no keep-alive\n" id |> log 2
                            sw.Close()
                        //t.Close()
                    elif resp.Bytes.Length <> 0 then
                        sprintf "fss%d: Binary response, sending %d bytes \n" id resp.Bytes.Length |> log 1
                        sendHeader "Content-Length" (string(resp.Bytes.Length))
                        sendHeader "Content-Type" resp.ContentType
                        match resp.ContentEncoding with 
                            | Some(e) -> sendHeader "Content-Encoding" e
                            | None -> ()
                        endHeaders()
                        // Stream writer was handling the texty parts, make sure it gets flushed before we start writing out binary parts
                        sw.Flush() 

                        (*
                        let b2s (b:byte) =
                            match b with
                                | 0uy -> "\0 "
                                | x -> sprintf "%3c" (char x)

                        if true then
                            let l = min (resp.Bytes.Length-1) 255
                            let row1 = [for i in {0..l} ->  (resp.Bytes.[i]|> b2s)]
                            let row2 = [for i in {0..l} -> sprintf "%3x" (resp.Bytes.[i])]
                            printfn "row1= %s" (String.Join("",row1))
                            printfn "row2= %s" (String.Join("",row2))
                        *)
                        ns.Write(resp.Bytes,0,resp.Bytes.Length)
                        sprintf "fss%d: Sent\n"  id |> log 1
                        ns.Flush()
                        if not keepAlive then sw.Close()
                    else
                        sprintf "fss%d: Text reply type:%s length:%d\n" id resp.ContentType resp.Text.Length |> log 1
                        sendHeader "Content-Type" resp.ContentType 

                        // Convert response to bytes, since we need to report the #bytes we are going to return and
                        // unicode messes up the #chars to #byte calculation
                        let byteCount = System.Text.Encoding.UTF8.GetByteCount(resp.Text.ToCharArray())
                        sendHeader "Content-Length" (string(byteCount))
                        endHeaders()
                        sw.Write(resp.Text)
                        sw.Flush()
                        if not keepAlive then sw.Close()
                with 
                    | x ->
                            sprintf "fss%d: IO exception writing to socket '%A'\n%s\n" id x x.StackTrace |> log 999
            // End runCmd function def
            // ==============================================================================================

            // Continue with actual handle code..
            try 
                /// Grab bytes from the header until we see a double newline or things break
                let grabHeader() =
                    let ms = new MemoryStream()

                    try 
                        //printf "\ngrabHeader timeout = %d\n" bs.ReadTimeout
                        let rec readOne a b c =
                            //if ms.Length % 100L = 0L && ms.Length > 0L then printf " .. %d" ms.Length
                            //if ms.Length < 16L then Console.Write(".") //  printf "%A\n" (ms.GetBuffer())
                            match bs.ReadByte() with
                                | -1 -> 
                                    sprintf "grabHeader exits via -1\n" |> log 1
                                    ms.ToArray() |> Array.map (char)
                                | 10 when a=13uy && b = 10uy && c = 13uy -> 
                                    sprintf "\ngrabHeader exits after double newline\n" |> log 1
                                    ms.ToArray() |> Array.map (char)
                                | x -> 
                                    let d = byte(x)
                                    //if ms.Length < 16L then printf "(%c)" (char(d))
                                    ms.WriteByte(d)
                                    readOne b c d 
                        sprintf "fss%d: starting grabHeaders\n" id |> log 1
                        let s = new String(readOne 0uy 0uy 0uy)
                        sprintf "\n"  |> log 1
                        sprintf "fss%d: made string from headers\n" id |> log 1
                        sprintf "fss%d: Literal headers len=%d\n----------------------------------\n%s\n----------------------------------\n" id s.Length s |> log 1
                        let headers = s.Split([|"\n"|],StringSplitOptions.None) |> Array.map (fun row -> 
                                                                    match row.IndexOf(" ") with
                                                                        | -1 -> row,""
                                                                        | i -> (row.[..i-1],row.[i+1..row.Length-1].Trim([| '\r' ; '\n' ; ' '|]))
                                                               ) |> Array.filter (fun (a,b) -> a.Length<>0 || b.Length<>0)
                        match headers with
                            | [||] -> None
                            | _ -> Some(headers |> Map.ofSeq)
                    
                    with x ->
                        if ms.Length = 0L then
                            // No foul, the browser probably just gave up on us and has no further questions
                            None
                        else
                            let buffer = new String(ms.ToArray() |> Array.map(char))
                            let clipped (s:string) = if s.Length < 200 then s else s.Substring(0,200)
                            sprintf "fss%d: Exception in grabHeaders after reading %d bytes\n%s\n: %s\n%s\n" id ms.Length (clipped buffer) x.Message x.StackTrace |> log 999
                            None
                
                // DEBUGGING
                sprintf "fss%d: running grab header\n" id |> log 1
                let rec procRequest prevKeepAlive =
                    match grabHeader() with
                        | None -> 
                            sprintf "fss%d: grabHeader returned None\n" id |> log 1
                            ns.Dispose()
                        | Some(headers) ->
                            //let line1 = sr.ReadLine()
                            sprintf "fss%d: procRequest: Headers:\n" id |> log 1
                            headers |> Seq.iter (fun pk -> sprintf "%s -> %s\n" pk.Key pk.Value |> log 1)

                            let keepAlive = prevKeepAlive || match headers.TryFind("Connection:") with
                                                                | Some("Keep-Alive") -> true
                                                                | Some("keep-alive") -> true
                                                                | Some("close") -> false
                                                                | Some("Close") -> false
                                                                | _ -> true

                            if headers.ContainsKey("GET") then // TODO: validate version number
                                match headers.["GET"].Split([| " " |],StringSplitOptions.None) with
                                    | [| path ; _(*version*) |] -> runCmd keepAlive "GET" path headers
                                    | [| path |] -> runCmd keepAlive "GET" path headers
                                    | x-> 
                                        sprintf "fss%d: Unrecognized format '%A'\n" id x |> log 2
                                        sendErr 400 "Bad request syntax"
                            else if headers.ContainsKey("POST") then // TODO: validate version number
                                match headers.["POST"].Split([| " " |],StringSplitOptions.None) with
                                    | [| path ; _(*version*) |] -> runCmd keepAlive "POST" path headers
                                    | [| path |] -> runCmd keepAlive "POST" path headers
                                    | x-> 
                                        sprintf "fss%d: Unrecognized format '%A'\n" id x |> log 2
                                        sendErr 400 "Bad request syntax"
                                    //runCmd keepAlive "POST" (headers.["POST"]) headers
                            else if headers.Count = 0 then
                                sprintf "fss%d:Empty headers sent\n" id |> log 1
                                sendErr 400 "Bad request syntax"

                            else
                                if headers.Count = 0 then
                                    sprintf "fss%d: empty header\n" id |> log 2
                                else
                                    sprintf "fss%d: Unrecognized format2 headers=\n%A\n" id headers |> log 2
                                sendErr 400 "Bad request syntax"
                                //sprintf "fss%d: Unrecognized header command\n" id |> log 2
                                //sendErr 400 "Bad request syntax"
                            //printf "fss%d: Headers: %A\n" id headers
                            sprintf "fss%d: done replying\n" id |> log 1
                            if keepAlive then
                                sprintf "fss%d: keeping alive\n" id |> log 1

                                procRequest true // process another request
                            else 
                                sprintf "fss%d: closing connection, since no keep-alive request\n" id |> log 1
                                sprintf "fss%d: Closing conn\n" id |> log 1
                                ns.Close()

                procRequest false // Process one or more requests
            with x ->
                try
                    sendErr 500 "Internal error"
                    sw.Write(x.ToString())
                    sw.Flush()           
                    sprintf "fss%d: Traceback processing %s\n%s\n" id (x.ToString()) x.StackTrace |> log 999
                    sw.Close()
                    ns.Close()
                with x ->
                    sprintf "fss%d: Giving up on closing connection\n" id |> log 1
            ()       
    
    end


    type UR = { handleId : int ; sr : Stream option ; isPost : bool ; sw : StreamWriter ; path : string ;
                        ud : FSS ; headers : Map<string,string> ; session : Session option ;
                            GET : Map<string,string> } with
        /// Read from input stream 
        member x.ReadPostAsBytes(limit:int) =
                if not x.isPost then failwith "ERROR: ReadPost() requested for non POST request"
                else match x.sr with
                        | None -> failwith "ERROR: ReadPost() no stream to read from.  Non POST requst?"
                        | Some(s) -> // Some stream available
                            let conLen = x.headers.["Content-Length:"] |> int
                            if conLen > limit then failwithf "ERROR: ReadPost content length %d exceeds limit of %d" conLen limit
                            else
                                let buffer = Array.init conLen (fun _ -> 0uy)
                                let rec readWhile offset remaining =
                                    if remaining = 0 then ()
                                    else
                                        let n = s.Read(buffer,offset,remaining)
                                        if n = remaining then ()
                                        else if n = 0 then failwithf "ERROR: end of stream reading %d of %d byes in ReadBytesAsPost" (buffer.Length-remaining) buffer.Length
                                        else
                                            readWhile (offset+n) (remaining-n)
                                readWhile 0 (buffer.Length)
                                buffer
        /// Read up to 2Gb of input and return result as an array of bytes
        member x.ReadPostAsBytes() = x.ReadPostAsBytes(2147483647)
            
        /// Read from POST input stream, return result as a string
        member x.ReadPostAsString() = new String(x.ReadPostAsBytes() |> Array.map (char))
        /// Read from POST input stream messaged with <= limit bytes, return result as a stirng
        member x.ReadPostAsString(limit :int) = new String(x.ReadPostAsBytes(limit) |> Array.map (char))

        member x.parseForm (post : byte array) =
            let text = new String(post |> Array.map (char))
            let unescape (v:string) = v.Replace("+"," ") |> Uri.UnescapeDataString
            text.Split([| '&' |]) |> Array.map (fun s -> match s.Split([|'='|]) with | [| k ; v |] ->(k,unescape v)  | _ -> failwithf "Invalid form args: %s" s) |> Map.ofSeq
        member x.parseMultipart (post : byte array) = Fss.Multipart.parse post
    // ---------------------------------------------------------------------------------

    /// Different function signatures for URL callbacks
    type UDFunc =
        | D0 of (UR->Response)
        | D1 of (UR->string->Response)
        | D2 of (UR->string->string->Response)
        | D3 of (UR->string->string->string->Response)
        | D4 of (UR->string->string->string->string->Response)
        | D5 of (UR->string->string->string->string->string->Response)
        | D6 of (UR->string->string->string->string->string->string->Response)
        | D7 of (UR->string->string->string->string->string->string->string->Response)
                
    /// Dispatch URLs matching a set of pattens to functions
    type UD(port:int,urlsInit : (string * UDFunc) list,logLevel:int,delayStart:bool) as this = class
        inherit FSS(port)
        let urlMatch path (url,_) = Regex.Match(path,url).Success
        let mutable urls = urlsInit

        // question: since path is in urInit, why have a separate path var at all?
        let dispatch urInit path =
            let ok,ur = this.preCheck path urInit
            if not ok then
                this.preCheckFailed ur
            else
                //printf "Testing path='%s'\n urls=%A" path (urls |> List.map (fst))
                match List.tryFind (urlMatch ur.path) urls with
                    | None ->
                        this.Log 1 (sprintf "Unmatched url...")
                        Http404(sprintf "<HTML>Unmatched URL; '%s'\n</HTML>" ur.path) :> Response
                    | Some(url,fn) ->
                        let m = Regex.Match(path,url)
                        sprintf "fss%d UD: matching %s -> %s\n" ur.handleId ur.path url |> this.Log 3
                        try
                            let g (x:int) = m.Groups.[x].Value
                            match m.Groups.Count-1,fn with
                                | 0,D0(fn) -> fn ur // (fn:?> UR->Response) ur
                                | 1,D1(fn) -> fn ur (g 1) //(fn:?> UR->string -> Response) ur (g 1)
                                | 2,D2(fn) -> fn ur (g 1) (g 2) //(fn:?> UR->string -> string->Response) ur (g 1) (g 2)
                                | 3,D3(fn) -> fn ur (g 1) (g 2) (g 3)//(fn:?> UR->string -> string-> string->Response) ur (g 1) (g 2) (g 3)
                                | 4,D4(fn) -> fn ur (g 1) (g 2) (g 3) (g 4) ////(fn:?> UR->string -> string-> string-> string->Response) ur  (g 1) (g 2) (g 3) (g 4)
                                | 5,D5(fn) -> fn ur (g 1) (g 2) (g 3) (g 4) (g 5) //(fn:?> UR->string -> string-> string-> string->Response) ur  (g 1) (g 2) (g 3) (g 4)
                                | 6,D6(fn) -> fn ur (g 1) (g 2) (g 3) (g 4) (g 5) (g 6) //(fn:?> UR->string -> string-> string-> string-> string->Response) ur (g 1) (g 2) (g 3) (g 4) (g 5)
                                | 7,D7(fn) -> fn ur (g 1) (g 2) (g 3) (g 4) (g 5) (g 6) (g 7) //(fn:?> UR->string -> string-> string-> string-> string-> string->Response) ur (g 1) (g 2) (g 3) (g 4) (g 5) (g 6)
                                | _ -> failwith "not supported"
                        with x ->
                            let resp = Response(500,"text/html")
                            resp.Text <- sprintf "<PRE>Error matching UD dispatch function\n%s\n%s\n</PRE>" x.Message x.StackTrace
                            printfn "fss%d UD:%s" ur.handleId resp.Text
                            resp
        let start() =
            this.Log 2 "Started my dispatcher\n"
            try
                this.run()
            with 
                | :? SocketException as exn ->
                    sprintf "Main socket exception, probably closing: %s\n%s" exn.Message exn.StackTrace |> this.Log 1 
                    ()
    
        let stop() =
            this.Log 2 "Stopping my dispatcher"
            this.halt()
    
        do
            this.LogLevel <- logLevel
            if not delayStart then start()
                
        /// Constructor taking port, URL list and setting a default log level
        new (port:int,urls : (string * UDFunc) list) = UD(port,urls,2,false)

        /// Pre filter for security implementation that can veto dispatch
        abstract preCheck :  string->UR->bool*UR
        /// Default implementation lets everything through
        default this.preCheck _(*path*) ur = true,ur 

        /// Result of preCheck failure
        abstract preCheckFailed : UR->Response
        default this.preCheckFailed ur =
            if ur.isPost then
                // take out the post content to avoid problems with follow on
                ur.ReadPostAsBytes() |> ignore
            let r = Response(401,"text/plain")   
            r.Text <- "Authentication required."
            r

        member this.Start() = start()
        member this.Stop() = stop()
        static member parseQueryArgs(s:string) =
                s.Split([| '&' |]) |> Array.map (fun s -> match s.Split([|'='|]) with
                                                                | [| k ; v |] ->(k,Uri.UnescapeDataString(v))
                                                                | _ -> failwithf "Invalid form args: %s" s) |> Map.ofSeq

        override this.doGet (handleId:int) (sw : StreamWriter) (path:string) headers =
            let path',args = match path.IndexOf('?') with
                               | -1 -> path,Map.empty // No ? param section
                               | i -> path.[..i-1],UD.parseQueryArgs (path.[i+1..])

            let ur =  { handleId = handleId ; sw = sw ; ud = this ; sr = None ; isPost = false ; path = path' ; headers = headers ; 
                            session = None ; GET= args}
            dispatch ur path'
            
        override this.doPost (handleId:int) (sr : Stream) (sw : StreamWriter) (path:string) headers =
            let path',args = match path.IndexOf('?') with
                               | -1 -> path,Map.empty // No ?param section
                               | i -> path.[..i-1],UD.parseQueryArgs (path.[i+1..])

            let ur =  { handleId = handleId ; sw = sw ; ud = this ; sr = Some sr ; isPost = true ; path = path' ; headers = headers ; 
                            session = None ; GET= args}
            dispatch ur path'

        member this.AddDispatcher (url:string) (func:UDFunc) =
            urls<- (url,func)::urls
   
    end

    // Helper functions

    /// Standard success Http response
    let http200 text = HttpResponse text :> Response

    /// http page not found response
    let http404 text = Http404(text) :> Response

    /// http redirect
    let http303 location = Http303(location) :> Response

    /// helper for xml responses
    let xmlResp text = XMLResponse(text) :> Response

    /// Standard success plain text response
    let plainText200 text =
        let resp = Response(200,"text/plain")
        resp.Text <- text
        resp

    (*
    /// Example web server implementation
    type MyServer(port:int) as this = class
        inherit Fss.FSS(port)
        do
            printf "Started my server\n"
        
        override this.doGet(path:string) =
            printf "Myget: path=%s\n" path
        
    end
    *)
        
    //createServer 8080

     
