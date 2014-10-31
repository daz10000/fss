namespace Fss

type FormPart = { header:Map<string,string> ; body : byte [] ; contentType : string option}

/// Parse multipart form responses from web submissions
module private Multipart =
    open System.IO


    /// Parse bytes comprising a multipart form
    let parse (bytes:byte []) =
        let l = bytes.Length
        let inline ws c = c=10uy || c=13uy

        let rec findEOL i =
            if i>=l then l-1
            elif (bytes.[i] |> ws) && 
                    (i=l-1 || bytes.[i+1]|> ws |> not) then i
            else findEOL (i+1)

        let rec find (a:byte[]) i j =
            if a.[i] = bytes.[j] then
                if i=a.Length-1 then Some(j-a.Length+1)
                elif j=bytes.Length-1 then None
                else 
                    match find a (i+1) (j+1) with
                        | Some(x) -> Some(x)
                        | None -> find a 0 (j+1)
            elif j=bytes.Length-1 then None
            else find a 0 (j+1)

    
        /// Skip over header lines terminated by newlines
        /// till we find an empty line
        let rec findEOH i =
            if i=l-1 then i
            elif i<= l-2 && (bytes.[i] |> ws) && (bytes.[i]=bytes.[i+1]) then i+1 // mac or window
            elif i<= l-4 && (bytes.[i] = 13uy) && // PC style \r\n\r\n
                    (bytes.[i+1] = 10uy) &&
                    (bytes.[i+2] = 13uy) &&
                    (bytes.[i+3] = 10uy) then i+3
            else findEOH (i+1) 

        let firstEOL = findEOL 0
        let eob  = seq { for i in firstEOL.. -1 .. 0 -> i } |> 
                        Seq.find (fun i -> bytes.[i] |> ws |> not) 
        let newlineLen = match firstEOL-eob with
                            | 1 -> 1
                            | 2 -> 2
                            | _ as x -> failwithf "ERROR: unexpected newline length %d" x
        // Find multipart boundary string
        let border = bytes.[..eob]
    
        let splitOneHeader (s:string) =    
            match s.IndexOf(':') with
                | -1 -> failwithf "ERROR: bad header '%s'" s
                | _ as i -> s.[..i-1].ToLower(),s.[i+1..]

        let rec parseMultipart startSection =      seq{
                    match find border 0 startSection with
                        | None -> failwithf "ERROR: didn't find terminal boundary"
                        | Some(startNextBoundary) ->
                            let eoh = findEOH (startSection+1)
                            let headers = new string(bytes.[startSection..eoh] |>                                                                                                                                                             
                                            System.Text.Encoding.UTF8.GetChars)
    
                          
                            let headers = headers.Split([| '\n' ; '\r' |],
                                                System.StringSplitOptions.RemoveEmptyEntries) |>
                                            Array.map(splitOneHeader) |>
                                            Map.ofArray

                            yield {header=headers ; body = bytes.[eoh+1..startNextBoundary-1-newlineLen];
                                    contentType = (match headers.TryFind("content-type") with 
                                                    |None-> None
                                                    |Some(x) -> Some(x.Trim()))}
                            // startNextBoundary
                            //  v
                            //  BOUNDARYBOUNDARY-- (can have CRLF on end still but this marks the end)
                            //                   ^
                            //                    L
                            if startNextBoundary<l-border.Length-2 &&
                                bytes.[startNextBoundary+border.Length] = 45uy && bytes.[startNextBoundary+border.Length+1] = 45uy then
                                    () // DONE - hit end
                            else 
                                yield! parseMultipart (startNextBoundary+border.Length)
                }
    
        parseMultipart (firstEOL+1) |> Array.ofSeq