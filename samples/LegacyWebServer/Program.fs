// The legacy fss web server and Jinja2-style templates.
//
//   dotnet run --project samples/LegacyWebServer            (serves http://localhost:8080/ until Enter)
//   dotnet run --project samples/LegacyWebServer -- --smoke (starts, makes a few requests, exits)
//
// NOTE: fss's HTTP server is a legacy library kept for existing users. It has no TLS, no request
// size limits beyond what handlers enforce, and templates do NOT HTML-escape output - escape any
// user-supplied text yourself (as below). For new web projects use ASP.NET Core / Giraffe / Falco.
// See docs/web-server.md.

open System
open System.IO
open System.Net
open System.Net.Http
open System.Net.Sockets
open System.Threading
open Fss.Server
open Fss.Template

type Person = { name : string ; city : string ; telephone : int }

let people =
    [| { name = "Prudence" ; city = "LA" ; telephone = 5551234 }
       { name = "Alex" ; city = "Seattle" ; telephone = 5556871 }
       { name = "FiFi" ; city = "New York" ; telephone = 5559888 } |]

/// Templates are loaded from a folder; this one is copied next to the executable
let templates = TemplateServer(Path.Combine(AppContext.BaseDirectory, "templates"))

let index (_:UR) =
    """<html><ul>
         <li><a href="/hello/world">/hello/world</a> - URL parameter</li>
         <li><a href="/add/3/4">/add/3/4</a> - two URL parameters</li>
         <li><a href="/people">/people</a> - template with a loop</li>
         <li><a href="/search?q=fss&amp;page=2">/search?q=fss&amp;page=2</a> - query string</li>
       </ul></html>""" |> http200

/// Regex groups in the route become string arguments (D1 = one argument)
let hello (_:UR) (name:string) =
    sprintf "<html>Hello, %s!</html>" (WebUtility.HtmlEncode name) |> http200

let add (_:UR) (a:string) (b:string) =
    sprintf "%d + %d = %d" (int a) (int b) (int a + int b) |> http200

let peoplePage (ur:UR) =
    let showTable = not (ur.GET.ContainsKey "hide")
    templates.Get("people.html").Render([| ("title", box "People") ; ("showTable", box showTable) ; ("people", box people) |])
    |> http200

/// Query string arguments arrive in ur.GET
let search (ur:UR) =
    let q = ur.GET.TryFind "q" |> Option.defaultValue ""
    sprintf "<html>You searched for <b>%s</b></html>" (WebUtility.HtmlEncode q) |> http200

let routes =
    [ ("^/$", D0 index)
      ("^/hello/([A-Za-z0-9]+)$", D1 hello)
      ("^/add/([0-9]+)/([0-9]+)$", D2 add)
      ("^/people$", D0 peoplePage)
      ("^/search$", D0 search) ]

let freePort () =
    let l = new TcpListener(IPAddress.Loopback, 0)
    l.Start()
    let p = (l.LocalEndpoint :?> IPEndPoint).Port
    l.Stop()
    p

[<EntryPoint>]
let main argv =
    let smoke = argv |> Array.contains "--smoke"
    let port = if smoke then freePort () else 8080
    // delayStart = true: construct now, start on our own thread so main can carry on
    let server = UD(port, routes, 3, true)
    let serverThread = Thread((fun () -> server.Start()), IsBackground = true)
    serverThread.Start()
    printfn "Serving on http://localhost:%d/" port

    if smoke then
        use client = new HttpClient(BaseAddress = Uri(sprintf "http://localhost:%d" port))
        let get (path:string) =
            let rec attempt n =
                try client.GetStringAsync(path).Result
                with _ when n > 0 -> Thread.Sleep 200; attempt (n - 1)
            attempt 10
        let checks =
            [ "/hello/world", "Hello, world!"
              "/add/3/4", "3 + 4 = 7"
              "/people", "<td>Alex</td>"
              "/search?q=%3Cb%3E", "&lt;b&gt;" ]
        let failures =
            [ for (path, expected) in checks do
                let body = get path
                let ok = body.Contains(expected: string)
                printfn "%-20s %s" path (if ok then "ok" else "FAILED")
                if not ok then yield path ]
        server.Stop()
        if failures.IsEmpty then 0 else 1
    else
        printfn "Press Enter to stop"
        Console.ReadLine() |> ignore
        server.Stop()
        0
