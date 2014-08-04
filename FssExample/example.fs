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

open Fss.Server
open Fss.Template
open System
open System.IO
open System.Net
open System.Security.Cryptography
open System.Text
open System.Threading

/// Reverse a string argument
let reverse (ur:UR) (s:string) = 
    new string(s.ToCharArray() |> Array.rev) |> http200
    
/// Report current date/time
let time (ur:UR) = 
    DateTime.Now.ToLocalTime().ToLongDateString() |> http200

/// Add two integers together
let add (ur:UR) (a:string) (b:string) = 
    sprintf "%d + %d = %d" (int a) (int b) ((int a) + (int b)) |> http200


let rng = new Random()

/// Generate some random pet XML
let pets (ur:UR) (n:string) =
    let species = [ "cat" ; "dog" ; "pidgeon" ; "iguana" ; "yeast" ; "python" ; "rattlesnake" ]
    let names = ["alex" ; "prudence" ; "sally" ; "jim" ; "billy" ; "foo foo" ; "mavis" ; "andy" ; "Mr Bean" ]
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n    <MENAGERIE>\n" +
    String.Join("",
                    [|for i in {1..int n} ->
                        sprintf "
                        <ANIMAL> 
                            <NAME> %s </NAME>
                            <SPECIES> %s </SPECIES>
                        </ANIMAL>" (List.nth names  (rng.Next(0,names.Length))) (List.nth species (rng.Next(0,species.Length)))
                        |]
                ) + "\n    </MENAGERIE>\n" |> xmlResp

/// Will fail at runtime because it doesn't return an http Response class
let badHandler1 (ur:UR) =
    3.14159

/// Will fail at runtime because it doesn't have a valid type signature that matches the RE
let badHandler2 (ur:UR) (f:float) =
    http200 "boo!"

/// Handle a post request, reading from the input stream
let imgUpload(u:UR)  =
    if not u.isPost then
        http200 "<HTML> Please use a POST request </HTML>"
    else
        let img = u.ReadPostAsBytes(100000000) // Read up to 100Mb of images
        use outF = File.Create("dump.jpg")

        // Note this assumes the data stream is pure image.  They could also
        // send a multipart form in which case we would want to parse the data stream
        // further
        outF.Write(img,0,img.Length) |> ignore
        outF.Close()
        sprintf "<HTML>Received %d byte image s </HTML>" img.Length |> http200

/// Example of a non-HTML response with additional headers.  Response is the base
/// class of HttpResponse.  If you need a custom response, this is a good example
let imgDownload(u:UR) =
    if not (File.Exists("prudence.jpg")) then "<HTML> prudence.jpg not present in CWD, run in data directory </HTML>" |> http200
    else
        // Create a base response, type it as a jpeg
        let binResp = Response(200,"image/jpeg")

        // Add in the data (use .Text for a text response, or .File to refer to a file that should be sent)
        binResp.Bytes <- File.ReadAllBytes("prudence.jpg")

        // Optionally set additional headers.  In this case we want to offer the user a download
        // but removing this will display the image in the web page.
        binResp.AddHeader("Content-Disposition", "attachment; filename=\"prudence.jpg\"")

        // To simply return a jpeg, we could have equivalently written
        // JPEGResponse(File.ReadAllBytes("prudence.jpg"))

        // return the response
        binResp

/// Demonstrates parameter parsing, e.g. test this with
/// http://localhost:8080/nameargs/platt?cow=6&dog=coffee%20cup
/// Should extract both the URL parameter "platt" and the query args into a dictionary
let nameAndArgs (ur:UR) (name:string) = 
    sprintf "<PRE>\nName=%s\n%s</PRE>" 
        name (String.Join("\n",seq { for kv in ur.GET -> sprintf "key=%s  value=%s" kv.Key kv.Value} )) |> http200


/// test we have all our string/byte lengths correct with unicode support
/// http://xkcd.com/1209/
let testUnicode1 (u:UR) = "™0123456789" |> http200

/// Explicitly code response type as unicode
let testUnicode2 (u:UR) = 
    let r = Response(200,"text/UTF8")
    r.Text<- "™0123456789"
    r

let cookieMeWith (u:UR) (cookie:string) =
    let r = Response(200,"text/html")
//    print "Set-Cookie:UserID=XYZ;\r\n"
//    print "Set-Cookie:Password=XYZ123;\r\n"
//    print "Set-Cookie:Expires=Tuesday, 31-Dec-2007 23:12:40 GMT";\r\n"
//    print "Set-Cookie:Domain=www.tutorialspoint.com;\r\n"
//    print "Set-Cookie:Path=/perl;\n"

    //r.Cookie<-cookie
    let expires = DateTime.Now.AddMonths(1)
    let formatCookieDate(x:DateTime) =
        // Wdy, DD Mon YYYY HH:MM:SS GMT
        sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" 
            (x.DayOfWeek.ToString())
            x.Day
            ([| "Jan" ; "Feb" ; "Mar" ; "Apr" ; "May" ; "Jun" ; "Jul" ; "Aug" ; "Sep" ; "Oct" ; "Nov" ; "Dec" |].[x.Month-1])
            x.Year
            x.Hour
            x.Month
            x.Second
    let now = DateTime.Now.ToUniversalTime()
    r.AddHeader("Set-Cookie",sprintf "x=%s;Expires=%s;Domain=.feb17.org;Path=/" cookie (now.AddMonths(1) |> formatCookieDate))
    //r.AddHeader("Set-Cookie","Domain=.feb17.org")
    //r.AddHeader("Set-Cookie","Path=/")
    //r.AddHeader("Set-Cookie",sprintf "x=%s" cookie)
    let headers = String.Join(";",u.headers)
    r.Text <- sprintf "Cookie me request for '%s', headers=%s" cookie headers
    r

let showHeaders(ur:UR) =
    String.Join(";",ur.headers) |> http200


let showCookie (ur:UR) =
    match ur.headers.TryFind("Cookie:") with
        | None -> sprintf "No cookie, headers= %s" (String.Join("<BR>",ur.headers))
        | Some(x) -> sprintf "Cookie\n%s" x

    |> http200

/// Deliberately slow response
let slow (ur:UR) (seconds:string) =
    Thread.Sleep(int seconds * 1000)
    (int seconds) |> sprintf "You were delayed %d seconds" |> http200


/// Prompt user for name/password
let login (ur:UR) =
    if ur.isPost then
        let args = ur.ReadPostAsBytes() |> ur.parseForm
        match args.TryFind("user"),args.TryFind("password") with
            | Some(userName),Some("test") ->
                // Success logging in, create a cookie
                let redirect = args.TryFind("redirect")
                let r = Response(match redirect with | None -> 200,"ok" | Some(s) -> 303,"See other")
                let expires = DateTime.Now.AddMonths(1)
                let formatCookieDate(x:DateTime) =
                    // Wdy, DD Mon YYYY HH:MM:SS GMT
                    sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" 
                        (x.DayOfWeek.ToString())
                        x.Day
                        ([| "Jan" ; "Feb" ; "Mar" ; "Apr" ; "May" ; "Jun" ; "Jul" ; "Aug" ; "Sep" ; "Oct" ; "Nov" ; "Dec" |].[x.Month-1])
                        x.Year
                        x.Hour
                        x.Month
                        x.Second

                let now = DateTime.Now.ToUniversalTime()
                let csumToString (csum:byte array) = String.Concat(csum |> Array.map (fun b -> sprintf "%02x" b))
                use sha = SHA256.Create()

                let sessId = rng.Next()

                let payLoad = sprintf "UserId=%s|SessId=%d|Created=%d" userName sessId (DateTime.UtcNow.ToBinary())
                let cSum = sha.ComputeHash(Encoding.UTF8.GetBytes(payLoad.ToCharArray() )) |> csumToString

                let contents = sprintf "%s|Csum=%s" payLoad cSum

                let host = ur.headers.["Host:"].Split([| ':' |]).[0]

                let cookieText = sprintf "%s;Expires=%s;Domain=%s;Path=/" contents (now.AddMonths(1) |> formatCookieDate) host
                r.AddHeader("Set-Cookie",cookieText)
                match redirect with
                    | None -> r.Text<-sprintf "Set cookie|%s" cookieText
                    | Some(newURL) -> 
                        r.AddHeader("Location",newURL)
                        r.Text <- sprintf "Returning to %s" newURL
                r
            | _,_ ->
                http200 "failure"
    else
        let redirect = match ur.GET.TryFind("redirect") with
                            | Some(r) -> r
                            | None -> ""

        let t = Template( "<HTML>
                    <FORM METHOD=\"POST\" action=\"/login\">
                    <input type=hidden name=redirect value=\"{{redirect}}\">
                    <table>
                    <tr>
                        <td>User</td>
                        <td><input type=text name=user></input></td>
                    </tr>
                    <tr> 
                        <td>Password</td> 
                        <td><input type=password name=password></input></td>
                    </tr>
                    <tr><td></td><td><input type=submit value=login></td></tr>
                    </FORM>
                </HTML>")
        t.Render([| ("redirect",redirect) |]) |> http200
            
/// F# assertions can't be caught by the regular traceback mechanism.  Will open
/// a dialog when in DEBUG mode, or ignore the assert in release mode
/// http://msdn.microsoft.com/en-us/library/dd233187.aspx
let testAssert(u:UR) =
    printf "Testing assert failure"
    assert(false)
    http200 "<HTML> passed by assert</HTML>"

type TemplateDemo1 = { name : string ; telephone : int ; city : string}
let template1 (ur:UR) =
    let ts = TemplateServer(@"..\..\templates")

    let people = [| 
                        {name="Prudence" ; telephone = 5551234 ; city = "LA" } ;
                        {name="Alex" ; telephone = 5556871 ; city = "Seattle" } ;
                        {name="FiFi" ; telephone = 5559888 ; city = "New York" } ;
                        |]


    let t = ts.Get("template1.html")
    t.Render([| ("peeps",people) |]) |> http200

let index (u:UR) =
    "<html>
        <ul>
            <li> <a href=\"/reverse/ABCDEFGH\">/reverbse/ABCDEFGH reverse ABCDEFGH</a>
            <li> <a href=\"/time\">/time current time</a>
            <li> <a href=\"/add/3/4\">/add/3/4 add 3+4</a>
            <li> <a href=\"/bad1\">/bad1 bad handler 1</a>
            <li> <a href=\"/bad2\">/bad2 bad handler 2</a>
            <li> <a href=\"/pets/10\">/pets/10</a>
            <li> <a href=\"/template1\">/template1</a>
            <li> <a href=\"/imgupload\">/imgupload</a>
            <li> <a href=\"/imgdownload\">/imgdownload</a>
            <li> <a href=\"/slow/10\">/slow10</a>
            <li> <a href=\"/cookieset/darren1970\">/cookieset/darren1970</a>
            <li> <a href=\"/cookieshow\">/cookieshow</a>
            <li> <a href=\"/headers\">/headers</a>
            <li> <a href=\"/nameargs/maryhadalittlelamb\">/nameargs/maryhadalittlelamb</a>
            <li> <a href=\"/assert\">/assert</a>
            <li> <a href=\"/unicode\">/unicode</a>
            <li> <a href=\"/login\">/login  (try user=daz password=test)</a>
            <li> <a href=\"/login?redirect=/time\">/login then time</a>
        </ul>
    </ul>
    " |> http200

/// Dispatchers recognized by web service
let urls = [ 
                ("^/reverse/([A-Za-z0-9/]*)",D1 reverse) ; 
                ("^/time/?",D0 time);
                ("^/add/([0-9]+)/([0-9]+)$" ,D2 add) ;
                //("^/bad1", D0 badHandler1 ) ; // Can't even compile these any more with V1 features
                //("^/bad2" , D0 badHandler2 ) ;
                ("^/pets/([0-9]+)",D1 pets) ;
                ("^/template1",D0 template1) ;
                ("^/imgupload", D0 imgUpload) ;
                ("^/slow/([0-9]+)$", D1 slow) ;
                ("^/imgdownload", D0 imgDownload) ;
                ("^/cookieset/([A-Z0-9a-z]+)$", D1 cookieMeWith) ;
                ("^/cookieshow$",D0 showCookie) ;
                ("^/headers$",D0 showHeaders) ;
                ("^/nameargs/([A-Za-z0-9]+)$", D1 nameAndArgs) ;
                ("^/assert",D0 testAssert) ; 
                ("^/unicode1",D0 testUnicode1) ; 
                ("^/unicode2",D0 testUnicode2) ; 
                ("^/login",D0 login) ;
                ("^/index.html",D0 index) ;
                ("^/$",D0 index) ;
           ]

/// Create a web server on port 8080 to dispath the given set of functions
let server = UD(8080,urls )
server.LogLevel <- 1
// Set it loose
server.run()

printf "Exitted"
stdin.ReadLine() |> ignore