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
open System.Collections.Generic

let rng = new Random()

type SecUD(port,urls,unsecuredPrefixes:string list,userCheck:string->string->(int*int) option,logLevel) as this = class
    inherit UD(port,urls,logLevel,true)

    let unsecuredPrefixesFinal = "/login"::unsecuredPrefixes

    do
        // Add one of our own internal methods to the dispatcher list.
        // This handles the login request which will come to our class
        this.AddDispatcher "/login.*" (D0 this.login)
        this.Start()

    /// decide if we are going to let a URL request though and 
    /// extract any relevant session info
    override x.preCheck (url:string) (ur:UR) =
        if unsecuredPrefixesFinal |> List.exists (fun p -> url.StartsWith(p)) then true,ur // anyone can try one of these URLS
        else Fss.Cookie.checkCookie ur
            
   
    /// Generate a login session
    member x.login (ur:UR) =
        if ur.isPost then
            let args = ur.ReadPostAsBytes() |> ur.parseForm
            match args.TryFind("user"),args.TryFind("password") with
                | Some(userName),Some("test") ->
                    // Success logging in, create a cookie
                    let redirect = args.TryFind("redirect")
                    let r = Response(match redirect with | None -> 200,"ok" | Some(s) -> 303,"See other")

                    let expires = DateTime.Now.AddMonths(1)
                    
                    let now = DateTime.Now.ToUniversalTime()
                    let csumToString (csum:byte array) = String.Concat(csum |> Array.map (fun b -> sprintf "%02x" b))
                    use sha = SHA256.Create()

                    let sessKey = Fss.Sec.genRandKey()

                    let payLoad = sprintf "user=%s|sesskey=%s|created=%d" userName sessKey (DateTime.UtcNow.ToBinary())
                    let cSum = sha.ComputeHash(Encoding.UTF8.GetBytes(payLoad.ToCharArray() )) |> csumToString

                    let contents = sprintf "%s|csum=%s" payLoad cSum

                    let host = ur.headers.["Host:"].Split([| ':' |]).[0]

                    let cookieText = sprintf "%s;Expires=%s;Domain=%s;Path=/" contents (now.AddMonths(1) |> Fss.Cookie.formatCookieDate) host
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
end


let showHeaders(ur:UR) =
    String.Join(";",ur.headers) |> http200

let showCookie (ur:UR) =
    match ur.headers.TryFind("Cookie:") with
        | None -> sprintf "No cookie, headers= %s" (String.Join(";",ur.headers))
        | Some(x) -> sprintf "Cookie\n%s" x

    |> http200


/// Dispatchers recognized by web service
let urls = [ 
                ("^/cookieshow$",D0 showCookie) ;
                ("^/headers$",D0 showHeaders) 
           ]

let unsecuredURLS = []
let userCheck (user:string) (password:string) =
    if password = "test" then Some(1,1) else None

/// Create a web server on port 8080 to dispath the given set of functions
let server = SecUD(8080,urls,unsecuredURLS, userCheck,1)

printf "Exitted"
stdin.ReadLine() |> ignore