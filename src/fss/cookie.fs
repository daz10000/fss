namespace Fss
module Cookie =
    open System
    open System.Text
    open Fss.Server
    open System.Security.Cryptography
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

    let checkCookie(ur:UR) =
        let headers = ur.headers |> Seq.map (fun k -> k.Key.ToLower(),k.Value) |> Map.ofSeq

        // Is there a session cookie
        match headers.TryFind("cookie:") with
            | None -> false,ur // no cookie, don't proceed
            | Some(cookie) -> 
                let cookieParts = cookie.Split([| ';' |]) 
                if cookieParts.Length = 0 then false,ur
                else
                    let sessParts=cookieParts.[0].Split([| '|' |]) |> Seq.choose (fun s -> match s.Split([| '=' |]) with | [| a ; b |] -> Some(a,b) | _ -> None) |> Map.ofSeq
                    match sessParts.TryFind("user"),sessParts.TryFind("sesskey"),sessParts.TryFind("created"),sessParts.TryFind("csum") with
                        | Some(user),Some(sessKey),Some(created),Some(cSumActual) ->
                            let rebuildKey = sprintf "user=%s|sesskey=%s|created=%s" user sessKey created
                            use sha = SHA256.Create()
                            let cSumComputed = sha.ComputeHash(Encoding.UTF8.GetBytes(rebuildKey.ToCharArray() )) |> Fss.Sec.csumToString
                            if cSumComputed <> cSumActual then
                                false,ur // Bad csum check
                            else
                                // Good to go
                                let s : Session = { user=user ; sid = -1 ; uid = -1 ; token = sessKey ; ext = null}
                                true, {ur with session = Some(s) }
                        | _ ->
                            false,ur // Failed to find relevant parts of cookie
   