# Web server (`fss` package): legacy

> **Status: legacy.** The `fss` package's HTTP server predates ASP.NET Core and is kept for existing
> applications. It gets security and correctness fixes but no new features. For new projects use
> ASP.NET Core (with [Giraffe](https://github.com/giraffe-fsharp/Giraffe) or
> [Falco](https://github.com/pimbrouwers/Falco) for F#). The database packages don't depend on it.

## Security limitations: read before exposing to a network

- **No TLS.** Run it behind a reverse proxy (nginx, Caddy, IIS, YARP) that terminates HTTPS.
- **Templates don't HTML-escape.** `{{ value }}` is inserted verbatim. Escape user-controlled text with
  `System.Net.WebUtility.HtmlEncode` before passing it to a template, or you have XSS.
- **`Fss.Cookie.checkCookie` is not authentication.** Its checksum is an unkeyed SHA256, so anyone can
  forge a cookie for any user. It doesn't check the session against server state or check expiry. Use
  an HMAC with a server secret, or better, a real auth stack.
- **Request size limits are up to you.** `UR.ReadPostAsBytes()` with no argument accepts up to 2 GB. Always
  call `ReadPostAsBytes(limit)`. Headers aren't size-limited and there are no socket timeouts, so put a
  proxy with limits in front.
- **Static files**: `Response.File` serves whatever path you give it. Validate paths yourself
  (`Path.GetFullPath` and a check that the result is under your root).
- **Route regexes** are matched with `Regex.Match`. Anchor them (`^...$`), especially the unsecured
  URL list of `SecUD`, or `/login` will also match `/admin/x/login`.
- The default error handler returns the exception message and stack trace (HTML-encoded). Pass your own
  `ErrorHandler` in production.

## Basics

```fsharp
open Fss.Server

let hello (ur:UR) (name:string) = sprintf "Hello %s" (System.Net.WebUtility.HtmlEncode name) |> http200
let add (_:UR) (a:string) (b:string) = sprintf "%d" (int a + int b) |> http200

let routes =
    [ "^/hello/([A-Za-z]+)$", D1 hello              // regex groups become string arguments
      "^/add/([0-9]+)/([0-9]+)$", D2 add ]          // D0..D7 = number of groups

let server = UD(8080, routes)                       // starts serving immediately (blocks)
// or: let server = UD(8080, routes, logLevel, true) ; Thread(server.Start).Start() ; ... ; server.Stop()
```

### The request: `UR`

| Member | |
|---|---|
| `ur.path`, `ur.isPost`, `ur.headers : Map<string,string>` | Request basics. Header keys include the trailing colon, e.g. `"Host:"`. |
| `ur.GET : Map<string,string>` | Query-string arguments. |
| `ur.ReadPostAsBytes(limit)` / `ReadPostAsString(limit)` | Request body. Always pass a limit. |
| `ur.parseForm bytes` | Parse `application/x-www-form-urlencoded`. |
| `ur.parseMultipart bytes` | Parse `multipart/form-data`. |
| `ur.session` | Set by `SecUD` when a request passes its check. |

### Responses

`http200 html`, `http404 html`, `http303 location`, `xmlResp text`, `plainText200 text`, or build a
`Response(code, contentType)` and set `.Text`, `.Bytes` or `.File`, `.AddHeader(name, value)`, and
`.Cookie` (sent as `Set-Cookie`).

### `SecUD`

A `UD` that vetoes requests unless a `userCheck` function accepts them (or the URL matches an unsecured
pattern), redirecting to your login handler otherwise. See the security notes above. The session check
is only as strong as the `userCheck` you supply.

## Templates

See [templates.md](templates.md).

## Sample

[`samples/LegacyWebServer`](../samples/LegacyWebServer) shows routes, query strings, templates and
escaping. `dotnet run --project samples/LegacyWebServer -- --smoke` runs a self-test.
