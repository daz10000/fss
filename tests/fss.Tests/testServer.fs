module testServer

open NUnit.Framework
open Fss.Server
open System.Text
open System.IO

let ud = UD(10000,[],1,true)

let fakeUR = { handleId = 0 ; sr = None ; isPost = true ; sw = null ; path ="/foo" ; 
                        ud = ud ; headers= Map.empty ; session =None ;
                            GET = Map.empty }

[<TestFixture>]
type TestServerBasic() = class 
    [<Test>]
    member x.TestFormParse() =
        let input = "nodename=http%3A%2F%2Flocalhost%3A8087+"    
        let x = fakeUR.parseForm(Encoding.UTF8.GetBytes(input))
        let f = match x.TryFind("nodename") with
                    | None -> "not found"
                    | Some(x) -> x
        let expected = "http://localhost:8087 "
        if not (f = expected ) then
            Assert.Fail(sprintf "Expected: '%s',  Got: '%s'" expected f)


    [<Test>]
    member x.TestErrorHandler() =
        let mutable handled = false
        let handle _ _ _ =
            handled <- true
            Response(500, "text/html")

        let routes = [("^/bad/$", D0 (fun _ -> failwith "explode"))]

        let ud = UD(10000, routes, 1, true, handle)

        let header = System.Text.Encoding.ASCII.GetBytes("GET /bad/ HTTP/1.1")

        use stream = new MemoryStream(256)

        stream.Write(header, 0, header.Length)
        stream.Position <- 0L
        Assert.That stream.CanWrite

        Assert.False(handled)
        ud.handle stream 0

        Assert.True(handled)
end
