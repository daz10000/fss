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

end
