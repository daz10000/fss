open System
open Fss.Template
open System.Net
open Fss.Pool
open System.Threading


let template = Template("{% if x%}hello{%endif%}")
let page = template.Render([| ("x",box true) |])


printf "page=%s" page

printf "done"
stdin.ReadLine() |> ignore

