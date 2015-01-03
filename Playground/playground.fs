open System
open Fss.Template
open System.Net
open Fss.Pool
open System.Threading

let t = Template("{{x[2][4]}}")
let page = t.Render([| ("x",
                        box [|
                            [|1;2;3;4;5;6|];
                            [|2;4;6;8;10;12|];
                            [|3;6;9;12;15;18|];
                            [|4;8;12;16;20;24|]
                        |])  |])
printf "page=%s" page

printf "done"
stdin.ReadLine() |> ignore

