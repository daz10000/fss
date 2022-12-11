module Shared

open System.IO

let getConnStringGeneral (fileName:string) =
    match ([for folder in ["." ; ".." ; "../.." ;  "../../.."] -> Path.Combine(folder,fileName)] |> List.tryFind (fun x -> File.Exists(x))) with
    | None ->
        failwithf "ERROR: expected %s file with connstring in folder %s" 
            fileName
            (System.IO.Directory.GetCurrentDirectory())
    | Some path ->
        System.IO.File.ReadAllText(path)
