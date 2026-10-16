module Shared

open System
open System.IO
open NUnit.Framework

/// Look for a connection string, first in an environment variable, then in a file
/// searched for in the current folder and up to three parents (the legacy mechanism).
let tryConnString (envVar:string) (fileName:string) =
    match Environment.GetEnvironmentVariable envVar with
    | s when not (String.IsNullOrWhiteSpace s) -> Some (s.Trim())
    | _ ->
        [ "." ; ".." ; "../.." ; "../../.." ; "../../../.." ]
        |> List.map (fun folder -> Path.Combine(folder, fileName))
        |> List.tryFind File.Exists
        |> Option.map (fun path -> File.ReadAllText(path).Trim())

/// Connection string for a database server, or mark the calling test(s) as ignored
/// when the server is not configured. This keeps `dotnet test` green on a machine
/// with no Postgres / MySQL, while CI (which sets the env vars) runs everything.
let requireConnString (envVar:string) (fileName:string) =
    match tryConnString envVar fileName with
    | Some s -> s
    | None ->
        Assert.Ignore(sprintf "Set %s (or create %s) to run these tests" envVar fileName)
        failwith "unreachable"

/// Kept for compatibility with older test code: fails rather than ignores.
let getConnStringGeneral (fileName:string) =
    match tryConnString "__no_env_var__" fileName with
    | Some s -> s
    | None ->
        failwithf "ERROR: expected %s file with connstring in folder %s"
            fileName (Directory.GetCurrentDirectory())
