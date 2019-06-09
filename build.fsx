
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

let configuration = "Release"

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs 
)

Target.create "Build" (fun _ ->
    !! "src/**/*.*proj"
    |> Seq.iter (DotNet.build id)
)

// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the
// src folder to support multiple project outputs
Target.create "CopyBinaries" (fun _ ->
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) </> "bin" </> configuration, "bin" </> (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> Fake.IO.Shell.copyDir toDir fromDir (fun _ -> true))
)

(*
Target.create "Test" (fun _ ->
       !! (testDir + "/NUnit.Test.*.dll")
         |> NUnit3.run (fun p ->
             {p with
                   ShadowCopy = false })
   )
*)


Target.create "RunTests" (fun _ ->
    let testFolders = !! "tests/*/" |> Array.ofSeq
    printfn "Found test folders : %A" testFolders
    for test in testFolders do
        DotNet.test
            (fun p ->
                {p with
                    Configuration=DotNet.BuildConfiguration.Release
                    Common = { DotNet.Options.Create() with WorkingDirectory = test}
                }
            )
            test
)

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "RunTests"
  ==> "CopyBinaries"
  ==> "All"

Target.runOrDefault "All"
