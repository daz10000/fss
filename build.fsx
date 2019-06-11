
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.DotNet.NuGet
open System.Reflection

let configuration = "Release"

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs 
)

let release = Fake.Core.ReleaseNotes.load "RELEASE_NOTES.md"

let project = "fss"
let summary = "F# server and db wrapper"

Target.create "AssemblyInfo" (fun _ ->
    AssemblyInfoFile.createFSharpWithConfig "src/Common/AssemblyInfo.fs"
      [ AssemblyInfo.Title project
        AssemblyInfo.Product project
        AssemblyInfo.Description summary
        AssemblyInfo.Version release.AssemblyVersion
        AssemblyInfo.FileVersion release.AssemblyVersion]
        (AssemblyInfoFileConfig(true,false,"Fss.AssemblyInfo"))
)

Target.create "Pack" (fun _ ->
    !! "src/**/*.*proj"
    |> Seq.iter (fun proj ->
                    let folder = System.IO.Path.GetDirectoryName proj
                    printfn "Packing %s" proj
                    Fake.DotNet.Paket.pack (fun p ->
                        { p with
                            ToolPath = ".tool/paket.exe"
                            WorkingDir = folder
                            BuildConfig = configuration
                            Version = release.NugetVersion
                            ReleaseNotes = System.String.Join("\n",release.Notes) // Fake.Core.String release.Notes
                            OutputPath = "."
                        }) 
    ) // end iteration over projects
) // end pack target

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
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "RunTests"
  ==> "Pack"
  ==> "CopyBinaries"
  ==> "All"

Target.runOrDefault "All"
