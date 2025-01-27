// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------


open System
open System.IO
open Fake.IO
open Fake.Api
open Fake.DotNet
open Fake.Tools
open Fake.Core
open Fake.Tools.Git
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.Core.TargetOperators

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let fssProject = "fss"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "F# server and db wrapper"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "Core library and basic plug-ins for the Amyris Genotype Specification Language (GSL) compiler. (Demetrix production release)"

// List of author names (for NuGet package)
let authors = [ "Darren Platt"; "Chris Macklin"]

// Tags for your project (for NuGet package)
let tags = "GSL amyris compiler demetrix"

// File system information
let solutionFile  = "Fss.sln"

// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "tests/**/bin/Release/netcoreapp2.0/*Tests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "Update GitHome in build.fsx"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

// The name of the project on GitHub
let gitName = "fss"

let gitRaw = Environment.environVarOrDefault "gitRaw" "https://raw.githubusercontent.com/Update GitHome in build.fsx"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

printfn $"Release version {release.NugetVersion}"
printfn $"AssemblyVersion: {release.AssemblyVersion}"
printfn $"Date: {release.Date}"
printfn $"SemVer: {release.SemVer}"

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)


// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override


let initTargets () =
    //BuildServer.install [
    //    GitHubActions.Installer
    //]
    /// Defines a dependency - y is dependent on x
    let (==>!) x y = x ==> y |> ignore
    /// Defines a soft dependency. x must run before y, if it is present, but y does not require x to be run.
    let (?=>!) x y = x ?=> y |> ignore

    Target.create "BuildPackage" ignore

    Target.create "All" ignore
    // Generate assembly info files with the right version & up-to-date information
    Target.create "AssemblyInfo" (fun _ ->
        let getAssemblyInfoAttributes projectName =
            [
                AssemblyInfo.Title (projectName)
                AssemblyInfo.Product fssProject
                AssemblyInfo.Description summary
                AssemblyInfo.Version release.AssemblyVersion
                AssemblyInfo.InformationalVersion (Git.Information.getCurrentHash())
                AssemblyInfo.FileVersion release.AssemblyVersion ]

        let getProjectDetails (projectPath : string) =
            let projectName = Path.GetFileNameWithoutExtension(projectPath)
            ( projectPath,
            projectName,
            Path.GetDirectoryName(projectPath),
            (getAssemblyInfoAttributes projectName)
            )

        !! "src/**/*.??proj"
        |> Seq.map getProjectDetails
        |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
            match projFileName with
            | Fsproj -> AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
            | Csproj -> AssemblyInfoFile.createCSharp ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
            | Vbproj -> AssemblyInfoFile.createVisualBasic ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
            | Shproj -> ()
            )
    )

    // Copies binaries from default VS location to expected bin folder
    // But keeps a subdirectory structure for each project in the
    // src folder to support multiple project outputs
    Target.create "CopyBinaries" (fun _ ->
        !! "src/**/*.??proj"
        -- "src/**/*.shproj"
        |>  Seq.map (fun f -> ((Path.GetDirectoryName f) </> "bin/Release", "bin" </> (Path.GetFileNameWithoutExtension f)))
        |>  Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
    )

    // --------------------------------------------------------------------------------------
    // Clean build results

    Target.create "Clean" (fun _ ->
        Shell.cleanDirs
            [   "bin"
                "temp"
                "src/*/bin"
                "tests/fss.Tests/bin" ]
    )

    // --------------------------------------------------------------------------------------
    // Build library & test project

    Target.create "Build" (fun _ ->
        !! "src/**/*.??proj"
        ++ "tests/**/*.??proj"
        |> Array.ofSeq
        |> Array.iter (fun project ->
            project
            |> DotNet.build (fun buildOptions ->
                
                { buildOptions with
                    MSBuildParams = { MSBuild.CliArguments.Create() with DisableInternalBinLog = true }                    
                    Configuration = DotNet.BuildConfiguration.Release })))


    // --------------------------------------------------------------------------------------
    // Run the unit tests using test runner

    Target.create "RunTests" (fun _ ->
        DotNet.test
            (fun parameters ->
                { parameters with Configuration = DotNet.BuildConfiguration.Release })
            "tests/Fss.Tests"
    )

    // --------------------------------------------------------------------------------------
    // Build a NuGet package

    Target.create "NuGet" (fun _ ->
        Paket.pack(fun p ->
            { p with
                ToolType = ToolType.CreateCLIToolReference()
                OutputPath = "bin"
                Version = release.NugetVersion
                MinimumFromLockFile = true
                ReleaseNotes = String.toLines release.Notes})
    )

    Target.create "PublishNuget" (fun _ ->
        Paket.push(fun p ->
            { p with
                ToolType = ToolType.CreateCLIToolReference()
                WorkingDir = "bin" })
    )

    // --------------------------------------------------------------------------------------
    // Release Scripts

    Target.create "Release" (fun _ ->
        let user =
            match Environment.environVarOrNone "github-user" with
            | Some(s) when not (String.IsNullOrWhiteSpace s) -> s
            | _ -> UserInput.getUserInput "Username: "
        let pw =
            match Environment.environVarOrNone "github-pw" with
            | Some(s) when not (String.IsNullOrWhiteSpace s) -> s
            | _ -> UserInput.getUserPassword "Password: "
        let remote =
            Git.CommandHelper.getGitResult "" "remote -v"
            |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
            |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
            |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

        Staging.stageAll ""
        Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
        Branches.pushBranch "" remote (Information.getBranchName "")

        Branches.tag "" release.NugetVersion
        Branches.pushTag "" remote release.NugetVersion
        // release on github
        GitHub.createClient user pw
        |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
        // TODO: |> uploadFile "PATH_TO_FILE"
        |> GitHub.publishDraft
        |> Async.RunSynchronously
    )


    "AssemblyInfo"
        ==> "Build"
        ==> "CopyBinaries"
        // ==> "RunTests"
        ==> "NuGet"
        ==> "BuildPackage"
        ==> "All"
    |> ignore

    "Clean"
        ==> "Release"
    |> ignore

    "BuildPackage"
        ==> "PublishNuget"
        ==> "Release"
    |> ignore

[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    initTargets()
    Target.runOrDefaultWithArguments "All"
    0
