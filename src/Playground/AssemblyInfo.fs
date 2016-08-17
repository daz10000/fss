namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Playground")>]
[<assembly: AssemblyProductAttribute("fss")>]
[<assembly: AssemblyDescriptionAttribute("F# Server")>]
[<assembly: AssemblyVersionAttribute("1.4.1")>]
[<assembly: AssemblyFileVersionAttribute("1.4.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.4.1"
    let [<Literal>] InformationalVersion = "1.4.1"
