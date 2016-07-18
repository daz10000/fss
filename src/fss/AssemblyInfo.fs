namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("fss")>]
[<assembly: AssemblyProductAttribute("fss")>]
[<assembly: AssemblyDescriptionAttribute("F# Server")>]
[<assembly: AssemblyVersionAttribute("1.4.0")>]
[<assembly: AssemblyFileVersionAttribute("1.4.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.4.0"
    let [<Literal>] InformationalVersion = "1.4.0"
