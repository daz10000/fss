namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("fss_sqlite")>]
[<assembly: AssemblyProductAttribute("fss")>]
[<assembly: AssemblyDescriptionAttribute("F# Server")>]
[<assembly: AssemblyVersionAttribute("1.5.2")>]
[<assembly: AssemblyFileVersionAttribute("1.5.2")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.5.2"
    let [<Literal>] InformationalVersion = "1.5.2"
