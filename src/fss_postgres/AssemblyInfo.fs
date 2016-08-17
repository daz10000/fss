namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("fss_postgres")>]
[<assembly: AssemblyProductAttribute("fss")>]
[<assembly: AssemblyDescriptionAttribute("F# Server")>]
[<assembly: AssemblyVersionAttribute("1.5.0")>]
[<assembly: AssemblyFileVersionAttribute("1.5.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.5.0"
    let [<Literal>] InformationalVersion = "1.5.0"
