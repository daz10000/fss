﻿namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("fss")>]
[<assembly: AssemblyProductAttribute("fss")>]
[<assembly: AssemblyDescriptionAttribute("F# Server")>]
[<assembly: AssemblyVersionAttribute("1.5.1")>]
[<assembly: AssemblyFileVersionAttribute("1.5.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.5.1"
    let [<Literal>] InformationalVersion = "1.5.1"
