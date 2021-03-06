﻿module TestTemplate

open NUnit.Framework
open Fss.Template

let test10 = "
preamble
 {% for x in var1 %}
        line: {{ x }}
    {% endfor %}
postamble
"
let test10Result = "
preamble
 
        line: cat
    
        line: dog
    
        line: mouse
    
        line: kangaroo
    
postamble
"

let flat(s:string) = s.Replace("\n","\\n").Replace("\r","\\r").Replace("\t","\\t")
/// Expected Actual comparison
let sc (s1:string) (s2:string) =
    if s1<>s2 then
        Assert.Fail(sprintf "String mismatch\nExpected>>>>>%s<<<<<<\nActual  >>>>>%s<<<<<<" (flat s1) (flat s2))

[<TestFixture>]
type Basic() = class     
//    let test1 = "
//        some text some {% for a in b %}
//        {{ var1 }}
//        some following text
//        {% endfor %}
//        "

    let test9 = "
    {% if var2+5>6 %}
    var2+5 > 6
    {% endif %}

     {% if (var2*2)>(var2*3)-2 %}
    (var2*2)>(var2*3)-2
    {% endif %}
    "
    let test9Result = "
    
    var2+5 > 6
    

     
    "

    do
        ()

   
    [<Test>]
    member __.Test005aNestedFors() =
        let t = Template("{% for x in var1 %}{% for y in var2%}{{y}}{% endfor %}{% endfor %}")
        let page = t.Render( [| ("var1" , box [| 1 ;2 ; 3|]) ; ("var2",box [| 4 ; 5 ; 6 |])  |])
        sc "456456456" page

    [<Test>]
    member __.Test005bNestedForsMultiInner() =
        // This version of nested fors has a space in it making two blocks
        // within the inner for loop and believe it or not, harder than the version
        // above.
        let t = Template("{% for x in var1 %}{% for y in var2%}{{y}}{% endfor %} {% endfor %}")
        let page = t.Render( [| ("var1" , box [| 1 ;2 ; 3|]) ; ("var2",box [| 4 ; 5 ; 6 |])  |])
        sc "456 456 456 " page

    [<Test>]
    member __.Test005cNestedForsMixedInner() =
        // Mixed blocks inside a for loop
        let t = Template("{% for x in var1 %}{{x}}{% for y in var2%}{{y}}{% endfor %} {% endfor %}")
        let page = t.Render( [| ("var1" , box [| 1 ;2 ; 3|]) ; ("var2",box [| 4 ; 5 ; 6 |])  |])
        sc "1456 2456 3456 " page

    [<Test>]
    member __.Test005dNestedForsComplex1() =
        // Mixed blocks inside a for loop
        let t = Template("preamble{% for x in var1 %}{{x}}{% for y in var2%}{{y}}{% endfor %} {% endfor %}postamble")
        let page = t.Render( [| ("var1" , box [| 1 ;2 ; 3|]) ; ("var2",box [| 4 ; 5 ; 6 |])  |])
        sc "preamble1456 2456 3456 postamble" page

    [<Test>]
    member __.Test005dNestedForsComplex2() =
        // Mixed blocks inside a for loop
        let t = Template("preamble{{hello}}{% for x in var1 %}{{x}}middle{% for y in var2%}{{y}}{% endfor %} {% endfor %}postamble")
        let page = t.Render( [| ("var1" , box [| 1 ;2 ; 3|]) ; ("var2",box [| 4 ; 5 ; 6 |]) ; ("hello",box "there") |])
        sc "preamblethere1middle456 2middle456 3middle456 postamble" page
    [<Test>]
    member __.Test005bEmptyFor() =
        // No content in for block - make sure we can parse this case
        let _ = Template("{% for x in var1 %}{% endfor %}")
        ()

    [<Test>]
    member __.Test006aNestedIfs() =
        // If block inside if block
        let t = Template("{%if x==0%} x is zero {%if y==0 %}y is zero too{%endif%}{%endif%}")
        let page = t.Render( [| ("x" , box 0) ; ("y",box 0) ; ("hello",box "there") |])
        sc " x is zero y is zero too" page

    [<Test>]
    member __.Test006bNestedIfs() =
        // If block inside if block
        let t = Template("{%if x==0%} x is zero {{hello}}{%if y==0 %}y is zero {{hello}} too{%endif%}{%endif%}")
        let page = t.Render( [| ("x" , box 0) ; ("y",box 0) ; ("hello",box "there") |])
        sc " x is zero therey is zero there too" page

    [<Test>]
    member __.Test006cEmptyIfBlock() =
        let _ = Template("{%if x==0%}{%endif%}")
        ()

    [<Test>]
    member __.Test006dEmptyElseBlock() =
        let _ = Template("{%if x==0%}{%else%}{%endif%}")
        ()

    [<Test>]
    member __.Test006eIfVarIsSet() =
        let t = Template("{%if x%}Yes{%endif%}")
        let page = t.Render( [| ("x", box t) |])
        sc "Yes" page

    [<Test>]
    member __.Test006ifEmptyArrayShouldBeFalse() =
        let t = Template("{% if x %}Yes{% else %}No{% endif %}")
        let page1 = t.Render( [| ("x", box [|1 ; 2; 3|]) |])
        sc "Yes" page1
        let page = t.Render( [| ("x", box [||]) |])
        sc "No" page 


    (*
    // Not ready for this - {{expressions}} not implemented
    [<Test>]
    member x.Test006cNestedIfsAndFors() =
        // If block inside for block block
        let t = Template("{% for y in var1 %}{% for x in var2%}
                            {%if x+y=5 %}{{x}}+{{y}}={{x+y}}{%endif%}
                            {%endfor%}{%endfor%}")
        let page = t.Render( [| ("var1" , box [| 1 ;2 ; 3 ; 4 ; 5|]) ; ("var2",box [| 1 ; 2 ; 3 ; 4 ; 5 |]) ; ("hello",box "there") |])
        sc " x is zero y is zero too" page

        *)


    [<Test>]
    member __.Test009() =
        let template1 = Template(test9)
        let page = template1.Render( [| ("var1",box "happy birthday") ; ("var2", box 2)|])
        sc page test9Result

    [<Test>]
    member __.Test000VarFetcher() =
        let vf = VarExtractor([| ("cat" , "prudence") ; ("dog" , "snoopy" ) |])
        sc (sprintf "'%A'" (vf.Get("cat")) ) "'SCONST \"prudence\"'"
        sc ( sprintf "'%A'" (vf.Get("dog")) )  "'SCONST \"snoopy\"'"
        sc ( sprintf "'%A'" (vf.Get("mouse")) ) "'SCONST \"missing value 'mouse'\"'"

    [<Test>]
    member __.Test010() =
        let template1 = Template(test10)
        let page = template1.Render( [| ("var1",box [| "cat" ; "dog" ; "mouse" ;"kangaroo"|]) ; ("var2", box 2)|])
        sc page test10Result

    [<Test>]
    member __.Test011aSingleCurlysOpening() =
        /// Ensure that single curly braces are handled gracefully
        Template("foo bar innocent { single curly") |> ignore
        ()     
    
    [<Test>]
    member __.Test011bSingleCurlysClosing() =
        /// Ensure that single curly braces are handled gracefully
        Template("foo bar innocent single curly } ") |> ignore
        ()     

    [<Test>]
    member __.Test011cSingleCurlysDouble() =
        /// Ensure that single curly braces are handled gracefully
        Template(" double {} together") |> ignore
        ()     

    [<Test>]
    member __.Test011dSingleCurlysComplex() =
        /// Ensure that single curly braces are handled gracefully
        Template("<!DOCTYPE html>
        <html lang=\"en\">
          <head>
            <meta charset=\"utf-8\">
            <meta name=\"generator\" content=\"CoffeeCup HTML Editor (www.coffeecup.com)\">
            <meta name=\"description\" content=\"\">
            <meta name=\"keywords\" content=\"\">
            <title>Cloud Analysis</title>
    
            <style type=\"text/css\">
            <!--
            body {
              color:#000000;
              background-color:#E0E0E0;
            }
            a  { color:#0000FF; }
            a:visited { color:#800080; }
            a:hover { color:#008000; }
            a:active { color:#FF0000; }
	        h1 { font:25px Calibri,sans-serif; }
	        h2 { font:20px Calibri,sans-serif; }
            th { font:15px Calibri,sans-serif; }
            -->
            </style>
            <!--[if IE]>
            <script src=\"http://html5shim.googlecode.com/svn/trunk/html5.js\"></script>
            <![endif]-->
          </head>
          <body> ") |> ignore
        ()
    [<Test>]
    member __.Test020IncludeSimple() =
        Template("{% include \"layout.html\" %} Mary had a little lamb") |> ignore
        // How to test this?  Will depend on a local folder at some point for the imports
        ()

    [<Test>]
    member __.Test021IncludeMultiple() =
        /// Proc (web) filesystem to expose status info
        let grab page = 
            match page with 
                | "mary.html" -> "mary had a little lamb"
                | "fleece.html" -> "its fleece was white as snow"
                | _ -> "file not found"
        let template = Template("{% include \"mary.html\" %} {% include \"fleece.html\" %}",grab)
        let templateExpected = "mary had a little lamb its fleece was white as snow"
        let page = template.Render([||])
        sc templateExpected page    

    [<Test>]
    member __.Test022IncludeRecursive() =
        /// Proc (web) filesystem to expose status info
        let grab page = 
            match page with 
                | "mary.html" -> "mary had a little {% include \"lamb.html\"%}"
                | "fleece.html" -> "its fleece was white as snow"
                | "lamb.html" -> "lamb"
                | _ -> "file not found"
        let template = Template("{% include \"mary.html\" %} {% include \"fleece.html\" %}",grab)
        let templateExpected = "mary had a little lamb its fleece was white as snow"
        let page = template.Render([||])
        sc templateExpected page    

        ()

    [<Test>]
    member __.Test023IncludeRecursiveVarSub() =
        /// Proc (web) filesystem to expose status info
        let grab page = 
            match page with 
                | "mary.html" -> "mary had a little {% include \"lamb.html\"%}"
                | "fleece.html" -> "its fleece was white as snow"
                | "lamb.html" -> "lamb name={{lambName}}"
                | _ -> "file not found"
        let template = Template("{% include \"mary.html\" %} {% include \"fleece.html\" %}",grab)
        let templateExpected = "mary had a little lamb name=lambchop its fleece was white as snow"
        let page = template.Render([| ("lambName",box "lambchop")|])
        sc templateExpected page    

        ()

    [<Test>]
    member __.Test024ExtendsIncludeCombo() =
        /// Proc (web) filesystem to expose status info
        let grab page = 
            match page with 
                | "mary.html" -> "{%block person%}{%endblock%} had a little {%block animal%}{%endblock%} its fleece was white as snow"
                | "lamb.html" -> "lamb name=lambchop"
                | _ -> "file not found"
        let template = Template("{%extends \"mary.html\" %}{%block person%}Mary{%endblock%}{%block animal%}{%include filename=\"lamb.html\"%}{%endblock animal%}",grab)
        let templateExpected = "Mary had a little lamb name=lambchop its fleece was white as snow"
        let page = template.Render([| ("lambName",box "lambchop")|])
        sc templateExpected page    

        ()

    [<Test>]
    member __.Test025ExtendsIncludeVarInIncludeCombo() =
         /// Proc (web) filesystem to expose status info
        let grab page = 
            match page with 
                | "mary.html" -> "{%block person%}{%endblock%} had a little {%block animal%}{%endblock%} its fleece was white as snow"
                | "lamb.html" -> "lamb name={{lambName}}"
                | _ -> "file not found"
        let template = Template("{%extends \"mary.html\" %}{%block person%}Mary{%endblock%}{%block animal%}{%include filename=\"lamb.html\"%}{%endblock animal%}",grab)
        let templateExpected = "Mary had a little lamb name=lambchop its fleece was white as snow"
        let page = template.Render([| ("lambName",box "lambchop")|])
        sc templateExpected page    

        ()

    [<Test>]
    /// Recognize block statements correctly
    member __.Test040Block() =
        let template = Template("{%block foo%} La de da da {%endblock%}",fun _ -> "")
        let templateExpected = " La de da da "
        let page = template.Render([||])
        sc templateExpected page    

    [<Test>]
    /// Recognize block statements correctly with named block close
    member __.Test041NamedEndBlock() =
        let template = Template("{%block foo%} La de da da {%endblock foo%}",fun _ -> "")
        let templateExpected = " La de da da "
        let page = template.Render([||])
        sc templateExpected page   
         
    /// Recognize block statements correctly with funny whitespace
    member __.Test042BlockWS() =
        let template = Template("{% block foo %} La de da da {% endblock %}",fun _ -> "")
        let templateExpected = " La de da da "
        let page = template.Render([||])
        sc templateExpected page    

    [<Test>]
    member __.Test043BlocksEmpty() =
        Template("{%block thing%}{%endblock%}") |> ignore
        () // That should exercise the parser

    [<Test>]
    /// Recognize block statements correctly with named block close with funny whitespace
    member __.Test044NamedEndBlockWS() =
        let template = Template("{%  block foo  %} La de da da {% endblock foo  %}",fun _ -> "")
        let templateExpected = " La de da da "
        let page = template.Render([||])
        sc templateExpected page    

    [<Test>]
    member __.Test045BlocksTextInterspersed1() =
        Template("mary had a little {%block animal%} insert animal here {%endblock%}") |> ignore
        () // That should exercise the parser

    [<Test>]
    member __.Test046BlocksTextInterspersed() =
        Template("mary had a little {%block animal%} insert animal here {%endblock%} its {%block animalpart %}fleece{%endblock%} was {%block color%}white{%endblock%}") |> ignore
        () // That should exercise the parser

    [<Test>]
    member __.Test047BlocksTextInterspersed() =
        Template("mary had a little {%block animal%} insert animal here {%endblock%} its {%block animalpart %}fleece{%endblock%} was {%block color%}white{%endblock%} as {%block thing%}{%endblock%}") |> ignore
        () // That should exercise the parser
    
    [<Test>]
    /// Recognize block statements correctly with named block close
    member __.Test050BasicExtends() =
        let grab page = 
            match page with 
                | "base.html" -> "mary had a little {%block animal%} insert animal here {%endblock%} its {%block animalpart %}animal part{%endblock%} was {%block color%}a color{%endblock%} as {%block thing%}{%endblock%}"
                | "lamb.html" -> ""
                | _ -> "file not found"
        let template1 = Template("{%extends \"base.html\"%}
                                        {%block animal%}lamb{%endblock%}
                                        {%block animalpart%}fleece{%endblock animalpart%}
                                        {%block color%}white{%endblock color%}
                                        {%block thing%}snow{%endblock%}
                                        ",grab)
        let templateExpected1 = "mary had a little lamb its fleece was white as snow"
        let page1 = template1.Render([||])
        sc templateExpected1 page1
        
        let template2 = Template("{%extends \"base.html\"%}
                                        {%block animal%}cow{%endblock%}
                                        {%block animalpart%}milk{%endblock animalpart%}
                                        {%block color%}brown{%endblock color%}
                                        {%block thing%}chocolate{%endblock%}
                                        ",grab)    
        let templateExpected2 = "mary had a little cow its milk was brown as chocolate"
        let page2 = template2.Render([||])
        sc templateExpected2 page2

    [<Test>]
    member __.Test051ExtendInclude() =
        /// Proc (web) filesystem to expose status info
        let grab page =
            match page with
                | "base.html" -> """in base {{rid}}.{% block content %}{% endblock %}"""
                | "navbar.html" -> "in navbar {{rid}}."
                | _ -> "file not found"
        let template = Template("""{% extends "base.html" %}{% block content %}{% include "navbar.html" %}in index {{rid}}.{% endblock %}""",grab)
        let templateExpected = """in base 55.in navbar 55.in index 55."""
        let page = template.Render([| ("rid", box 55)|])
        sc templateExpected page   
 
        ()
          
end

// Test variable substitution

type Person = { name : string ; zip : int ; age : float }
let bh90210 = [| { name = "Brenda" ; age = 22.2 ; zip = 90210 } ; { name = "Dylan" ; age = 23.5 ; zip = 90210 } ; { name = "Kelly" ; age = 22.3 ; zip = 90210 } |]

type Test12Type = {id: int; str: string}
let test1 = "
        some text some {% for a in b %}
        {{ var1 }}
        some following text
        {% endfor %}
        "
let test1Result ="
        some text some 
        linecontents
        some following text
        
        linecontents
        some following text
        
        linecontents
        some following text
        
        linecontents
        some following text
        
        "


[<TestFixture>]
type Vars() = class     
 [<Test>]
    member __.Test001() =
        let t = Template(test1)
        let page = t.Render( [| ("b",box [| "cat" ; "dog" ; "mouse" ;"kangaroo"|] ); ("var1" , box "linecontents")|] )
        sc test1Result page

    [<Test>]
    member __.Test002a() = 
        // Inline variable expansion outside a block
        let t = Template(" Simple inline variable var1={{var1}} . ")
        let page = t.Render( [| ("var1" , box 99)|] )
        sc " Simple inline variable var1=99 . " page
    [<Test>]
    member __.Test002b() = 
        let t = Template(" Simple inline terminal variable var1={{var1}}")
        let page = t.Render( [| ("var1" , box 99)|] )
        sc " Simple inline terminal variable var1=99" page
    [<Test>]
    member __.Test002c() = 
        let t = Template("{{var1}} simple leading variable")
        let page = t.Render( [| ("var1" , box 99)|] )
        sc "99 simple leading variable" page

    [<Test>]
    member __.Test002d() = 
        // leading variable inside a for block
        let t = Template("preamble {% for x in var1 %}{{x}} leading var{% endfor %} postamble")
        sc "preamble 1 leading var2 leading var3 leading var postamble" (t.Render( [| ("var1" , box [| 1 ;2 ; 3|])|] ))
    
    [<Test>]
    member __.Test002e() = 
        // only variable inside a for block
        let t = Template("preamble {% for x in var1 %}{{x}}{% endfor %} postamble")
        let page = t.Render( [| ("var1" , box [| 1 ;2 ; 3|])|] )
        sc "preamble 123 postamble" page

    [<Test>]
    member __.Test002f() =
        // trailing variable inside a for block
        let t = Template("preamble {% for x in var1 %} trailing {{x}}{% endfor %} postamble")
        let page = t.Render( [| ("var1" , box [| 1 ;2 ; 3|])|] )
        sc "preamble  trailing 1 trailing 2 trailing 3 postamble" page

    [<Test>]
    member __.Test002g() = 
        let test002gTemplate = "<br/>{{var2}}<br/>{{var1}} foobar {% for run in rundata %}{{run}}{% endfor %}"
        let rundata = [| "mary" ; "had" ; "lamb" |]
        let t = Template(test002gTemplate)
        let page = t.Render( [| ("rundata" , box rundata) ; ("var1",box "party") ; ("var2",box "discoball")|])
        sc "<br/>discoball<br/>party foobar maryhadlamb" page

    [<Test>]
    member __.Test003a() = 
        let t = Template("{% for x in var1 %}{{x}} leading var{% endfor %}")
        let page = t.Render( [| ("var1" , box [| 1 ;2 ; 3|])|] )
        sc "1 leading var2 leading var3 leading var" page

    [<Test>]
    member __.Test003b() = 
        let t = Template("preamble {% for x in var1 %}{{x}} leading var{% endfor %}")
        let page = t.Render( [| ("var1" , box [| 1 ;2 ; 3|])|] )
        sc "preamble 1 leading var2 leading var3 leading var" page


    [<Test>]
    /// Simple class passed to the template renderer, class name used to look up fields
    member __.Test004aClass() =
        let t = Template("{{Person.name}}")
        sc "Brenda" ( t.Render({ name = "Brenda" ; age = 28.2 ; zip = 90210 }))

    [<Test>]
    /// Simple class passed as an array element
    member __.Test004bClass() =
        let t = Template("{{person.name}}")
        sc "Brenda" ( t.Render( [| ("person" , { name = "Brenda" ; age = 22.2 ; zip = 90210 }) |] ) )

    [<Test>]
    /// Iterate over an array of classes
    member __.Test004cClass() =
        //let bh90210 = [| { name = "Brenda" ; age = 22.2 ; zip = 90210 } ; { name = "Dylan" ; age = 23.5 ; zip = 90210 } ; { name = "Kelly" ; age = 22.3 ; zip = 90210 } |]
        let t = Template("{% for x in bh90210cast %}{{x.name}} {{x.age}} {{x.zip}}\n{% endfor %}")
        sc "Brenda 22.200000 90210\nDylan 23.500000 90210\nKelly 22.300000 90210\n" ( t.Render( [| ("bh90210cast" , bh90210) |] ) )

    [<Test>]
    /// Expressions inside {{ }}
    member __.Test010Expression1() =
        let t = Template("{{x*y}}")
        sc "54" (t.Render([| ("x",box 6) ; ("y", box 9) |]))

    [<Test>]
    /// Expressions inside {{ }}
    member __.Test010Expression2() =
        let t = Template("{{10*y}}")
        sc "90" (t.Render([| ("x",box 6) ; ("y", box 9) |]))

    [<Test>]
    /// Expressions with array indices inside {{ }}
    member __.Test020ExpressionArray() =
        let t = Template("{{x[4]}}")
        sc "40" (t.Render([| ("x",box [|0;10;20;30;40;50;60|]) ; ("y", box 9) |]))

    [<Test>]
    /// Expressions with array indices inside 2D {{ }}
    member __.Test021ExpressionArray2D() =
        let t = Template("{{x[2][4]}}")
        sc "15" (t.Render([| ("x",
                                box [|
                                    [|1;2;3;4;5;6|];
                                    [|2;4;6;8;10;12|];
                                    [|3;6;9;12;15;18|];
                                    [|4;8;12;16;20;24|]
                                |]
                                        
                              ) |]))

    [<Test>]
    /// Expressions with array indices inside 2D {{ }}
    member __.Test022Array2DIndexExpressions() =
        let t = Template("{{a[y-1][x-1]}}")
        sc "6" (t.Render([| ("a",
                                box [|
                                    [|1;2;3;4;5;6|];
                                    [|2;4;6;8;10;12|];
                                    [|3;6;9;12;15;18|];
                                    [|4;8;12;16;20;24|]
                                |]
                                        
                              ) ; ("x",box 2) ; ("y",box 3) |]))

    [<Test>]
    /// array index dot something {{ }}
    member __.Test023ArrayDot() =
        let t = Template("{{x[i].age}}")
        sc "22.300000" (t.Render([| ("x",box bh90210) ; ("i",box 2) |]))

end

type Test019 = { url : string}

[<TestFixture>]
type Boolean() = class    
    [<Test>]
    member __.Test014cIfBoolVar() =
        let template = Template("{% if x%}hello{%endif%}")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box true) |])
        sc templateExpected page
    [<Test>]
    member __.Test014dIfBoolExpr() =
        let template = Template("{% if not x%}hello{%endif%}")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box false) |])
        sc templateExpected page
 
    [<Test>]
    member __.Test016aOneEqOne() =
        let template = Template("""{% if 1==1%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test016bOneEqOneAnd() =
        let template = Template("""{% if 1==1 and 1==1%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test016bCOneEqOneoR() =
        let template = Template("""{% if 1==1 or 1==1%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test017BoolPrecedence1() =
        let template = Template("""{% if f and t or f %}true{%else%}false{%endif%}""")
        let templateExpected = "false"
        let page = template.Render([| ("f" , box false); ( "t",box true) |])
        sc templateExpected page

    [<Test>]
    member __.Test017BoolPrecedence2() =
        let template = Template("""{% if t or f and t or f %}true{%else%}false{%endif%}""")
        let templateExpected = "true"
        let page = template.Render([| ("f" , box false);( "t",box true) |])
        sc templateExpected page

    [<Test>]
    member __.Test017BoolVar() =
        let template = Template("""{% if t %}true{%else%}false{%endif%}""")
        let templateExpected = "true"
        let page = template.Render([| ("f" , box false) ; ( "t",box true) |])
        sc templateExpected page


    [<Test>]
    member __.Test018ExprPlus1() =
        let template = Template("""{% if 1+1==2 %}true{%else%}false{%endif%}""")
        let templateExpected = "true"
        let page = template.Render([| |])
        sc templateExpected page
    
    [<Test>]
    member __.Test018ExprPlus2() =
        let template = Template("""{% if 1+1==2+2 %}true{%else%}false{%endif%}""")
        let templateExpected = "false"
        let page = template.Render([| |])
        sc templateExpected page

    [<Test>]
    member __.Test018ExprPlusTimes1() =
        let template = Template("""{% if 2*3+1==7 %}true{%else%}false{%endif%}""")
        let templateExpected = "true"
        let page = template.Render([| |])
        sc templateExpected page

    [<Test>]
    member __.Test018ExprPlusTimes2() =
        let template = Template("""{% if 1+3*2==7 %}true{%else%}false{%endif%}""")
        let templateExpected = "true"
        let page = template.Render([| |])
        sc templateExpected page

    [<Test>]
    member __.Test018ExprPlusTimesVar() =
        let template = Template("""{% if 1+x*2==7 %}true{%else%}false{%endif%}""")
        let templateExpected = "true"
        let page = template.Render([| "x",box 3|])
        sc templateExpected page

    [<Test>]
    member __.Test019IfDot() =
        let f = { url = "http"}
        let template = Template("""{% if f.url=='' %}yes {% else %}no{%endif%}""")
        sc "no" (template.Render([| "f",box f|]))

    [<Test>]
    member __.Test015fIfGT1() =
        let template = Template("""{% if 'zebra'>'aardvark'%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test015fIfGT2() =
        let template = Template("""{% if 'aardvark'>'zebra'%}hello{%endif%}""")
        let templateExpected = ""
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test015fIfGTE1() =
        let template = Template("""{% if 'zebra'>='zebra'%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test015fIfGTE2() =
        let template = Template("""{% if 1>=0%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page
    [<Test>]
    member __.Test015fIfGTE3() =
        let template = Template("""{% if 1>=2%}hello{%endif%}""")
        let templateExpected = ""
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page
   
    [<Test>]
    member __.Test015gIfGTFalse() =
        let template = Template("""{% if 'aardvark'>'zebra'%}hello{%endif%}""")
        let templateExpected = ""
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page
    
    [<Test>]
    member __.Test015gIfGTWS() =
        let template = Template("""{% if 'zebra' > 'aardvark'%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test015fIfLT1() =
        let template = Template("""{% if 'zebra'<'aardvark'%}hello{%endif%}""")
        let templateExpected = ""
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test015fIfLT2() =
        let template = Template("""{% if 'aardvark'<'zebra'%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test015fIfLTE1() =
        let template = Template("""{% if 'zebra'<='zebra'%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test015fIfLTE2() =
        let template = Template("""{% if 0<=1%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test015fIfLTE3() =
        let template = Template("""{% if 9<=1%}hello{%endif%}""")
        let templateExpected = ""
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page
end


[<TestFixture>]
type ElseIf() = class  
    [<Test>]
    member __.Test016ElseIf2() =
        let template = Template("""{% if 9<=1%}1{%elseif 2>1%}3{%endif%}""")
        let templateExpected = "3"
        let page = template.Render([| |])
        sc templateExpected page
    [<Test>]
    member __.Test017ElseIfNone() =
        let template = Template("""{% if 9<=1%}1{%elseif 2>3%}3{%endif%}""")
        let templateExpected = ""
        let page = template.Render([| |])
        sc templateExpected page
    [<Test>]
    member __.Test017ElseIf3() =
        let template = Template("""{% if 9<=1%}1{%elseif 2>1%}3{%endif%}""")
        let templateExpected = "3"
        let page = template.Render([| |])
        sc templateExpected page
    [<Test>]
    member __.Test018ElseIf1() =
        let template = Template("""{% if 9<=11%}1{%elseif 2>1%}3{%endif%}""")
        let templateExpected = "1"
        let page = template.Render([| |])
        sc templateExpected page

    [<Test>]
    member __.Test019ElseIf4() =
        let template = Template("""{% if 9<=1%}1{%elseif 2>4%}3{%elseif 2>1%}4{%else%}5{%endif%}""")
        let templateExpected = "4"
        let page = template.Render([| |])
        sc templateExpected page

    [<Test>]
    member __.Test020ElseIf5() =
        let template = Template("""{% if 9<=1%}1{%elseif 2>4%}3{%elseif 2>5%}4{%else%}5{%endif%}""")
        let templateExpected = "5"
        let page = template.Render([| |])
        sc templateExpected page

    [<Test>]
    member __.Test020ElseIf5bWS() =
        let template = Template("""{% if 9 <= 1%}1{%elseif 2 > 4%}3{%elseif 2 > 5 %}4{%else%}5{%endif%}""")
        let templateExpected = "5"
        let page = template.Render([| |])
        sc templateExpected page


    [<Test>]
    member __.Test021ElseIfEmpty0() =
        let template = Template("""{% if 9<=1%}{%elseif 4>2%}3{%endif%}""")
        let templateExpected = "3"
        let page = template.Render([| |])
        sc templateExpected page
    [<Test>]
    member __.Test021ElseIfEmpty1() =
        let template = Template("""{% if 9<=1%}{%elseif 2>4%}3{%elseif 2>5%}4{%else%}5{%endif%}""")
        let templateExpected = "5"
        let page = template.Render([| |])
        sc templateExpected page
    [<Test>]
    member __.Test021ElseIfEmpty2() =
        let template = Template("""{% if 9<=1%}{%elseif 2>4%}3{%elseif 2>5%}{%else%}5{%endif%}""")
        let templateExpected = "5"
        let page = template.Render([| |])
        sc templateExpected page
    [<Test>]
    member __.Test021ElseIfEmpty3() =
        let template = Template("""{% if 9<=1%}{%elseif 2>4%}3{%elseif 2>5%}{%else%}{%endif%}""")
        let templateExpected = ""
        let page = template.Render([| |])
        sc templateExpected page
    [<Test>]
    member __.Test021ElseIfEmptyAll() =
        let template = Template("""{% if 9<=1%}{%elseif 2>4%}{%elseif 2>5%}{%else%}{%endif%}""")
        let templateExpected = ""
        let page = template.Render([| |])
        sc templateExpected page
    [<Test>]
    member __.Test022Nested1() =
        let template = Template("""{% if 9<=1%}{%elseif 2>1%}{%if 1==1%}1{%else%}0{%endif%}{%elseif 2>5%}{%else%}{%endif%}""")
        let templateExpected = "1"
        let page = template.Render([| |])
        sc templateExpected page
    [<Test>]
    member __.Test022Nested2() =
        let template = Template("""{% if 9<=1%}{%elseif 2>1%}{%if 1==0%}1{%else%}0{%endif%}{%elseif 2>5%}{%else%}{%endif%}""")
        let templateExpected = "0"
        let page = template.Render([| |])
        sc templateExpected page

end

[<TestFixture>]
type TestParseError() = class
    [<Test>]
    member __.MissingEndFor1() =
        try
            let _ = Template("{% for x in range(1,10)%}whatever")
            ()
        with exc ->
            let expected = "ERROR: parse error, likely unbalanced elements in template, parsed\n for x in range(1,10)\nwhatever\n"
            sc expected exc.Message

    [<Test>]
    member __.IfNonSequiter1() =
        try
            let template = Template("{% if 1==1 %}{%endfor%}")
            ()
        with exn ->
            let expected = "ERROR: parsing tail of IF block, unparseable: Endfor\n"
            sc expected exn.Message
end

  
[<TestFixture>]
type TestIfParsing() = class   
    [<Test>]
    member __.Test012IfDot() =
        let template = Template("{% for x in var1 %}{{x.str}}{% if x.str=='cat' %}ok{% endif %}{% endfor %}")
        let test12Expected = "catokdog"
        let page = template.Render( [| ("var1",box [| {id= 1; str= "cat"} ; {id= 2; str= "dog"}|]) ; ("var2", box 2)|])
        sc test12Expected page    
        ()

    [<Test>]
    member __.Test013aIf() =
        let template = Template("{% if x==7 %}hello{%endif%}")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box 7) |])
        sc templateExpected page

    
    [<Test>]
    member __.Test013bIfNot() =
        let template = Template("{% if not x==6 %}hello{%endif%}")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box 7) |])
        sc templateExpected page

    [<Test>]
    member __.Test013c_IfNotParens() =
        let template = Template("{% if not (x==6) %}hello{%endif%}")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box 7) |])
        sc templateExpected page
    [<Test>]
    member __.Test014a_elseTrueBranch() =
        let template = Template("{% if x==6 %}x is 6{%else%}x is not 6{%endif%}")
        let templateExpected = "x is 6"
        let page = template.Render([| ("x",box 6) |])
        sc templateExpected page
    [<Test>]
    member __.Test014bElseFalseBranch() =
        let template = Template("{% if x==6 %}x is 6{%else%}x is not 6{%endif%}")
        let templateExpected = "x is not 6"
        let page = template.Render([| ("x",box 7) |])
        sc templateExpected page

    [<Test>]
    member __.Test015aIfStrExprTrue() =
        let template = Template("""{% if x=="hi"%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test015bIfStrExprFalse() =
        let template = Template("""{% if x=="hi"%}hello{%endif%}""")
        let templateExpected = ""
        let page = template.Render([| ("x",box "ho") |])
        sc templateExpected page

    [<Test>]
    member __.Test015cIfStrNETrue() =
        let template = Template("""{% if x!="hi"%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "ho") |])
        sc templateExpected page

    [<Test>]
    member __.Test015dIfStrNEFalse() =
        let template = Template("""{% if x!="hi"%}hello{%endif%}""")
        let templateExpected = ""
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test015eIfSingleQuotes() =
        let template = Template("""{% if x=='hi'%}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box "hi") |])
        sc templateExpected page

    [<Test>]
    member __.Test015hIfStrExprWithRecordTrue() =
        let template = Template("""{% if x.name=="Brenda" %}hello{%endif%}""")
        let templateExpected = "hello"
        let page = template.Render([| ("x",box { name = "Brenda" ; age = 28.2 ; zip = 90210 }) |])
        sc templateExpected page


    [<Test>]
    member __.Test019Null() =
        let template = Template("The value is {{x}}.")
        let templateExpected = "The value is none."
        let page = template.Render([| ("x",box null) |])
        sc templateExpected page    



end  

[<TestFixture>]
type Range() = class    
    [<Test>]
    member __.Test001RangeBasic() =
        let template = Template("{% for x in range(1,10)%}{{x}} {%endfor%}")
        let templateExpected = "1 2 3 4 5 6 7 8 9 10 "
        let page = template.Render([|  |])
        sc templateExpected page

    [<Test>]
    member __.Test002RangeBasicStep() =
        let template = Template("{% for x in range(1,2,9)%}{{x}} {%endfor%}")
        let templateExpected = "1 3 5 7 9 "
        let page = template.Render([|  |])
        sc templateExpected page


    [<Test>]
    member __.Test010RangeVar() =
        let template = Template("{% for x in range(1,n)%}{{x}} {%endfor%}")
        let templateExpected = "1 2 3 4 5 6 7 8 9 10 "
        let page = template.Render([| ("n",box 10) |])
        sc templateExpected page

    [<Test>]
    member __.Test099Complex() =
        let template = Template("{% for y in range(1,n)%}\n\
                                    {%for x in range(1,n)%}{{x*y}} {%endfor%}\
                                 {%endfor%}")
        let templateExpected = "\n\
        1 2 3 4 \n\
                                2 4 6 8 \n\
                                3 6 9 12 \n\
                                4 8 12 16 "
        let page = template.Render([| ("n",box 4) |])
        sc templateExpected page
end