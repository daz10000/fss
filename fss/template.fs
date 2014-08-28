namespace Fss

/// Templating library modeled loosely on jinga2
module Template =
    open System.Text
    open System
    open System.IO
    let extendsFileRE = RegularExpressions.Regex("\"([^\"]*)\"")
    let fillVarsInTemplate (vars:Map<string,string>) (t:string)  =
        vars |> Seq.fold (fun (t:string) kv -> t.Replace(sprintf "{{%s}}" kv.Key,kv.Value)) t


    let a2s (a:char array) = new string(a)
    let l2s (l:char list) = new string(l |> Array.ofList)

    /// Legal characters to start an indentifier
    let legalStart = seq { 
                            for c in 'a' .. 'z' do yield c
                            for c in 'A' .. 'Z' do yield c
                            yield '_'
                        } |> Set.ofSeq
    /// Legal characters in an indentifier
    let legalID = seq { 
                            for c in 'a' .. 'z' do yield c
                            for c in 'A' .. 'Z' do yield c
                            for c in '0' .. '9' do yield c
                            yield '.'
                            yield '_'
                        } |> Set.ofSeq


    /// Components of mathematical expressions
    type Expression =
            | VARIABLE of string
            | ADD of Expression*Expression
            | SUB of Expression*Expression
            | EQUALS of Expression*Expression
            | LESSTHAN of Expression*Expression
            | GREATERTHAN of Expression*Expression
            | MULT of Expression*Expression
            | MOD of Expression*Expression
            | NEGATE of Expression
            | NOT of Expression
            | DIVIDE of Expression*Expression
            | FCONST of float
            | ICONST of int
            | ICONST64 of int64
            | SCONST of string
            | ARRAYCONST of Expression array
            | BCONST of bool
            | CLASS of Map<string,Expression>

    /// Pretty print expression
    let rec ppExpr = function
            | ADD(e1,e2) -> sprintf "%s+%s" (ppExpr e1) (ppExpr e2)
            | SUB(e1,e2) -> sprintf "%s-%s" (ppExpr e1) (ppExpr e2)
            | MULT(e1,e2) -> sprintf "%s*%s" (ppExpr e1) (ppExpr e2)
            | MOD(e1,e2) -> sprintf "%s%%%s" (ppExpr e1) (ppExpr e2)
            | DIVIDE(e1,e2) -> sprintf "%s/%s" (ppExpr e1) (ppExpr e2)
            | EQUALS(e1,e2) -> sprintf "%s=%s" (ppExpr e1) (ppExpr e2)
            | GREATERTHAN(e1,e2) -> sprintf "%s>%s" (ppExpr e1) (ppExpr e2)
            | LESSTHAN(e1,e2) -> sprintf "%s<%s" (ppExpr e1) (ppExpr e2)
            | FCONST(f) -> sprintf "%f" f
            | ICONST(d) -> sprintf "%d" d
            | ICONST64(d) -> sprintf "%d" d
            | SCONST(s) -> s
            | NEGATE(e) -> sprintf "-%s" (ppExpr e)
            | NOT(e) -> sprintf "not %s" (ppExpr e)
            | VARIABLE(v) -> sprintf "%s" v
            | BCONST(b) -> sprintf "%s" (if b then "True" else "False")
            | ARRAYCONST(a) -> 
                let s = a |> Array.map (ppExpr)
                sprintf "[%s]" (String.Join(";",s))
            | CLASS(c) ->
                String.Join("",
                                    seq { yield "{"
                                          for kv in c do
                                            yield sprintf "%s:%s" kv.Key (ppExpr kv.Value)
                                          yield "}"
                                        }
                            )

    /// Pattern to match an expression, including equality
    let rec (|Comparison|_|) = function
        | 'n'::'o'::'t'::' '::Comparison(e,rem) -> Some(NOT(e),rem)
        | 'n'::'o'::'t'::' '::Factor(e,rem) -> Some(NOT(e),rem)
        | '('::Comparison(e,[')']) -> Some(e,[]) // parenthetic comparator  e.g. not (a=6)
        | Expr(e1,'='::Expr(e2,rem)) -> Some(EQUALS(e1,e2),rem)
        | Expr(e1,'<'::Expr(e2,rem)) -> Some(LESSTHAN(e1,e2),rem)
        | Expr(e1,'>'::Expr(e2,rem)) -> Some(GREATERTHAN(e1,e2),rem)
        | _ -> None
    and (|Expr|_|) = function
        | Factor(e1, t) ->
            let rec aux e1 = function
              | '+'::Factor(e2, t) -> aux (ADD(e1,e2)) t
              | '-'::Factor(e2, t) -> aux (SUB(e1,e2)) t
              | t -> Some(e1, t)
            aux e1 t
        | _ -> None
      and (|Factor|_|) = function
        | '-'::Factor(e, t) -> Some(NEGATE(e), t)
        | Atom(e1, '*'::Factor(e2, t)) -> Some(MULT(e1,e2), t)
        | Atom(e1, '/'::Factor(e2, t)) -> Some(DIVIDE(e1,e2), t)
        | Atom(e1, '%'::Factor(e2, t)) -> Some(MOD(e1,e2), t)
        | Atom(e, t) -> Some(e, t)
        | _ -> None
      and (|Atom|_|) = function
        | c::tl1 when '0'<=c && c<='9' ->
            let sb = StringBuilder().Append(c)
            let rec aux count = 
                        function
                            | c::t when '0'<=c && c<='9' -> 
                                        sb.Append(c) |> ignore
                                        aux count t
                            | '.'::t ->
                                    if count > 0 then failwithf "Illegal floating point number %s." (sb.ToString())
                                    else
                                        sb.Append('.') |> ignore
                                        aux 1 t
                            | t -> 
                                let s = sb.ToString()
                                Some(match count with 
                                            | 0 ->ICONST(int(s)) 
                                            | 1 -> FCONST(float s) 
                                            | _ -> failwithf "impossible"
                                    ,t
                                )
            aux 0 tl1
        | '\''::t -> // Single quoted string
            let sb = StringBuilder()
            let rec aux = function
                          | c::t when c <> '\'' -> sb.Append(c) |> ignore ; aux t
                          | '\''::t -> Some(SCONST(sb.ToString()),t)
                          | _ -> failwithf "Unexpected end of string constant"
            aux t
         | '"'::t -> // Double quoted string
            let sb = StringBuilder()
            let rec aux = function
                          | c::t when c <> '\'' -> sb.Append(c) |> ignore ; aux t
                          | '"'::t -> Some(SCONST(sb.ToString()),t)
                          | _ -> failwithf "Unexpected end of string constant"
            aux t
        | '('::Expr(e, ')'::t) -> Some(e, t)
        | c::tl when legalStart.Contains(c) ->
            let sb = StringBuilder().Append(c)
            let rec aux = function
                          | c::t when legalID.Contains(c) -> sb.Append(c) |> ignore ; aux t
                          | t -> Some(VARIABLE(sb.ToString()),t)
            aux tl
        | _ -> None

    /// Components of template language
    type TemplatePart =
            | VAR of string
            | LOGIC of string
            | ENDFOR
            | ELSE
            | ENDIF
            | IFSTART of Expression
            | FORSTART of string*Expression // For var name, iterable var name
            | TEXT of string
            | BLOCKSTART of string
            | ENDBLOCK of string option
            | INCLUDE of string
            | EXTENDS of string
            | BLOCK of string*TemplatePart list
            | EXTENDSBUNDLED of (TemplatePart list)*(TemplatePart list)
            | UNKNOWNLOGIC of string
            | RAW 
            | ENDRAW
            | FOR of string*Expression*TemplatePart list
            | IF of Expression*TemplatePart list*TemplatePart list option
            | EXPANDED of string

    /// Pretty print template part lists
    let rec pp (indent:string) (parts: TemplatePart list) =
        String.Join("",
            seq {
                for part in parts do
                    stdout.Write(indent)
                    match part with 
                        | TEXT(b) -> yield sprintf "text: %s\n" b
                        | VAR(v) -> yield sprintf "Var: %s\n" v
                        | LOGIC(v) -> yield sprintf "Logic: %s\n" v
                        | EXTENDS(file) -> yield sprintf "Extends: %s\n" file
                        | EXTENDSBUNDLED(l1,l2) -> 
                                    yield sprintf "ExtendsBunded:\n" 
                                    yield (pp (indent+"111>") l1)
                                    yield (pp (indent+"222>") l2)
                        | INCLUDE(file) -> yield sprintf "Include: %s\n" file
                        | FORSTART(fv,fi) -> yield sprintf "For: %s in %s\n" fv (ppExpr fi)
                        | ENDBLOCK(name) -> yield sprintf "Endblock %s\n" (match name with | Some(x) -> x | None -> "none")
                        | BLOCKSTART(name) -> yield sprintf "Startblock %s\n" name
                        | BLOCK(name,contents) ->
                            yield sprintf "BLOCK: %s" name
                            yield (pp (indent+"iii>") contents)
                        | ELSE -> yield sprintf "Else\n"
                        | ENDFOR -> yield sprintf "Endfor\n"
                        | IFSTART(s) -> yield sprintf "IfStart %s\n" (ppExpr s)
                        | ENDIF -> yield sprintf "Endif\n"
                        | RAW -> yield sprintf "Raw\n"
                        | ENDRAW -> yield  sprintf "EndRaw"
                        | UNKNOWNLOGIC(ul) -> yield sprintf "Unknownlogic: %s\n" ul
                        | FOR(fv,fi,contents) -> 
                            yield sprintf "FOR: %s in %s\n" (fv.Trim()) (ppExpr fi)
                            yield ( pp (indent+"fff>") contents )
                        | IF(f,contents,elseContent) -> 
                            yield (sprintf "IF: %s" (ppExpr f))
                            yield (pp (indent+"iii>") contents)
                            match elseContent with
                                | None -> ()
                                | Some(c) ->  yield (pp (indent+"eee>") c)
                        | EXPANDED(x) -> yield (sprintf "EXPANDED: %s" x)

                    })

    let rec (|ForIter|_|) = function
                | Atom(fv,remainder) ->
                    let rec aux = function
                                        | 'i'::'n'::' '::Atom(fi,remainder2) -> Some(fv,fi)
                                        | ' '::tl -> aux tl
                                        | _ as x -> failwithf "ERROR: unexpected near %s" (new String(Array.ofList x))
                    aux remainder
                | _ -> None

    and  (|LogicType|) = function
                            | "endfor" -> ENDFOR
                            | "endblock" -> ENDBLOCK(None) // unnamed endblock
                            | "raw" -> RAW
                            | "endraw" -> ENDRAW
                            | "endif" -> ENDIF
                            | "else" -> ELSE
                            | x when x.StartsWith("block") -> BLOCKSTART(x.[5..].Trim()) // startblock
                            | x when x.StartsWith("endblock") -> ENDBLOCK(Some(x.[8..].Trim())) // named endblock
                            | x when x.StartsWith("extends") -> 
                                        let file = x.Trim().[7..]
                                        let m = extendsFileRE.Match(file)
                                        if m.Success then
                                            EXTENDS(m.Groups.[1].Value)
                                        else
                                            failwithf "ERROR: extends filename '%s' doesn't match \"filename\" pattern" file
                            | x when x.StartsWith("include") -> 
                                        let file = x.Trim().[7..]
                                        let m = extendsFileRE.Match(file)
                                        if m.Success then
                                            INCLUDE(m.Groups.[1].Value)
                                        else
                                            failwithf "ERROR: include filename '%s' doesn't match \"filename\" pattern" file
                            | x when x.StartsWith("for") -> 
                                        match x.[4..].Trim() |> List.ofSeq with
                                            | ForIter((forVar,targetVar)) -> 
                                                    match forVar with
                                                        | VARIABLE(v) -> FORSTART(v,targetVar)
                                                        | _ as x -> failwithf "ERROR: for loops must take form for x in y where x is a variable"
                                            | _ as x -> failwithf "ERROR: bad for loop format %A" x
                            | x when x.StartsWith("if") -> 
                                        match (List.ofSeq x.[3..]) with
                                                    | Comparison(c,[]) -> IFSTART(c)
                                                    | Expr(c,[]) -> IFSTART(c) // boolean expression like if x
                                                    | _ as x -> failwithf "ERROR: parsing if expression, unparseable expression encountered %A" x
                                        //IFSTART(c)
                            | _ as x -> UNKNOWNLOGIC(x)


    
    /// A template consists of a page which is a series
    /// of blocks interspersed with either LOGIC regions or variable
    /// regions.  
    and (|Page|_|) = function
            // Just some piece of text
            | Block(b,[]) -> Some([TEXT(b)])            // Block of text at start of page
    
            | Block(b,Page(p)) -> Some(TEXT(b)::p)

            // Leading logic followed by a page
            | Logic(v:string,Page(p)) ->
                      let (LogicType l) = v.Trim()
                      Some(l::p)

            // Leading variable in a page
            | Var(v,Page(p)) -> Some(VAR(v)::p)

            // Just a variable
            | Var(v,[]) -> Some( [VAR(v)])
            //| Block(b,Var(v,[])) -> Some( [TEXT(b) ; VAR(v) ])

            // Just some logic
            | Logic(v:string,[]) ->
                let (LogicType l) = v.Trim()
                Some([l])


            // Empty page
            | [] -> Some([])
            | _ -> None
        and (|Logic|_|) = function
            | '{'::'%'::tl ->
                let rec aux res = function // Go till end of %} block
                                    | c::d::tl when c <> '%' || d <> '}' ->  aux (c::res) (d::tl)
                                    | '%'::'}'::tl -> Some(new string(List.rev res |> Array.ofList),tl)
                                    | _ -> failwithf "Unexpected EOF parsing {% %} logic"   
                aux [] tl
            | _ -> None
        and (|Var|_|) = function
            | '{'::'{'::tl ->
                let rec aux res = function
                                    | c::d::tl when c <> '}' || d <> '}' ->  aux (c::res) (d::tl)
                                    | '}'::'}'::tl -> 
                                        let name = new string(List.rev res |> Array.ofList)
                                        Some(name.Trim(),tl)
                                    | _ -> failwithf "Unexpected EOF parsing {{ }} variable started from %s" (l2s tl)
                aux [] tl
            | _ -> None
        /// Block of text leading up to a { delimiter
        and (|Block|_|) = function
            | c::t when c <> '{' ->
                let rec aux res = function 
                                    | [c;d] -> Some(new string(List.rev (d::c::res) |> Array.ofList),[])
                                    | c::d::t when c <> '{' || (d<>'{' && d<>'%') -> aux (c::res) (d::t)
                                    | t -> Some(new string(List.rev res |> Array.ofList),t)
                aux [c] t 
            | _ -> None


    /// Now go through the parsed parts and roll them up into
    /// larger semantic groups (like pairing start/end block elements.
    /// Takes a list of Template Elements and returns a consolidated list of possibly
    /// high level template elements.
    let rec (|ParsedSection|_|) = function
        | EXTENDS(e)::ParsedSections(body,[]) -> Some(EXTENDSBUNDLED(body,[]),[]) // pass through - standalone element
        | INCLUDE(e)::tl -> Some(INCLUDE(e),tl) // pass through - standalone element
        | TEXT(t)::tl -> Some(TEXT(t),tl) // pass through
        | FORSTART(fv,fi)::ParsedSections(body,ENDFOR::tl) -> Some(FOR(fv,fi,body),tl) // Gather up FOR/ENDFOR blocks
        | FORSTART(fv,fi)::ENDFOR::tl -> Some(FOR(fv,fi,[]),tl) // Gather up FOR/ENDFOR blocks with empty for body
        | BLOCKSTART(name)::ENDBLOCK(name2)::tl ->
            if name2.IsNone || name=name2.Value then
                 Some(BLOCK(name,[]),tl) // Gather up BLOCK/ENDBLOCK blocks
            else
                 failwithf "ERROR processing template BLOCK %s doesn't match endblock %s" name 
                        (match name2 with | Some(x)->x | None -> "none")
        | BLOCKSTART(name)::ParsedSections(body,ENDBLOCK(name2)::tl) ->
            if name2.IsNone || name=name2.Value then
                 Some(BLOCK(name,body),tl) // Gather up BLOCK/ENDBLOCK blocks
            else
                 failwithf "ERROR processing template BLOCK %s doesn't match endblock %s" name 
                        (match name2 with | Some(x)->x | None -> "none")
        | IFSTART(f)::ParsedSections(body,ENDIF::tl) -> Some(IF(f,body,None),tl) // Gather up IF/ENDIF blocks
        | IFSTART(f)::ENDIF::tl -> Some(IF(f,[],None),tl) // Gather up IF/ENDIF blocks empty body
        | IFSTART(f)::ParsedSections(body,ELSE::ParsedSections(body2,ENDIF::tl)) -> Some(IF(f,body,Some(body2)),tl) // Gather up IF/ENDIF blocks
        | IFSTART(f)::ELSE::ParsedSections(body2,ENDIF::tl) -> Some(IF(f,[],Some(body2)),tl) // Gather up IF/ENDIF blocks
        | IFSTART(f)::ELSE::ParsedSections(body2,ENDIF::tl) -> Some(IF(f,[],Some(body2)),tl) // Gather up IF/ENDIF blocks
        | IFSTART(f)::ELSE::ENDIF::tl -> Some(IF(f,[],Some([])),tl) // Gather up IF/ENDIF blocks
        | VAR(v)::tl -> Some(VAR(v),tl)
        | EXTENDSBUNDLED(subs,body):: tl -> Some(EXTENDSBUNDLED(subs,body),tl)
        | _  as x -> None
        
    and (|ParsedSections|_|) = function
                | ParsedSection(p,ParsedSections(p2,rem)) -> Some(p::p2,rem)
                | ParsedSection(p,rem) -> Some([p],rem)
                | ParsedSection(p,[]) -> Some([p],[])
                | _ -> None

    and (|Parsed|) = function
            | ParsedSections(p,[]) -> p
            | _ as p -> failwithf "ERROR: parse error, likely unbalanced elements in template, got to %A" p

    // -------------------------------------------------------------------------------------------------

    // Template front end
    open System.Reflection
    open Microsoft.FSharp.Reflection

    /// Take a parameter argument and build a map of
    /// name -> values
    type VarExtractor(args:obj) = class
        let verbose = false
        // Some basic type definitions
        let  stringType = "cat".GetType()
        let  intType = (1).GetType()
        let  boolType = (true).GetType()
        let  int64Type = (1L).GetType()
        let  floatType = (1.2).GetType()
        let  genericListType = typedefof<List<_>>
        let  genericArrayType = [||].GetType()

        let rec procAtom (o:obj) =
            try
                if o = null then
                    Some(SCONST("none"))
                else
                    let f2t = o.GetType() 
                    let v = 
                        if f2t = intType then ICONST(o :?> int)
                        elif f2t = boolType then BCONST(o :?> bool)
                        elif f2t = int64Type then ICONST64(o :?> int64)
                        elif f2t = floatType then FCONST(o :?> float)
                        elif f2t = stringType then SCONST(string o)
                        elif f2t.IsGenericType && f2t.GetGenericTypeDefinition() = genericListType then 
                            // Doesn't work, see http://stackoverflow.com/questions/2140079/how-to-cast-an-object-to-a-list-of-generic-type-in-f
                            let x = o :?> List<obj> |> Seq.ofList
                            let y = [| for i in x -> procAtom i |] |> Array.choose (id)
                            ARRAYCONST(y)
                        elif f2t.IsArray then 
                            let x : Array = o :?> Array
                            let y = [| for i in x -> procAtom i |] |> Array.choose (id)
                            ARRAYCONST(y)
                        elif f2t.IsClass then
                            CLASS(
                                    seq {
                                        for p in f2t.GetProperties()  do
                                                match procAtom (p.GetValue(o,null)) with
                                                    | Some(v) -> yield p.Name,v
                                                    | None -> ()
                                    } |> Map.ofSeq    
                            )
                        else SCONST(o.ToString()) // failwithf "ERROR: unknown type %A used in variable" o
                    Some(v)
            with _ as exn ->
                Some(SCONST(sprintf "%s\n%s" exn.Message exn.StackTrace))
        let rec procOne (o:obj) =
            if FSharpType.IsTuple(o.GetType()) then
                match FSharpValue.GetTupleFields(o) with
                    | [| f1 ; f2 |] when f1.GetType() = stringType ->
                            (*
                            let f2t = f2.GetType() // FIX FIX - doesn't handle list type
                            let v = 
                                if f2t = intType then ICONST(f2 :?> int)
                                elif f2t = floatType then FCONST(f2 :?> float)
                                elif f2t = stringType then SCONST(string f2)
                                elif f2t.IsGenericType && f2t.GetGenericTypeDefinition() = genericListType then 
                                    let x : Array = f2 :?> Array
                                    let y = [| for i in x -> procOne i |] |> Array.choose (id)
                                    ARRAYCONST(y)
                                else failwithf "ERROR: unknown type %A used in variable" f2t
                            *)
                            match procAtom f2 with
                                | None -> failwithf "ERROR: unable to unbox expression %A" f2
                                | Some(v) ->
                                    Some(string(f1),v)
                    | _ -> None
            else
                None

        let init() =
            let t = args.GetType()
            if t.IsArray then
                if verbose then printfn "Is array"
                if t.GetArrayRank() = 1 then
                    if verbose then printfn "Is 1D array"
                    let arr = args :?> System.Array
                    
                    let lookup = seq {
                                        for o in arr do
                                            match procOne o with
                                                | Some(v) -> yield v
                                                | None -> ()
                                            
                                    } |> Map.ofSeq
                    
                    lookup
                else
                    Map.empty

            elif t.IsClass then
                if verbose then printf "Is class\n"
                Map.empty.Add(t.Name ,
                    CLASS(
                            seq {
                                for p in t.GetProperties()  do
                                        match procAtom (p.GetValue(args,null)) with
                                            | Some(v) -> yield p.Name,v
                                            | None -> ()
                            } |> Map.ofSeq    
                    ))
            else
                printf "Is unknown\n"
                Map.empty
        let lookup = init()

        member x.Get(v:string) = 
            let lookup1 (name:string) = match lookup.TryFind(name) with 
                                            | None -> SCONST(sprintf "missing value '%s'" name) 
                                            | Some(v) -> v
            match v.Split([| '.' |]) |> List.ofArray with
                | [single] -> lookup1 single // Simple reference to a name
                | hd::tl -> // name1.name2...
                    let initial = lookup1 hd // find object associated with name1
                    let rec iter (ns:string list) (e:Expression) =
                        match ns with
                            | [] -> e // out of name segments, take what we have
                            | hd::tl ->
                                match e with
                                    | CLASS(cMap) -> iter tl (cMap.[hd])
                                    | _ -> SCONST(sprintf "ERROR: can't expand class at point '%s', not a class, value=%s" hd (ppExpr e))
                                
                    // recurse into subsequent names
                    iter tl initial
                | [] -> failwithf "ERROR: ran out of name segments in  lookup, should not be possible"
                        
        member x.Print() =
            for kv in lookup do
                printfn "%s:%A" kv.Key kv.Value
    end

    let lookupLocals (locals:Map<string,Expression>) (name:string) =
            let lookup1 (name:string) = match locals.TryFind(name) with | None -> None | Some(v) -> Some(v)
            match name.Split([| '.' |]) |> List.ofArray with
                | [single] -> lookup1 single // Simple reference to a name
                | hd::tl -> // name1.name2...
                    match lookup1 hd with
                        | None -> None
                        | Some(initial) ->
                            let rec iter (ns:string list) (e:Expression) =
                                match ns with
                                    | [] -> Some(e) // out of name segments, take what we have
                                    | hd::tl ->
                                        match e with
                                            | CLASS(cMap) -> 
                                                match cMap.TryFind(hd) with
                                                    | Some(x) -> iter tl x
                                                    | None -> Some(SCONST(sprintf "ERROR: parsing class access, '%s' not found" hd))
                                            | _ -> 
                                                Some(SCONST(sprintf "ERROR: can't expand class at point '%s', not a class, value=\n%s" hd (ppExpr e) ))
                                
                            // recurse into subsequent names
                            iter tl initial
                | [] -> failwithf "ERROR: ran out of name segments in  lookup, should not be possible"

    type VarFetcher(ve:VarExtractor,locals:Map<string,Expression>) =
        do
            ()
        member x.Get(v) = match lookupLocals locals  v with // match locals.TryFind(v) with 
                            | None -> ve.Get(v) 
                            | Some(s) -> s

    /// Main template rendering class.  Instantiated usings
    /// string as a constructor and optionally a function to fetch sub templates  
    type Template(input:string,fetcher:string->string) = class

        /// First take the input text and create a list of parts.  This should all roll up to
        /// a single Page object.   No semantic processing is done at this stage, e.g. matching FOR/ENDFOR
        let parts = match  List.ofSeq input with
                        | Page(p) -> p
                        | _ as x -> failwithf "ERROR: failed to process (parse) page template, received %A instead" x
                        
        // Wipe out any include tokens in the page, recursively so the whole page is now assembled into one
        // document
        // ------------------------------------------------
        let rec expandIncludesExtends (parts:TemplatePart list) (res:TemplatePart list)=
                    match parts with 
                        | [] -> List.rev res // reverse final list of parts
                        | EXTENDS(file)::tl -> // Hit an extends statement, grab parent template and build out tree of template and filled out blocks
                            let parts = match List.ofSeq (fetcher file) with
                                            | Page(p) -> p
                                            | _ as x -> failwithf "ERROR: failed to process (parse) extends page template %s, received %A instead" file x
                            let (Parsed parsed) = parts
                            [EXTENDSBUNDLED(tl,parsed)]
                        | INCLUDE(file)::tl ->
                            let parts = match  List.ofSeq (fetcher file) with
                                            | Page(p) -> p
                                            | _ as x -> failwithf "ERROR: failed to process (parse) included page template %s, received %A instead" file x
                            expandIncludesExtends tl (( (expandIncludesExtends parts [] )|> List.rev)@res)
                        | hd::tl -> expandIncludesExtends tl (hd::res)

             
        //let containsIncludeOrExtends(parts:TemplatePart list) = parts |> List.exists (fun p -> match p with | INCLUDE(_) -> true | EXTENDS(_) -> true | _ -> false)
        //let rec removeIncludeOrExtends (parts:TemplatePart list) =
        //    if containsIncludeOrExtends parts then
        //        let parts' = expandIncludesExtends parts []
        //        removeIncludeOrExtends parts'
        //    else parts

        let includeFreePartsList =  expandIncludesExtends parts []

        /// Interpreted template part list, so matching open/close elements are rolled up
        /// into consolidated template parts. e.g. FORSTART/ENDFOR -> FOR()
        let (Parsed parsed) = includeFreePartsList
        
        let rec calc (vf:VarFetcher) (expression:Expression)  =
            let c = calc vf
            match expression with
            | ADD(e1,e2) -> 
                match (c e1),(c e2) with
                    | ICONST(i1),ICONST(i2) -> ICONST(i1+i2)
                    | ICONST64(i1),ICONST64(i2) -> ICONST64(i1+i2)
                    | SCONST(i1),SCONST(i2) -> SCONST(i1+i2)
                    | FCONST(i1),FCONST(i2) -> FCONST(i1+i2)
                    | _,_ as x -> failwithf "Error: evaluating expression, addition performed on inappropriate types %A" x

            | SUB(e1,e2) -> 
                match (c e1),(c e2) with
                    | ICONST(i1),ICONST(i2) -> ICONST(i1-i2)
                    | ICONST64(i1),ICONST64(i2) -> ICONST64(i1-i2)
                    | FCONST(i1),FCONST(i2) -> FCONST(i1-i2)
                    | _,_ as x -> failwithf "Error: evaluating expression, subtraction performed on inappropriate types %A" x

            | MULT(e1,e2) -> 
                match (c e1),(c e2) with
                    | ICONST(i1),ICONST(i2) -> ICONST(i1*i2)
                    | ICONST64(i1),ICONST64(i2) -> ICONST64(i1*i2)
                    | FCONST(i1),FCONST(i2) -> FCONST(i1*i2)
                    | _,_ as x -> failwithf "Error: evaluating expression, multiplication performed on inappropriate types %A" x

            | MOD(e1,e2) -> 
                match (c e1),(c e2) with
                    | ICONST(i1),ICONST(i2) -> ICONST(i1%i2)
                    | ICONST64(i1),ICONST64(i2) -> ICONST64(i1%i2)
                    | _,_ as x -> failwithf "Error: evaluating expression, mod performed on inappropriate types"

            | DIVIDE(e1,e2) -> 
                match (c e1),(c e2) with
                    | ICONST(i1),ICONST(i2) -> ICONST(i1/i2)
                    | ICONST64(i1),ICONST64(i2) -> ICONST64(i1/i2)
                    | FCONST(i1),FCONST(i2) -> FCONST(i1/i2)
                    | _,_ as x -> failwithf "Error: evaluating expression, subtraction performed on inappropriate types %A" x

            | EQUALS(e1,e2) -> 
                match (c e1),(c e2) with
                    | ICONST(i1),ICONST(i2) -> BCONST(i1=i2)
                    | ICONST64(i1),ICONST64(i2) -> BCONST(i1=i2)
                    | ICONST64(i1),ICONST(i2) -> BCONST(i1=int64 i2)
                    | ICONST(i1),ICONST64(i2) -> BCONST(int64 i1=i2)
                    | SCONST(i1),SCONST(i2) -> BCONST(i1=i2)
                    | FCONST(i1),FCONST(i2) -> BCONST(i1=i2)
                    | _,_ as x -> failwithf "Error: evaluating expression, equality performed on inappropriate types %A" x

            | GREATERTHAN(e1,e2) -> 
                match (c e1),(c e2) with
                    | ICONST(i1),ICONST(i2) -> BCONST(i1>i2)
                    | ICONST64(i1),ICONST(i2) -> BCONST(i1>int64 i2)
                    | ICONST(i1),ICONST64(i2) -> BCONST(int64 i1>i2)
                    | SCONST(i1),SCONST(i2) -> BCONST(i1>i2)
                    | FCONST(i1),FCONST(i2) -> BCONST(i1>i2)
                    | _,_ as x -> failwithf "Error: evaluating expression, > performed on inappropriate types %A" x

            | LESSTHAN(e1,e2) -> 
                match (c e1),(c e2) with
                    | ICONST(i1),ICONST(i2) -> BCONST(i1<i2)
                    | ICONST64(i1),ICONST(i2) -> BCONST(i1<int64 i2)
                    | ICONST(i1),ICONST64(i2) -> BCONST(int64 i1<i2)
                    | SCONST(i1),SCONST(i2) -> BCONST(i1<i2)
                    | FCONST(i1),FCONST(i2) -> BCONST(i1<i2)
                    | _,_ as x -> failwithf "Error: evaluating expression, > performed on inappropriate types %A" x
            | FCONST(f) as x -> x
            | ICONST(d) as x -> x
            | ICONST64(d) as x -> x
            | SCONST(s) as x -> x
            | NEGATE(e) -> 
                match (c e) with
                    | ICONST(i1) -> ICONST(-i1)
                    | ICONST64(i1) -> ICONST64(-i1)
                    | FCONST(i1) -> FCONST(-i1)
                    | _ as x -> failwithf "Error: evaluating expression, negation performed on inappropriate types %A" x

            | NOT(e) -> match (c e) with
                            | BCONST(b) -> BCONST(not b)
                            | _ as x -> failwithf "Error: evaluating expression, not performed on inappropriate types %A" x

            | VARIABLE(v) -> vf.Get(v) // TODO - implement dot notation
            | BCONST(_) as x -> x // Nothing to calculate here
            | ARRAYCONST(_) as x -> x
            | CLASS(_) -> failwithf "Error: evaluating expression: can't evaluate a class"
                
        let isTrue (expression:Expression) (vf:VarFetcher) = 
            match calc vf expression with
                | BCONST(b) -> b
                | _ as x -> 
                    failwithf "Non boolean expression %s used in if statement" (ppExpr expression)


        new (templateString:string) = Template(templateString,fun s -> sprintf "[Warning: no data source to fetch '%s']" s)
        ///
        /// What types of data could a user provide?
        /// Top level:
        ///   Array of string*obj    mapping names onto different variables
        ///   Class with members representing different fields
        member x.Render(args:obj) =
            let t = args.GetType()
            let ve = VarExtractor(args)
            /// Accumulates assembled page
            let sb = StringBuilder()
            
            let rec expandParts (locals:Map<string,Expression>) (blocks:Map<string,TemplatePart list>) (parts:TemplatePart list) =
                        match parts with
                            | [] -> ()
                            | hd::tl -> 
                                expandPart locals blocks hd
                                expandParts locals blocks tl
            and expandPart (locals:Map<string,Expression>) (blocks:Map<string,TemplatePart list>) (part:TemplatePart) =
                        match part with
                            | TEXT(t) -> sb.Append(t) |> ignore
                            | IF(expression,ifBlock,elseBlock) ->
                                if isTrue expression (VarFetcher(ve,locals)) then
                                    expandParts locals blocks ifBlock
                                else
                                    match elseBlock with
                                        | None -> () // No else statement
                                        | Some(c) -> expandParts  locals blocks c
                            | BLOCK(name,body) ->   
                                match blocks.TryFind name with
                                    | None -> expandParts locals blocks body // Use existing block body since it hasn't been replaced
                                    | Some(content) -> expandParts locals blocks content
                            | EXTENDSBUNDLED(defs,body) ->
                                  let newBlocks = defs |> List.choose (fun p -> match p with 
                                                                                | BLOCK(n,c) -> Some(n,c) 
                                                                                | _ -> None)
                                                    |> Map.ofSeq
                                  expandParts locals (blocks |> Seq.fold (fun m v -> m.Add(v.Key,v.Value)) newBlocks) body

                            | RAW ->
                                failwithf "Unimplemented RAW block expansion"
                            | INCLUDE(file) -> sb.Append(fetcher file) |> ignore 
                            | EXTENDS(file) -> failwithf "ERROR: unimpemented extends for file %s" file // FIXFIX
                            | FOR(fv,expr,parts) ->
                                let arr = 
                                    match expr with
                                        | VARIABLE(name) -> 
                                            match (match (lookupLocals locals name) with | Some(s) ->s | None -> ve.Get(name) ) (* ve.Get(name) *) with
                                                | ARRAYCONST(expArr) -> expArr
                                                | _ as x -> failwithf "ERROR: loop expansion only permitted over array types.  %s is %A" name x
                                        | ARRAYCONST(expArr) -> expArr
                                        | _ as x ->
                                            failwithf "ERROR: loop expansion only permitted over array types, not %A" x
                                for v in arr do
                                    // Now expand the inner block
                                    expandParts (locals.Add(fv,v)) blocks parts
                            | VAR(v) ->
                                // Try local variable and if that fails, passed in global
                                sb.Append((match (lookupLocals locals v) with | Some(s) ->s | None -> ve.Get(v) ) |> ppExpr) |> ignore
                            | IFSTART(_)  | BLOCKSTART(_) | ENDBLOCK(_) | ENDFOR | ENDIF | ENDRAW | FORSTART(_) | LOGIC(_) | ELSE as x -> 
                                failwithf "Internal error: Unexpected %A element that should have been consolidated" x
                            | UNKNOWNLOGIC(u) -> 
                                failwithf "Unknown logic element %A encountered in expansion" u
                            | EXPANDED(s) -> sb.Append(s) |> ignore
            
            expandParts Map.empty Map.empty parsed
            sb.ToString()
    end

    type TemplateServer(root:string) = class
        do
            ()

        member x.GetAsString(name:string) = 
            let path = Path.Combine(root,name)
            if not (File.Exists(path)) then 
                failwithf "ERROR: template path %s not found" path
            else
                File.ReadAllText(path)
        /// Get a template given a relative path to root
        member x.Get(name:string) = Template(x.GetAsString(name),fun name -> x.GetAsString(name))
    end