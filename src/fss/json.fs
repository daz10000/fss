namespace Fss

// TODO
// inline string character encodings

module JSON =
    open System.Text
    type JValue = 
        | JSTRING of string
        //| JNUMBER of JNumber
        | JINT of int
        | JFLOAT of float
        | JOBJECT of Map<string,JValue>
        | JARRAY of JValue array
        | JTRUE 
        | JFALSE
        | JNULL

    /// Parse one JSON string entity if possible, recognizing leading " and reading till closing "
    let rec (|JString|_|) = function
                | '"'::tl ->
                    let sb = StringBuilder()
                    let rec accum = function
                        | '"'::tl -> Some(JSTRING(sb.ToString()),tl)
                        | hd::tl -> sb.Append(hd) |> ignore ; accum tl
                        | [] -> failwithf "ERROR: unexpected end of string in JSON String parsing, string was '%s'" (sb.ToString())
                    accum tl
                | _ -> None

    let isDigit (c:char) = c >= '0' && c <= '9'

    /// Parse the integer part of a number before the possible '.'
    let rec (|Digits|_|) = function
        | x::tl when isDigit x ->
            let sb = StringBuilder()
            sb.Append(x) |>  ignore
            let rec accum = function
                    | hd::tl when isDigit hd -> sb.Append(hd) |> ignore ; accum tl
                    | _ as x -> Some(sb.ToString() |> int,x)
            accum tl
        | _ -> None

    // Match different version of the 'E' notation
    let (|Exp|_|) = function
        | 'E'::'+'::Digits(d,rem) -> Some((10.0 ** (float d)),rem)
        | 'E'::'-'::Digits(d,rem) -> Some((10.0 ** (float -d)),rem)
        | 'E'::Digits(d,rem) -> Some((10.0 ** (float d)),rem)
        | 'e'::'+'::Digits(d,rem) -> Some((10.0 ** (float d)),rem)
        | 'e'::'-'::Digits(d,rem) -> Some((10.0 ** (float -d)),rem)
        | 'e'::Digits(d,rem) -> Some((10.0 ** (float d)),rem)
        | _ -> None

    /// Allow for possible exponent, but return 10 ** 0 if nothing found
    let (|OptExp|) = function
        | Exp(e,rem) -> e,rem
        | _ as rem -> 1.0,rem

    /// Convert an integer representing digits after the '.' to a floating point number
    let rec convertFractional (f:float) = if f >=1.0 then convertFractional (f/10.0) else f

    /// Parse one JSON numeric value
    let rec (|JNumber|_|) = function 
                | '-'::Digits(whole,'.'::Digits(fractional,OptExp(exp,rem))) -> // Negative floating point number with decimal point
                                let f = ((float whole) + (fractional |> float |> convertFractional)) * exp * -1.0 // negate
                                Some(JFLOAT(f),rem)
                | '-'::Digits(whole,Exp(exp,rem)) -> // Negative floating point number no decimal point, exponent
                                let f = (float whole) * exp * -1.0 // negate
                                Some(JFLOAT(f),rem)
                | '-'::Digits(whole,rem) -> Some(JINT(whole),rem) // negative integer
                | '0'::'.'::Digits(fractional,OptExp(exp,rem)) -> // floating point no leading whole part, decimal point, optional exp
                                let f = ( (fractional |> float |> convertFractional)) * exp 
                                Some(JFLOAT(f),rem)
                | '0'::tl -> Some(JINT(0),tl) // simple int zero
                | Digits(whole,'.'::Digits(fractional,OptExp(exp,rem))) -> // Floating point, no fractional part, optional exponent
                                let f = ((float whole) + (fractional |> float |> convertFractional)) * exp 
                                Some(JFLOAT(f),rem)
                | Digits(whole,Exp(exp,rem)) -> // float with no fractional part,  exponent means it's a float
                                let f = (float whole) * exp 
                                Some(JFLOAT(f),rem)

                | Digits(whole,rem) -> Some(JINT(whole),rem) // Just digits, no exponent, must be int
                | _ -> None

    let rec (|CommaList|_|) = function
            | JValue(v,','::CommaList(cl,rem)) -> Some(v::cl,rem)
            | JValue(v,rem) -> Some([v],rem)
            | _ -> None
        
    and  (|JArray|_|) = function
        | '['::CommaList(cl,']'::rem) -> Some(cl,rem)
        | _ -> None
                    

    and  (|JValue|_|) = function
                    | JString(js,remainder) -> Some(js,remainder)
                    | JNumber(jn,remainder) -> Some(jn,remainder)
                    | JArray(ja,remainder) -> Some(JARRAY(ja |> Array.ofList),remainder)
                    | _ -> None

    let parse (s:string) =
        let tokens = s.ToCharArray() |> List.ofArray

        let j : JValue = match tokens with
                            | JValue(jv,[]) -> jv
                            | _ -> failwithf "ERROR: failed parsing JSON string '%s' " s
                    
        
        j





