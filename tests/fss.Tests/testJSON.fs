module testJSON
open NUnit.Framework

open Fss.JSON

[<TestFixture>]
type TestJSONBasic() = class     
    let approxEq (f1:float) (f2:float) = 
         if ( abs (f1-f2) > 0.00000001 ) then
            Assert.Fail(sprintf "ERROR: f1 =%f f2=%f delta=%f" f1 f2 (abs (f1-f2) ))

    do
        ()
    [<Test>]
    member x.TestInt1() =
        Assert.IsTrue(parse "12345" = JINT(12345)) 
    
    [<Test>]
    member x.TestInt2() =
        Assert.IsFalse(parse "12345" = JINT(12346)) 

    [<Test>]
    member x.TestNegInt1() =
        Assert.IsFalse(parse "-12345" = JINT(-12346)) 

    [<Test>]
    member x.TestInt0() =
        Assert.IsTrue(parse "0" = JINT(0)) 

    [<Test>]
    member x.TestNegInt0() =
        let p = parse "-0"
        if not(p = JINT(0))  then
            Assert.Fail(sprintf "Testing -0 gave %A" p)
        //Assert.IsTrue(parse "-0" = JINT(0)) 

    [<Test>]
    member x.TestFloat1() =
        match parse "12345.6789"  with
            | JFLOAT(f) -> approxEq f 12345.6789
            | _ -> Assert.Fail()
        
    [<Test>]
    member x.TestFloatNeg1() =
        match parse "-12345.6789"  with
            | JFLOAT(f) -> approxEq f -12345.6789
            | _ -> Assert.Fail()

    [<Test>]
    member x.TestFloatLeadingZero1() =
        match parse "0.6789"  with
            | JFLOAT(f) -> approxEq f 0.6789
            | _ -> Assert.Fail()

    [<Test>]
    member x.TestFloatLeadingZero2() =
        match parse "-0.6789"  with
            | JFLOAT(f) -> approxEq f -0.6789
            | _ -> Assert.Fail()

    [<Test>]
    /// Basic exp notation, fractional component, positive exponent
    member x.TestFloatExp1() =
        match parse "10.5E20"  with
            | JFLOAT(f) -> approxEq f 10.5e20
            | _ -> Assert.Fail()

    [<Test>]
    /// Test negative exponent
    member x.TestFloatExp2() =
        match parse "10.5E-20"  with
            | JFLOAT(f) -> approxEq f 10.5e-20
            | _ -> Assert.Fail()

    [<Test>]
    /// Test little 'e' exponent
    member x.TestFloatExp3() =
        match parse "10.5e20"  with
            | JFLOAT(f) -> approxEq f 10.5e20
            | _ -> Assert.Fail()

    [<Test>]
    /// Test little 'e' negative exponent
    member x.TestFloatExp4() =
        match parse "10.5e-20"  with
            | JFLOAT(f) -> approxEq f 10.5e-20
            | _ -> Assert.Fail()

    [<Test>]
    /// No fractional part to exponential float
    member x.TestFloatExp5() =
        match parse "10e20"  with
            | JFLOAT(f) -> approxEq f 10e20
            | _ -> Assert.Fail()

    [<Test>]
    /// No fractional part to negative exponential float
    member x.TestFloatExp6() =
        match parse "10e-20"  with
            | JFLOAT(f) -> approxEq f 10e-20
            | _ -> Assert.Fail()

    [<Test>]
    // No trailing fractional part to float positive exponent
    member x.TestFloatExp7() =
        match parse "10e20"  with
            | JFLOAT(f) -> approxEq f 10e20
            | _ -> Assert.Fail()

    [<Test>]
    // No trailing fractional part to negative float positive exponent
    member x.TestFloatExp7a() =
        match parse "-10e20"  with
            | JFLOAT(f) -> approxEq f -10e20
            | _ -> Assert.Fail()

    [<Test>]
    // No leading whole part to negative float positive exponent
    member x.TestFloatExp7b() =
        match parse "-0.10e20"  with
            | JFLOAT(f) -> approxEq f -0.10e20
            | _ -> Assert.Fail()


    [<Test>]
    /// No leading whole part to float, negative exponent
    member x.TestFloatExp8() =
        match parse "0.5e-20"  with
            | JFLOAT(f) -> approxEq f 0.5e-20
            | _ -> Assert.Fail()

    [<Test>]
    /// '+' in exponent
    member x.TestFloatExp9() =
        match parse "10.5e+20"  with
            | JFLOAT(f) -> approxEq f 10.5e20
            | _ -> Assert.Fail()


    [<Test>]
    /// Simple floating point zero
    member x.TestFZero() =
        match parse "0.0" with
            | JFLOAT(f) -> approxEq f 0.0
            | _ -> Assert.Fail()

    [<Test>]
    /// Negative floating point zero
    member x.TestFNegZero() =
        match parse "-0.0" with
            | JFLOAT(f) -> approxEq f 0.0
            | _ -> Assert.Fail()

    [<Test>]
    /// fractional only floating point number
    member x.TestFloatLeadingNegZero() =
        match parse "-0.6789"  with
            | JFLOAT(f) -> approxEq f -0.6789
            | _ -> Assert.Fail()



end

[<TestFixture>]
type TestJSONArray() = class     
    
    do
        ()
    [<Test>]
    member x.Test2ElementIntArray() =
        Assert.IsTrue(parse "[1234,5678]" = JARRAY([|JINT(1234)  ; JINT(5678)|]))
    
    [<Test>]
    member x.Test3ElementIntArray() =
        Assert.IsTrue(parse "[1234,5678,91011]" = JARRAY([|JINT(1234)  ; JINT(5678) ; JINT(91011)|]))

    [<Test>]
    member x.Test3ElementStringArray() =
        Assert.IsTrue(parse "[\"cat\",\"cow\",\"dog\"]" = JARRAY([|JSTRING("cat")  ; JSTRING("cow") ; JSTRING("dog")|]))

    [<Test>]
    member x.TestHetArray() =
        Assert.IsTrue(parse "[\"cat\",-1234E-23,0]" = JARRAY([|JSTRING("cat")  ; JFLOAT(-1234E-23) ; JINT(0)|]))

end