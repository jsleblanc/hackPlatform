module Tests

open System
open Xunit

open assembler.parsers
open assembler.types
open FParsec
    
[<Theory>]
[<InlineData("//comment")>]
[<InlineData("// comment")>]
[<InlineData("//  comment")>]
[<InlineData("//   comment ")>]
let ``Should Parse Comments`` s =
    let success = 
        match run pComment s with
        | Success _ -> true
        | _ -> false
    Assert.True(success)
    
[<Theory>]
[<InlineData("not a comment")>]
let ``Should Not Parse as Comments`` s =
    let success = 
        match run pComment s with
        | Success _ -> false
        | _ -> true
    Assert.True(success)      
   
[<Fact>]
let ``Should Parse Jump - JGT`` () =
    let success =
        match run pJump ";JGT" with
        | Success(JGT, _, _) -> true
        | _ -> false
    Assert.True(success)
    
[<Fact>]
let ``Should Parse Jump - JEQ`` () =
    let success =
        match run pJump ";JEQ" with
        | Success(JEQ, _, _) -> true
        | _ -> false
    Assert.True(success)
    
[<Fact>]
let ``Should Parse Jump - JGE`` () =
    let success =
        match run pJump ";JGE" with
        | Success(JGE, _, _) -> true
        | _ -> false
    Assert.True(success)
    
[<Fact>]
let ``Should Parse Jump - JLT`` () =
    let success =
        match run pJump ";JLT" with
        | Success(JLT, _, _) -> true
        | _ -> false
    Assert.True(success)
    
[<Fact>]
let ``Should Parse Jump - JNE`` () =
    let success =
        match run pJump ";JNE" with
        | Success(JNE, _, _) -> true
        | _ -> false
    Assert.True(success)
    
[<Fact>]
let ``Should Parse Jump - JLE`` () =
    let success =
        match run pJump ";JLE" with
        | Success(JLE, _, _) -> true
        | _ -> false
    Assert.True(success)
    
[<Fact>]
let ``Should Parse Jump - JMP`` () =
    let success =
        match run pJump ";JMP" with
        | Success(JMP, _, _) -> true
        | _ -> false
    Assert.True(success)
    
[<Theory>]
[<InlineData("test")>]
[<InlineData("//;JMP")>]
[<InlineData("  //   ;JEQ  ")>]
let ``Should Not Parse as Jump`` s =
    let success =
        match run pJump s with
        | Success _ -> false
        | _ -> true
    Assert.True(success)
    
    
[<Theory>]
[<InlineData("@1234", 1234)>]
[<InlineData("@1", 1)>]
let ``Should Parse Constant`` s exp =
    let success =
        match run pConstant s with
        | Success(Constant c, _, _) -> exp = c
        | _ -> false
    Assert.True(success)

[<Theory>]
[<InlineData("1")>]
[<InlineData(" 10")>]
[<InlineData("  100")>]
let ``Should Not Parse as Constant`` s =
    match run pConstant s with
    | Success _ -> Assert.Fail("Parse should have failed")
    | _ -> Assert.True(true)
    
[<Theory>]
[<InlineData("@foo", "foo")>]
[<InlineData("@i", "i")>]
[<InlineData("@i_a", "i_a")>]
[<InlineData("@i:a", "i:a")>]
[<InlineData("@$i", "$i")>]
[<InlineData("@i", "i")>]
[<InlineData("@i123_foo:abc$", "i123_foo:abc$")>]
let ``Should Parse Variable`` s exp =
    match run pVariable s with
    | Success(Variable v, _, _) -> Assert.Equal(v, exp)
    | _ -> Assert.Fail("Parse failed")
    
[<Theory>]
[<InlineData("@ foo")>]
[<InlineData("@1foo")>]
[<InlineData("i")>]
[<InlineData("//@i")>]
[<InlineData("$foo")>]
[<InlineData(":foo")>]
[<InlineData("_foo")>]
[<InlineData("(foo)")>]
let ``Should Not Parse as Variable`` s =
    match run pVariable s with
    | Success _ -> Assert.Fail("Parse should have failed")
    | _ -> Assert.True(true)
    
[<Theory>]
[<InlineData("(foo)", "foo")>]
[<InlineData("( foo )", "foo")>]
[<InlineData("(foo )", "foo")>]
[<InlineData("( foo)", "foo")>]
[<InlineData("(  foo  )", "foo")>]
let ``Should Parse Label`` s exp =
    match run pLabel s with
    | Success(Label l, _, _) -> Assert.Equal(l, exp)
    | _ -> Assert.Fail("Parse failed")
    
[<Theory>]
[<InlineData("()")>]
[<InlineData("( )")>]
let ``Should Not Parse as Label`` s =
    match run pLabel s with
    | Success _ -> Assert.Fail("Parse should fail")
    | _ -> Assert.True(true)
    
[<Fact>]
let ``Should Parse Destination - M`` () =
    match run pDestination "M=" with
    | Success (d, _, _) -> Assert.Equal(d, Destination.M)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse Destination - D`` () =
    match run pDestination "D=" with
    | Success (d, _, _) -> Assert.Equal(d, Destination.D)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse Destination - DM`` () =
    match run pDestination "DM=" with
    | Success (d, _, _) -> Assert.Equal(d, Destination.D ||| Destination.M)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse Destination - A`` () =
    match run pDestination "A=" with
    | Success (d, _, _) -> Assert.Equal(d, Destination.A)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse Destination - AM`` () =
    match run pDestination "AM=" with
    | Success (d, _, _) -> Assert.Equal(d, Destination.A ||| Destination.M)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse Destination - AD`` () =
    match run pDestination "AD=" with
    | Success (d, _, _) -> Assert.Equal(d, Destination.A ||| Destination.D)
    | _ -> Assert.Fail("Parsing failed")    
    
[<Fact>]
let ``Should Parse Destination - AMD`` () =
    match run pDestination "AMD=" with
    | Success (d, _, _) -> Assert.Equal(d, Destination.A ||| Destination.M ||| Destination.D)
    | _ -> Assert.Fail("Parsing failed") 
    
[<Fact>]
let ``Should Parse Destination, Order does not matter - AMD, DAM`` () =
    let t s = 
        match run pDestination s with
        | Success (d, _, _) -> Assert.Equal(d, Destination.A ||| Destination.M ||| Destination.D)
        | _ -> Assert.Fail("Parsing failed")
    t "AMD="
    t "DAM="
    
[<Theory>]
[<InlineData("Q=")>]
[<InlineData("=")>]
let ``Should Not Parse as Destination`` s =
    match run pDestination s with
    | Success _ -> Assert.Fail("Parsing should fail")
    | _ -> Assert.True(true)