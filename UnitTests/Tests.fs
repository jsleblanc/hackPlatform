module Tests

open Xunit

open assembler.parsers
open assembler.types
open FParsec
    
[<Theory>]
[<InlineData("//comment")>]
[<InlineData("// comment")>]
[<InlineData("//  comment")>]
[<InlineData("//   comment ")>]
[<InlineData(" //comment")>]
[<InlineData("  //comment")>]
[<InlineData(" // comment")>]
[<InlineData("  //  comment")>]
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
[<InlineData(";JMP")>]
[<InlineData(" ;JMP")>]
[<InlineData(";JMP ")>]
[<InlineData(" ; JMP")>]
[<InlineData("  ;  JMP  ")>]
let ``Should Parse Jump`` s =
    match run pJump s with
    | Success _ -> Assert.True(true)
    | _ -> Assert.Fail("Parsing failed")
    
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
let ``Should Parse as Constant`` s exp =
    match run pConstant s with
    | Success(Constant c, _, _) -> Assert.Equal(exp, c)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")

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
[<InlineData(" @i", "i")>]
[<InlineData("  @i", "i")>]
[<InlineData("@i ", "i")>]
[<InlineData("@i  ", "i")>]
[<InlineData("  @i  ", "i")>]
[<InlineData("@i_a", "i_a")>]
[<InlineData("@i:a", "i:a")>]
[<InlineData("@$i", "$i")>]
[<InlineData("@i", "i")>]
[<InlineData("@i123_foo:abc$", "i123_foo:abc$")>]
[<InlineData("@ITSR0", "ITSR0")>]
[<InlineData("@MYSCREEN", "MYSCREEN")>]
let ``Should Parse as Variable`` s exp =
    match run pVariable s with
    | Success(Variable v, _, _) -> Assert.Equal(exp, v)
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
[<InlineData("(ITSR0)", "ITSR0")>]
[<InlineData("(MYSCREEN)", "MYSCREEN")>]
let ``Should Parse as Label`` s exp =
    match run pLabel s with
    | Success(Label l, _, _) -> Assert.Equal(exp, l)
    | _ -> Assert.Fail("Parse failed")
    
[<Theory>]
[<InlineData("()")>]
[<InlineData("( )")>]
let ``Should Not Parse as Label`` s =
    match run pLabel s with
    | Success _ -> Assert.Fail("Parse should fail")
    | _ -> Assert.True(true)
    
[<Fact>]
let ``Should Parse as Destination - M`` () =
    match run pDestination "M=" with
    | Success (d, _, _) -> Assert.Equal(Destination.M, d)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse as Destination - D`` () =
    match run pDestination "D=" with
    | Success (d, _, _) -> Assert.Equal(Destination.D, d)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse as Destination - DM`` () =
    match run pDestination "DM=" with
    | Success (d, _, _) -> Assert.Equal(Destination.D ||| Destination.M, d)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse as Destination - A`` () =
    match run pDestination "A=" with
    | Success (d, _, _) -> Assert.Equal(Destination.A, d)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse as Destination - AM`` () =
    match run pDestination "AM=" with
    | Success (d, _, _) -> Assert.Equal(Destination.A ||| Destination.M, d)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse as Destination - AD`` () =
    match run pDestination "AD=" with
    | Success (d, _, _) -> Assert.Equal(Destination.A ||| Destination.D, d)
    | _ -> Assert.Fail("Parsing failed")    
    
[<Fact>]
let ``Should Parse as Destination - AMD`` () =
    match run pDestination "AMD=" with
    | Success (d, _, _) -> Assert.Equal(Destination.A ||| Destination.M ||| Destination.D, d)
    | _ -> Assert.Fail("Parsing failed") 
    
[<Fact>]
let ``Should Parse as Destination, Order does not matter - AMD, DAM`` () =
    let t s = 
        match run pDestination s with
        | Success (d, _, _) -> Assert.Equal(Destination.A ||| Destination.M ||| Destination.D, d)
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
    
[<Theory>]
[<InlineData("0", "0")>]
[<InlineData(" 0", "0")>]
[<InlineData("0 ", "0")>]
[<InlineData(" 0 ", "0")>]
[<InlineData("1", "1")>]
[<InlineData("-1", "-1")>]
[<InlineData("D", "D")>]
[<InlineData("A", "A")>]
[<InlineData("!D", "!D")>]
[<InlineData("!A", "!A")>]
[<InlineData("-D", "-D")>]
[<InlineData("-A", "-A")>]
[<InlineData("D+1", "D+1")>]
[<InlineData("A+1", "A+1")>]
[<InlineData("D-1", "D-1")>]
[<InlineData("A-1", "A-1")>]
[<InlineData("D+A", "D+A")>]
[<InlineData("D-A", "D-A")>]
[<InlineData("A-D", "A-D")>]
[<InlineData("D&A", "D&A")>]
[<InlineData("D|A", "D|A")>]
[<InlineData("D | A", "D|A")>]
[<InlineData(" D | A ", "D|A")>]
[<InlineData("M", "M")>]
[<InlineData("!M", "!M")>]
[<InlineData("-M", "-M")>]
[<InlineData("M+1", "M+1")>]
[<InlineData("M-1", "M-1")>]
[<InlineData("D+M", "D+M")>]
[<InlineData("D-M", "D-M")>]
[<InlineData("M-D", "M-D")>]
[<InlineData("D&M", "D&M")>]
[<InlineData("D|M", "D|M")>]
let ``Should Parse as Computation`` s exp =
    match run pComputation s with
    | Success(c, _, _) -> Assert.Equal(exp, c)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse as Symbol - Variable``  () =
    match run pSymbol "@ITSR0" with
    | Success(Variable s, _, _) -> Assert.Equal("ITSR0", s)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse as Symbol - Predefined Symbol``  () =
    match run pSymbol "@R0" with
    | Success(Predefined s, _, _) -> Assert.Equal(R0, s)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")    
    
[<Fact>]
let ``Should Parse as Symbol - Constant``  () =
    match run pSymbol "@100" with
    | Success(Constant s, _, _) -> Assert.Equal(uint16 100, s)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse as Symbol - Label``  () =
    match run pSymbol "(test)" with
    | Success(Label s, _, _) -> Assert.Equal("test", s)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")
    
[<Theory>]
[<InlineData("D;JGT", "D")>]
[<InlineData("A;JGT", "A")>]
[<InlineData("0;JMP", "0")>]
[<InlineData("D=D-M", "D-M")>]
[<InlineData("AM=M+1", "M+1")>]
let ``Should Parse as C Instruction`` s exp =
    match run pCInstruction s with
    | Success(C_Instruction (d, c, j), _, _) -> Assert.Equal(exp, c)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")
    
[<Fact>]
let ``Should Parse Multiline Input`` () =
    let s = """
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/max/Max.asm
    
// Computes R2 = max(R0, R1)  (R0,R1,R2 refer to RAM[0],RAM[1],RAM[2])

   // D = R0 - R1
   @R0
   D=M
   @R1
   D=D-M
   // If (D > 0) goto ITSR0
   @ITSR0
   D;JGT
   // Its R1
   @R1
   D=M
   @R2
   M=D
   @END
   0;JMP
(ITSR0)
   @R0             
   D=M
   @R2
   M=D
(END)
   @END
   0;JMP
"""
    match run pAssembly s with
    | Success(p, _, _) -> Assert.Equal(20, p.Length)
    | Failure(msg, _, _) -> Assert.Fail(msg)