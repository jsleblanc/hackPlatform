module AssemblerTests.ParserTests

open Xunit

open AssemblerTests.util
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
[<InlineData(";JMP")>]
[<InlineData(";JMP ")>]
let ``Should Parse Jump`` s =
    match run pJump s with
    | Success _ -> Assert.True(true)
    | _ -> Assert.Fail("Parsing failed")
    
[<Theory>]
[<InlineData("test")>]
[<InlineData("//;JMP")>]
[<InlineData("  //   ;JEQ  ")>]
[<InlineData("; JEQ")>]
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
[<InlineData("@i ", "i")>]
[<InlineData("@i  ", "i")>]
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

type OpCodeTestCases() =
    inherit ClassDataBase([
        [| "0"; OP_ZERO |]
        [| "1"; OP_ONE |]
        [| "-1"; OP_NEG_ONE |]
        [| "D"; OP_D |]
        [| "A"; OP_A |]
        [| "M"; OP_M |]
        [| "!D"; OP_NOT_D |]
        [| "!A"; OP_NOT_A |]
        [| "!M"; OP_NOT_M |]
        [| "-D"; OP_NEG_D |]
        [| "-A"; OP_NEG_A |]
        [| "-M"; OP_NEG_M |]
        [| "D+1"; OP_D_PLUS_ONE |]
        [| "A+1"; OP_A_PLUS_ONE |]
        [| "M+1"; OP_M_PLUS_ONE |]
        [| "D-1"; OP_D_MINUS_ONE |]
        [| "A-1"; OP_A_MINUS_ONE |]
        [| "M-1"; OP_M_MINUS_ONE |]
        [| "D+A"; OP_D_PLUS_A |]
        [| "D+M"; OP_D_PLUS_M |]
        [| "D-A"; OP_D_MINUS_A |]
        [| "D-M"; OP_D_MINUS_M |]
        [| "A-D"; OP_A_MINUS_D |]
        [| "M-D"; OP_M_MINUS_D |]
        [| "D&A"; OP_D_AND_A |]
        [| "D&M"; OP_D_AND_M |]
        [| "D|A"; OP_D_OR_A |]
        [| "D|M"; OP_D_OR_M |]
    ])
    
[<Theory>]
[<ClassData(typeof<OpCodeTestCases>)>]
let ``Should Parse as OP Code`` s exp =
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
let ``Should Parse as Instruction - Label``  () =
    match run pInstruction "(test)" with
    | Success(Label s, _, _) -> Assert.Equal("test", s)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")
    
[<Theory>]
[<ClassData(typeof<OpCodeTestCases>)>]
let ``Should Parse OP Code when a destination and a jump are added`` s exp =
    match run pCInstruction $"AM={s};JMP" with
    | Success(C_Instruction (d, c, j), _, _) -> Assert.Equal(exp, c)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")
   
[<Theory>]
[<InlineData("@100")>]
[<InlineData("@R9")>]
[<InlineData("@R10")>]
[<InlineData("@R12")>]
[<InlineData("@R13")>]
[<InlineData("@R14")>]
[<InlineData("@R15")>]
[<InlineData("@i")>]
let ``Should Parse as A Instruction`` s =
    match run pAInstruction s with
    | Success(A_Instruction s, _, _) -> Assert.True(true)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")

[<Fact>]
let ``Should Parse Line as C Instruction`` () =
    match run pLine "AM=D+1;JEQ" with
    | Success(Code (C_Instruction (d,c,j)), _, _) ->
        Assert.Equal(Some (Destination.A ||| Destination.M), d)
        Assert.Equal(OP_D_PLUS_ONE, c)
        Assert.Equal(Some JEQ, j)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")

[<Fact>]
let ``Should Parse Line as C Instruction ignoring comment`` () =
    match run pLine "AM=D+1;JEQ //comment" with
    | Success(Code (C_Instruction (d,c,j)), _, _) ->
        Assert.Equal(Some (Destination.A ||| Destination.M), d)
        Assert.Equal(OP_D_PLUS_ONE, c)
        Assert.Equal(Some JEQ, j)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")

[<Theory>]
[<InlineData("(FOO)", "FOO")>]
[<InlineData("(foo)", "foo")>]
[<InlineData(" (FOO)", "FOO")>]
let ``Should Parse Line as Label`` s exp =
    match run pLine s with
    | Success(Code (Label l), _, _) -> Assert.Equal(exp, l)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")

[<Theory>]
[<InlineData("//comment")>]
[<InlineData("// comment")>]
[<InlineData("//  comment")>]
[<InlineData("//   comment")>]
[<InlineData(" // comment")>]
let ``Should Parse Line as Comment`` s =
    match run pLine s with
    | Success (Comment c, _, _) -> Assert.True(true)
    | Failure(msg, _,_) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")

[<Theory>]
[<InlineData("@R1")>]
[<InlineData("@R2")>]
[<InlineData("@R3")>]
[<InlineData("@R4")>]
[<InlineData("@R5")>]
[<InlineData("@R6")>]
[<InlineData("@R7")>]
[<InlineData("@R8")>]
[<InlineData("@R9")>]
[<InlineData("@R10")>]
[<InlineData("@R11")>]
[<InlineData("@R12")>]
[<InlineData("@R13")>]
[<InlineData("@R14")>]
[<InlineData("@R15")>]
let ``Should Parse Built In Registers when parsing whole assembly`` s =
    match run pAssembly s with
    | Success(i, _, _) -> Assert.True(true)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
[<Fact>]
let ``Should Parse Multiline Input`` () =
    let s = """
   @R0
   D=M
   @R1
   D=D-M
   @ITSR0
   D;JGT
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
    
[<Fact>]
let ``Should Parse Multiline Input with Comments`` () =
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
    | Success(p, _, _) -> Assert.Equal(28, p.Length)
    | Failure(msg, _, _) -> Assert.Fail(msg)    