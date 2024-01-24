module vmil2asmTests.ParserTests

open FParsec
open vmil2asm.types
open vmil2asm.parser
open Xunit
open vmil2asmTests.util

type ArithmeticTestCases() =
    inherit ClassDataBase([
        [|"add"; Arithmetic ADD |]
        [|"sub"; Arithmetic SUB |]
        [|"neg"; Arithmetic NEG |]
        [|"eq"; Arithmetic EQ |]
        [|"gt"; Arithmetic GT |]
        [|"lt"; Arithmetic LT |]
        [|"and"; Arithmetic AND |]
        [|"or"; Arithmetic OR |]
        [|"not"; Arithmetic NOT |]
    ])

[<Theory>]
[<ClassData(typeof<ArithmeticTestCases>)>]
let ``Should parse arithmetic commands`` s exp =
    match run pArithmeticCommand s with
    | Success(a, _, _) -> Assert.Equal(exp, a)
    | Failure(msg, _,_) -> Assert.Fail(msg)
    
type SegmentTestCases() =
    inherit ClassDataBase([
        [|"argument"; Argument|]
        [|"local"; Local|]
        [|"static"; Static|]
        [|"constant"; Constant|]
        [|"this"; This|]
        [|"that"; That|]
        [|"pointer"; Pointer|]
        [|"temp"; Temp|]
    ])
    
[<Theory>]
[<ClassData(typeof<SegmentTestCases>)>]
let ``Should parse segments`` s exp =
    match run pSegment s with
    | Success(s, _, _) -> Assert.Equal(exp, s)
    | Failure(msg, _,_) -> Assert.Fail(msg)
    
type PushTestCases() =
    inherit ClassDataBase([
        [|"push argument 1"; PUSH (Argument, SegmentIndex (uint16 1)) |]
        [|"push local 1"; PUSH (Local, SegmentIndex (uint16 1)) |]
        [|"push static 1"; PUSH (Static, SegmentIndex (uint16 1)) |]
        [|"push constant 1"; PUSH (Constant, SegmentIndex (uint16 1)) |]
        [|"push this 1"; PUSH (This, SegmentIndex (uint16 1)) |]
        [|"push that 1"; PUSH (That, SegmentIndex (uint16 1)) |]
        [|"push pointer 1"; PUSH (Pointer, SegmentIndex (uint16 1)) |]
        [|"push temp 1"; PUSH (Temp, SegmentIndex (uint16 1)) |]
    ])
    
[<Theory>]
[<ClassData(typeof<PushTestCases>)>]
let ``Should parse push command`` s exp =
    match run pC_Push s with
    | Success (c, _, _) -> Assert.Equal(exp, c)
    | Failure(msg, _,_) -> Assert.Fail(msg)
    
type PopTestCases() =
    inherit ClassDataBase([
        [|"pop argument 1"; POP (Argument, SegmentIndex (uint16 1)) |]
        [|"pop local 1"; POP (Local, SegmentIndex (uint16 1)) |]
        [|"pop static 1"; POP (Static, SegmentIndex (uint16 1)) |]
        [|"pop constant 1"; POP (Constant, SegmentIndex (uint16 1)) |] //not valid but allowed by syntax
        [|"pop this 1"; POP (This, SegmentIndex (uint16 1)) |]
        [|"pop that 1"; POP (That, SegmentIndex (uint16 1)) |]
        [|"pop pointer 1"; POP (Pointer, SegmentIndex (uint16 1)) |]
        [|"pop temp 1"; POP (Temp, SegmentIndex (uint16 1)) |]
    ])
    
[<Theory>]
[<ClassData(typeof<PopTestCases>)>]
let ``Should parse pop command`` s exp =
    match run pC_Pop s with
    | Success (c, _, _) -> Assert.Equal(exp, c)
    | Failure(msg, _,_) -> Assert.Fail(msg)
    
[<Theory>]
[<InlineData("//comment", "comment")>]
[<InlineData(" //comment", "comment")>]
[<InlineData("  //  comment", "comment")>]
let ``Should parse comment`` s exp =
    match run pLine s with
    | Success(Comment c, _, _) -> Assert.Equal(exp, c)
    | Failure(msg, _,_) -> Assert.Fail(msg)
    | _ -> Assert.Fail("Parsing failed")

[<Fact>]
let ``Should parse multiline input with comments`` () =
    let s = """
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/07/MemoryAccess/PointerTest/PointerTest.vm

// Executes pop and push commands using the 
// pointer, this, and that segments.
push constant 3030
pop pointer 0
push constant 3040
pop pointer 1
push constant 32
pop this 2
push constant 46
pop that 6
push pointer 0
push pointer 1
add
push this 2
sub
push that 6
add
"""
    match run pInput s with
    | Success(p, _, _) -> Assert.Equal(21, p.Length)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
    
[<Theory>]
[<InlineData("label foo", "foo")>]
[<InlineData("label foo1", "foo1")>]
[<InlineData("label     foo     ", "foo")>]
let ``Should parse label`` s exp =
    match run pCommand s with
    | Success(Label l, _, _) -> Assert.Equal(exp, l)
    | Success _ -> Assert.Fail("Should have parsed as a label")
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
[<Theory>]
[<InlineData("goto foo", "foo")>]
[<InlineData("goto foo1", "foo1")>]
[<InlineData("goto     foo     ", "foo")>]
let ``Should parse goto`` s exp =
    match run pCommand s with
    | Success(Goto l, _, _) -> Assert.Equal(exp, l)
    | Success _ -> Assert.Fail("Should have parsed as a goto")
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
[<Theory>]
[<InlineData("if-goto foo", "foo")>]
[<InlineData("if-goto foo1", "foo1")>]
[<InlineData("if-goto     foo     ", "foo")>]
let ``Should parse if-goto`` s exp =
    match run pCommand s with
    | Success(If_Goto l, _, _) -> Assert.Equal(exp, l)
    | Success _ -> Assert.Fail("Should have parsed as a if-goto")
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
[<Theory>]
[<InlineData("function foo 0", "foo", 0)>]
[<InlineData("function foo1 1", "foo1", 1)>]
[<InlineData("function     foo    2 ", "foo", 2)>]
let ``Should parse function definition`` s exp c =
    match run pCommand s with
    | Success(Function (name, argCount), _, _) ->
        Assert.Equal(exp, name)
        Assert.Equal(c, argCount)
    | Success _ -> Assert.Fail("Should have parsed as a function definition")
    | Failure(msg, _, _) -> Assert.Fail(msg)
      
[<Theory>]
[<InlineData("call foo 0", "foo", 0)>]
[<InlineData("call foo1 1", "foo1", 1)>]
[<InlineData("call     foo    2 ", "foo", 2)>]
let ``Should parse function call`` s exp c =
    match run pCommand s with
    | Success(Call (name, argCount), _, _) ->
        Assert.Equal(exp, name)
        Assert.Equal(c, argCount)
    | Success _ -> Assert.Fail("Should have parsed as a function call")
    | Failure(msg, _, _) -> Assert.Fail(msg)
            
[<Theory>]
[<InlineData("return")>]
[<InlineData("return ")>]
[<InlineData("return   ")>]
let ``Should parse return`` s =
    match run pCommand s with
    | Success(Return, _, _) -> Assert.True(true)
    | Success _ -> Assert.Fail("Should have parsed return")
    | Failure(msg, _, _) -> Assert.Fail(msg)