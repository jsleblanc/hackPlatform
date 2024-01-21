module AssemblerTests.TranslatorTests

open System
open FsCheck.Xunit
open Xunit
open assembler.types
open assembler.translator

[<Fact>]
let ``Should Translate Instruction to correct Binary`` () =
    let i = C_Instruction (Some (Destination.A ||| Destination.M), OP_M_MINUS_ONE, Some JNE)
    let result = translateInstructionBuiltinSymbolTable i
    match result with
    | Some binary -> Assert.Equal("1111110010101101", binary)
    | None -> Assert.Fail("Should not have skipped a valid computation instruction")
    
[<Fact>]
let ``Should skip labels`` () =
    let i = Label "foo"
    let result = translateInstructionBuiltinSymbolTable i
    match result with
    | Some _ -> Assert.Fail("Labels should not generate instructions")
    | None -> Assert.True(true)
    
[<Property>]
let ``Jumps should always use the lowest 3 bits`` (j:Jump) =
    let i = C_Instruction (None, OP_D_AND_A, Some j) //D&A instruction encodes to 0
    let result = translateInstructionBuiltinSymbolTable i
    match result with
    | Some binary -> binary.StartsWith("1110000000000") && binary.Length = 16
    | None -> false
    
[<Property>]
let ``Destination should always use bits 3,4,5`` (d:Destination) =
    let i = C_Instruction (Some d, OP_D_AND_A, None)
    let result = translateInstructionBuiltinSymbolTable i
    match result with
    | Some binary -> binary.StartsWith("1110000000") && binary.Length = 16 && binary.EndsWith("000")
    | None -> false
    
[<Property>]
let ``Should translate constant value to correct binary`` (c:uint16) =
    let i = A_Instruction (Constant c)
    let result = translateInstructionBuiltinSymbolTable i
    match result with
    | Some binary -> binary = i2b c 
    | None -> false
    
[<Property>]
let ``Should translate built in symbol to address from symbol table`` (s:BuiltInSymbol) =
    let i = A_Instruction (Predefined s)
    let symbolTable = seedSymbolMap
    let result = translateInstructionBuiltinSymbolTable i
    match result with
    | Some binary -> binary = i2b symbolTable[builtInSymbolToString s]
    | None -> false