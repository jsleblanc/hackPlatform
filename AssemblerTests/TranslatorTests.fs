module AssemblerTests.TranslatorTests

open Xunit
open assembler.types
open assembler.translator

[<Fact>]
let ``Should Translate Instruction to correct Binary`` () =
    let i = C_Instruction (Some (Destination.A ||| Destination.M), "M-1", Some JNE)
    let result = translateInstruction Map.empty i
    match result with
    | Some binary -> Assert.Equal("1111110010101101", binary)
    | None -> Assert.Fail("Should not have skipped a valid computation instruction")
    
[<Fact>]
let ``Should skip labels`` () =
    let i = Label "foo"
    let result = translateInstruction Map.empty i
    match result with
    | Some _ -> Assert.Fail("Labels should not generate instructions")
    | None -> Assert.True(true)