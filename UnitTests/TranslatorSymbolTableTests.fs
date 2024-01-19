module UnitTests.TranslatorTests.SymbolTableTests

open Xunit
open assembler.types
open assembler.translator

let u = uint16

[<Fact>]
let ``Should Build Symbol Table Only Variables no repetition`` () =
    let instructions = [
        A_Instruction (Variable "a")
        A_Instruction (Variable "b")
        A_Instruction (Variable "c")
        A_Instruction (Variable "d")
        A_Instruction (Variable "e")
        A_Instruction (Variable "f")
        A_Instruction (Variable "g")
        A_Instruction (Variable "h")
        A_Instruction (Variable "i")
        A_Instruction (Variable "j")
        A_Instruction (Variable "k")
        A_Instruction (Variable "l")
        A_Instruction (Variable "m")
        A_Instruction (Variable "n")
        A_Instruction (Variable "o")
        A_Instruction (Variable "p")
        A_Instruction (Variable "q")
        A_Instruction (Variable "r")
        A_Instruction (Variable "s")
        A_Instruction (Variable "t")
        A_Instruction (Variable "u")
        A_Instruction (Variable "v")
        A_Instruction (Variable "w")
        A_Instruction (Variable "x")
        A_Instruction (Variable "y")
        A_Instruction (Variable "z")
    ]
    let table = buildSymbolTable instructions
    Assert.Equal(u VARIABLE_BASE_ADDRESS, table.Item("a"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+1), table.Item("b"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+2), table.Item("c"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+3), table.Item("d"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+4), table.Item("e"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+5), table.Item("f"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+6), table.Item("g"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+7), table.Item("h"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+8), table.Item("i"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+9), table.Item("j"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+10), table.Item("k"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+11), table.Item("l"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+12), table.Item("m"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+13), table.Item("n"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+14), table.Item("o"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+15), table.Item("p"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+16), table.Item("q"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+17), table.Item("r"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+18), table.Item("s"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+19), table.Item("t"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+20), table.Item("u"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+21), table.Item("v"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+22), table.Item("w"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+23), table.Item("x"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+24), table.Item("y"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+25), table.Item("z"))


[<Fact>]
let ``Should Build Symbol Table Only Variables repeated variable should use same address`` () =
    let instructions = [
        A_Instruction (Variable "a")
        A_Instruction (Variable "b")
        A_Instruction (Variable "c")
        A_Instruction (Variable "b")
        A_Instruction (Variable "a")
    ]
    let table = buildSymbolTable instructions
    Assert.Equal(u VARIABLE_BASE_ADDRESS, table.Item("a"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+1), table.Item("b"))
    Assert.Equal(u (VARIABLE_BASE_ADDRESS+2), table.Item("c"))

[<Fact>]
let ``Labels Should Capture Instruction Addresses single label first instruction`` () =
    let instructions = [
        Label "a"
        C_Instruction (None, "1", None) //0
    ]
    let table = buildSymbolTable instructions
    Assert.Equal(u 0x0, table.Item("a"))

[<Fact>]
let ``Labels Should Capture Instruction Addresses multiple labels multiple instructions`` () =
    let instructions = [
        C_Instruction (None, "1", None) //0
        Label "a"
        C_Instruction (None, "1", None) //1
        C_Instruction (None, "1", None) //2
        C_Instruction (None, "1", None) //3
        C_Instruction (None, "1", None) //4
        Label "b"
        C_Instruction (None, "1", None) //5
        C_Instruction (None, "1", None) //6
        Label "c"
        C_Instruction (None, "1", None) //7
    ]
    let table = buildSymbolTable instructions
    Assert.Equal(u 0x1, table.Item("a"))
    Assert.Equal(u 0x5, table.Item("b"))
    Assert.Equal(u 0x7, table.Item("c"))
