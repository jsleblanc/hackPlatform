module jackcTests.codeGenTests

open Xunit
open jackc.types
open jackc.symbolTable
open jackc.codeGen

[<Fact>]
let ``Should add two constants`` () =
    let expr = J_ADD (J_Constant_Int 1s, J_Constant_Int 2s)
    let code = compileExpression expr emptySymbolTable
    let expected = ["push constant 1"; "push constant 2"; "add"]
    Assert.Equal<string list>(expected, code)
    
[<Fact>]
let ``Should add three constants (left)`` () =
    let expr = J_ADD (J_ADD (J_Constant_Int 1s, J_Constant_Int 2s), J_Constant_Int 3s)
    let code = compileExpression expr emptySymbolTable
    let expected = ["push constant 1"; "push constant 2"; "add"; "push constant 3"; "add"]
    Assert.Equal<string list>(expected, code)
    
[<Fact>]
let ``Should add three constants (right)`` () =
    let expr = J_ADD (J_Constant_Int 3s, J_ADD (J_Constant_Int 1s, J_Constant_Int 2s))
    let code = compileExpression expr emptySymbolTable
    let expected = ["push constant 3"; "push constant 1"; "push constant 2"; "add"; "add"]
    Assert.Equal<string list>(expected, code)
    
[<Fact>]
let ``Should subtract two constants`` () =
    let expr = J_SUB (J_Constant_Int 3s, J_Constant_Int 2s)
    let code = compileExpression expr emptySymbolTable
    let expected = ["push constant 3"; "push constant 2"; "sub"]
    Assert.Equal<string list>(expected, code)
    
[<Fact>]
let ``Should subtract three constants (left)`` () =
    let expr = J_SUB (J_SUB (J_Constant_Int 3s, J_Constant_Int 2s), J_Constant_Int 1s)
    let code = compileExpression expr emptySymbolTable
    let expected = ["push constant 3"; "push constant 2"; "sub"; "push constant 1"; "sub"]
    Assert.Equal<string list>(expected, code)
    
[<Fact>]
let ``Should subtract three constants (right)`` () =
    let expr = J_SUB (J_Constant_Int 3s, J_SUB (J_Constant_Int 2s, J_Constant_Int 1s))
    let code = compileExpression expr emptySymbolTable
    let expected = ["push constant 3"; "push constant 2"; "push constant 1"; "sub"; "sub"]
    Assert.Equal<string list>(expected, code)
    
[<Fact>]
let ``Should give multiplication precedence in expression`` () =
    let expr = J_ADD (J_Constant_Int 5s, J_MUL (J_Constant_Int 3s, J_Constant_Int 2s)) //5 + 3 * 2
    let code = compileExpression expr emptySymbolTable
    let expected = ["push constant 5"; "push constant 3"; "push constant 2"; "call Math.multiply 2"; "add"]
    Assert.Equal<string list>(expected, code)
    
[<Fact>]
let ``Should give addition precedence in expression`` () =
    let expr = J_MUL (J_ADD (J_Constant_Int 5s, J_Constant_Int 3s), J_Constant_Int 2s) //(5 + 3) * 2
    let code = compileExpression expr emptySymbolTable
    let expected = ["push constant 5"; "push constant 3"; "add"; "push constant 2"; "call Math.multiply 2";]
    Assert.Equal<string list>(expected, code)
    
        