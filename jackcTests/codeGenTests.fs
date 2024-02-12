module jackcTests.codeGenTests

open Xunit
open jackc.validation
open jackc.types
open jackc.symbolTable
open jackc.codeGen

[<Fact>]
let ``Should add two constants`` () =
    let expr = J_ADD (J_Constant_Int 1s, J_Constant_Int 2s)
    let code = compileExpression "" expr emptySymbolTable
    let expected = OK ["push constant 1"; "push constant 2"; "add"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should add three constants (left)`` () =
    let expr = J_ADD (J_ADD (J_Constant_Int 1s, J_Constant_Int 2s), J_Constant_Int 3s)
    let code = compileExpression "" expr emptySymbolTable
    let expected = OK ["push constant 1"; "push constant 2"; "add"; "push constant 3"; "add"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should add three constants (right)`` () =
    let expr = J_ADD (J_Constant_Int 3s, J_ADD (J_Constant_Int 1s, J_Constant_Int 2s))
    let code = compileExpression "" expr emptySymbolTable
    let expected = OK ["push constant 3"; "push constant 1"; "push constant 2"; "add"; "add"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should subtract two constants`` () =
    let expr = J_SUB (J_Constant_Int 3s, J_Constant_Int 2s)
    let code = compileExpression "" expr emptySymbolTable
    let expected = OK ["push constant 3"; "push constant 2"; "sub"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should subtract three constants (left)`` () =
    let expr = J_SUB (J_SUB (J_Constant_Int 3s, J_Constant_Int 2s), J_Constant_Int 1s)
    let code = compileExpression "" expr emptySymbolTable
    let expected = OK ["push constant 3"; "push constant 2"; "sub"; "push constant 1"; "sub"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should subtract three constants (right)`` () =
    let expr = J_SUB (J_Constant_Int 3s, J_SUB (J_Constant_Int 2s, J_Constant_Int 1s))
    let code = compileExpression "" expr emptySymbolTable
    let expected = OK ["push constant 3"; "push constant 2"; "push constant 1"; "sub"; "sub"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should give multiplication precedence in expression`` () =
    let expr = J_ADD (J_Constant_Int 5s, J_MUL (J_Constant_Int 3s, J_Constant_Int 2s)) //5 + 3 * 2
    let code = compileExpression "" expr emptySymbolTable
    let expected = OK ["push constant 5"; "push constant 3"; "push constant 2"; "call Math.multiply 2"; "add"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should give addition precedence in expression`` () =
    let expr = J_MUL (J_ADD (J_Constant_Int 5s, J_Constant_Int 3s), J_Constant_Int 2s) //(5 + 3) * 2
    let code = compileExpression "" expr emptySymbolTable
    let expected = OK ["push constant 5"; "push constant 3"; "add"; "push constant 2"; "call Math.multiply 2";]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should add two local variable together`` () =
    let expr = J_ADD (J_Variable "x", J_Variable "y")
    let symbolTable = buildSymbolTableFromEntries [
        { name = "x"; scope = SubroutineScope; varType = J_Int; segment = Local 0 }
        { name = "y"; scope = SubroutineScope; varType = J_Int; segment = Local 1 }
    ]
    let code = compileExpression "" expr symbolTable
    let expected = OK ["push local 0"; "push local 1"; "add"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should fail to compile expression that references variable missing from symbol table`` () =
    let expr = J_ADD (J_Variable "x", J_Constant_Int 5s)
    let code = compileExpression "" expr emptySymbolTable
    match code with
    | Invalid _ -> Assert.True(true)
    | _ -> Assert.Fail("Compiling expression should have failed due to missing symbol")
 
[<Fact>]
let ``Should compile let statement assigning value to variable`` () =
    let statement = J_Let (J_EQ (J_Variable "x", J_Constant_Int 5s))
    let symbolTable = buildSymbolTableFromEntries [
        { name = "x"; scope = SubroutineScope; varType = J_Int; segment = Local 0 }
    ]
    let code = compileStatement "" symbolTable statement
    let expected = OK ["push constant 5"; "pop local 0"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should compile let statement assigning expression to variable`` () =
    let statement = J_Let (J_EQ (J_Variable "v", J_ADD (J_Variable "g", J_Variable "r2")))
    let symbolTable = buildSymbolTableFromEntries [
        { name = "v"; scope = SubroutineScope; varType = J_Int; segment = Local 1 }
        { name = "g"; scope = ClassScope; varType = J_Int; segment = Static 0 }
        { name = "r2"; scope = ClassScope; varType = J_Int; segment = This 1 }
    ]
    let code = compileStatement "" symbolTable statement
    let expected = OK ["push static 0"; "push this 1"; "add"; "pop local 1"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should compile return statement that returns no value`` () =
    let statement = J_Return None
    let code = compileStatement "" emptySymbolTable statement
    let expected = OK ["push constant 0"; "return"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should compile return statement that returns this`` () =
    let statement = J_Return (Some J_Constant_This)
    let code = compileStatement "" emptySymbolTable statement
    let expected = OK ["push pointer 0"; "return"]
    Assert.Equal(expected, code)    