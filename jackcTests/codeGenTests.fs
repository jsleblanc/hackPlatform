module jackcTests.codeGenTests

open Xunit
open jackc.validation
open jackc.state
open jackc.types
open jackc.symbolTable
open jackc.codeGen

[<Fact>]
let ``Should add two constants`` () =
    let expr = J_ADD (J_Constant_Int 1s, J_Constant_Int 2s)
    let code,_ = run emptyCompilationState (compileExpression expr)
    let expected = OK ["push constant 1"; "push constant 2"; "add"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should add three constants (left)`` () =
    let expr = J_ADD (J_ADD (J_Constant_Int 1s, J_Constant_Int 2s), J_Constant_Int 3s)
    let code,_ = run emptyCompilationState (compileExpression expr)
    let expected = OK ["push constant 1"; "push constant 2"; "add"; "push constant 3"; "add"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should add three constants (right)`` () =
    let expr = J_ADD (J_Constant_Int 3s, J_ADD (J_Constant_Int 1s, J_Constant_Int 2s))
    let code,_ = run emptyCompilationState (compileExpression expr)
    let expected = OK ["push constant 3"; "push constant 1"; "push constant 2"; "add"; "add"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should subtract two constants`` () =
    let expr = J_SUB (J_Constant_Int 3s, J_Constant_Int 2s)
    let code,_ = run emptyCompilationState (compileExpression expr)
    let expected = OK ["push constant 3"; "push constant 2"; "sub"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should subtract three constants (left)`` () =
    let expr = J_SUB (J_SUB (J_Constant_Int 3s, J_Constant_Int 2s), J_Constant_Int 1s)
    let code,_ = run emptyCompilationState (compileExpression expr)
    let expected = OK ["push constant 3"; "push constant 2"; "sub"; "push constant 1"; "sub"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should subtract three constants (right)`` () =
    let expr = J_SUB (J_Constant_Int 3s, J_SUB (J_Constant_Int 2s, J_Constant_Int 1s))
    let code,_ = run emptyCompilationState (compileExpression expr)
    let expected = OK ["push constant 3"; "push constant 2"; "push constant 1"; "sub"; "sub"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should give multiplication precedence in expression`` () =
    let expr = J_ADD (J_Constant_Int 5s, J_MUL (J_Constant_Int 3s, J_Constant_Int 2s)) //5 + 3 * 2
    let code,_ = run emptyCompilationState (compileExpression expr)
    let expected = OK ["push constant 5"; "push constant 3"; "push constant 2"; "call Math.multiply 2"; "add"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should give addition precedence in expression`` () =
    let expr = J_MUL (J_ADD (J_Constant_Int 5s, J_Constant_Int 3s), J_Constant_Int 2s) //(5 + 3) * 2
    let code,_ = run emptyCompilationState (compileExpression expr)
    let expected = OK ["push constant 5"; "push constant 3"; "add"; "push constant 2"; "call Math.multiply 2";]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should add two local variable together`` () =
    let expr = J_ADD (J_Variable "x", J_Variable "y")
    let symbolTable = buildSymbolTableFromEntries [
        { name = "x"; scope = SubroutineScope; varType = J_Int; segment = Local 0 }
        { name = "y"; scope = SubroutineScope; varType = J_Int; segment = Local 1 }
    ]
    let code,_ = run (initStateWithSymbolTable symbolTable) (compileExpression expr)
    let expected = OK ["push local 0"; "push local 1"; "add"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should fail to compile expression that references variable missing from symbol table`` () =
    let expr = J_ADD (J_Variable "x", J_Constant_Int 5s)
    let code,_ = run emptyCompilationState (compileExpression expr)
    match code with
    | Invalid _ -> Assert.True(true)
    | _ -> Assert.Fail("Compiling expression should have failed due to missing symbol")
 
[<Fact>]
let ``Should compile let statement assigning value to variable`` () =
    let statement = J_Let (J_EQ (J_Variable "x", J_Constant_Int 5s))
    let symbolTable = buildSymbolTableFromEntries [
        { name = "x"; scope = SubroutineScope; varType = J_Int; segment = Local 0 }
    ]
    let code,_ = run (initStateWithSymbolTable symbolTable) (compileStatement statement)
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
    let code,_ = run (initStateWithSymbolTable symbolTable) (compileStatement statement)
    let expected = OK ["push static 0"; "push this 1"; "add"; "pop local 1"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should compile return statement that returns no value`` () =
    let statement = J_Return None
    let code,_ = run emptyCompilationState (compileStatement statement)
    let expected = OK ["push constant 0"; "return"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should compile return statement that returns this`` () =
    let statement = J_Return (Some J_Constant_This)
    let code,_ = run emptyCompilationState (compileStatement statement)
    let expected = OK ["push pointer 0"; "return"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should compile do statement making subroutine call`` () =
    let statement = J_Do (Some "Output", "printInt", [J_Constant_Int 7s])
    let code,_ = run emptyCompilationState (compileStatement statement)
    let expected = OK ["push constant 7";"call Output.printInt 1";"pop temp 0"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should compile do statement making subroutine call multiple arguments`` () =
    let statement = J_Do (Some "Output", "printInt", [J_Constant_Int 7s; J_Constant_Int 5s])
    let code,_ = run emptyCompilationState (compileStatement statement)
    let expected = OK ["push constant 7";"push constant 5";"call Output.printInt 2";"pop temp 0"]
    Assert.Equal(expected, code)
    
[<Fact>]
let ``Should compile if-else statement`` () =
    let statement = J_If_Else (J_GT (J_Variable "u", J_Constant_Int 0s), [J_Let (J_EQ (J_Variable "v", J_Variable "g"))], [])
    let symbolTable = buildSymbolTableFromEntries [
        { name = "u"; scope = SubroutineScope; varType = J_Int; segment = Local 0 }
        { name = "g"; scope = SubroutineScope; varType = J_Int; segment = Local 1 }
        { name = "v"; scope = SubroutineScope; varType = J_Int; segment = Local 2 }
    ]
    let code,_ = run (initStateWithSymbolTable symbolTable) (compileStatement statement)
    let expected = OK ["push local 0";"push constant 0";"gt";"not";"if-goto .IF_ELSE$1";"push local 1";"pop local 2";"label .IF_ELSE$1"]
    Assert.Equal(expected, code)
   
[<Fact>]
let ``Should compile if-else statement with else clause`` () =
    let statement = J_If_Else (J_EQ (J_Constant_Int 1s, J_Constant_Int 2s), [J_Do (Some "Output", "printInt", [J_Constant_Int 3s; J_Constant_Int 4s])], [J_Do (Some "Output", "printInt", [J_Constant_Int 5s; J_Constant_Int 6s])])
    let code,_ = run emptyCompilationState (compileStatement statement)
    let expected =
        OK [
            "push constant 1"
            "push constant 2"
            "eq"
            "not"
            "if-goto .IF_ELSE$1"
            "push constant 3"
            "push constant 4"
            "call Output.printInt 2"
            "pop temp 0"
            "goto .IF_ELSE$2"
            "label .IF_ELSE$1"
            "push constant 5"
            "push constant 6"
            "call Output.printInt 2"
            "pop temp 0"
            "label .IF_ELSE$2"
        ]
    Assert.Equal(expected, code)

[<Fact>]
let ``Should compile if-else statement with nested if-else statements`` () =
    let statement =
        J_If_Else (J_EQ (J_Constant_Boolean true, J_Constant_Boolean false),
                   [
                       J_If_Else (J_EQ (J_Constant_Int 1s, J_Constant_Int 2s), [
                          J_Do (None, "printInt", [J_Constant_Int 7s]) 
                       ],[
                          J_Do (None, "printInt", [J_Constant_Int 8s]) 
                       ])
                   ],
                   [J_Do (None, "printInt", [J_Constant_Int 9s])])
    let code,_ = run emptyCompilationState (compileStatement statement)
    let expected =
        OK [
            "push constant 1"
            "neg"
            "push constant 0"
            "eq"
            "not"
            "if-goto .IF_ELSE$1"
            "push constant 1"
            "push constant 2"
            "eq"
            "not"
            "if-goto .IF_ELSE$2"
            "push constant 7"            
            "call printInt 1"
            "pop temp 0"
            "goto .IF_ELSE$3"
            "label .IF_ELSE$2"
            "push constant 8"
            "call printInt 1"
            "pop temp 0"
            "label .IF_ELSE$3"
            "goto .IF_ELSE$4"
            "label .IF_ELSE$1"
            "push constant 9"
            "call printInt 1"
            "pop temp 0"
            "label .IF_ELSE$4"
        ]
    Assert.Equal(expected, code)

[<Fact>]
let ``Should compile while statement`` () =
    let statement = J_While (J_EQ (J_Constant_Int 1s, J_Constant_Int 2s), [J_Do (None, "foo", [J_Constant_Int 5s])])
    let code,_ = run emptyCompilationState (compileStatement statement)
    let expected = OK ["label .WHILE$1";"push constant 1";"push constant 2";"eq";"not";"if-goto .WHILE$2";"push constant 5";"call foo 1";"pop temp 0";"goto .WHILE$1";"label .WHILE$2"]
    Assert.Equal(expected, code)
        
[<Fact>]
let ``Should compile subroutine with empty body`` () =
    let subroutine = {
        name = "foo"
        subType = J_Method
        returnType = J_Void
        parameters = []
        variables = []
        body = [] 
    }
    let code,_ = run emptyCompilationState (compileSubroutine subroutine)
    let expected = OK ["function foo 0"]
    Assert.Equal(expected, code)