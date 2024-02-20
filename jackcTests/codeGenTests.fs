module jackcTests.codeGenTests

open Xunit
open jackc.validation
open jackc.state
open jackc.types
open jackc.symbolTable
open jackc.codeGen
open jackc.api

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
    let statement = J_If_Else (J_EQ (J_Constant_Int 1s, J_Constant_Int 2s), [
        J_Return (Some (J_Constant_Int 3s))
    ], [
        J_Return (Some (J_Constant_Int 5s))
    ])
    let code,_ = run emptyCompilationState (compileStatement statement)
    let expected =
        OK [
            "push constant 1"
            "push constant 2"
            "eq"
            "not"
            "if-goto .IF_ELSE$1"
            "push constant 3"
            "return"
            "goto .IF_ELSE$2"
            "label .IF_ELSE$1"
            "push constant 5"
            "return"
            "label .IF_ELSE$2"
        ]
    Assert.Equal(expected, code)

[<Fact>]
let ``Should compile if-else statement with nested if-else statements`` () =
    let statement =
        J_If_Else (J_EQ (J_Constant_Boolean true, J_Constant_Boolean false),
                   [
                       J_If_Else (J_EQ (J_Constant_Int 1s, J_Constant_Int 2s), [
                          J_Return (Some (J_Constant_Int 7s))
                       ],[
                          J_Return (Some (J_Constant_Int 8s))
                       ])
                   ],
                   [J_Return (Some (J_Constant_Int 9s))])
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
            "return"
            "goto .IF_ELSE$3"
            "label .IF_ELSE$2"
            "push constant 8"
            "return"
            "label .IF_ELSE$3"
            "goto .IF_ELSE$4"
            "label .IF_ELSE$1"
            "push constant 9"
            "return"
            "label .IF_ELSE$4"
        ]
    Assert.Equal(expected, code)

[<Fact>]
let ``Should compile while statement`` () =
    let statement = J_While (J_EQ (J_Constant_Int 1s, J_Constant_Int 2s), [
        J_Return (Some (J_Constant_Int 5s))
    ])
    let code,_ = run emptyCompilationState (compileStatement statement)
    let expected = OK [
        "label .WHILE_EXP$1"
        "push constant 1"
        "push constant 2"
        "eq"
        "not"
        "if-goto .WHILE_END$2"
        "push constant 5"
        "return"
        "goto .WHILE_EXP$1"
        "label .WHILE_END$2"
    ]
    Assert.Equal(expected, code)
        
[<Fact>]
let ``Should compile method subroutine with empty body`` () =
    let subroutine = {
        name = "foo"
        subType = J_Method
        returnType = J_Void
        parameters = []
        variables = []
        body = [] 
    }
    let code,_ = run emptyCompilationState (compileSubroutine subroutine)
    let expected = OK ["function .foo 0";"push argument 0";"pop pointer 0"]
    Assert.Equal(expected, code)
   
[<Fact>]
let ``Should compile function subroutine with empty body`` () =
    let subroutine = {
        name = "foo"
        subType = J_Function
        returnType = J_Void
        parameters = []
        variables = []
        body = [] 
    }
    let code,_ = run emptyCompilationState (compileSubroutine subroutine)
    let expected = OK ["function .foo 0"]
    Assert.Equal(expected, code)   
    
[<Fact>]
let ``Should compile class - only simple constructor`` () =
    let code = """
class test1 {
    constructor test1 new() {
        return this;
    }
}
"""
    let expected = """function test1.new 0
push constant 0
call Memory.alloc 1
pop pointer 0
push pointer 0
return
"""
    match compileString code with
    | OK cc -> Assert.Equal(expected, cc.code)
    | Invalid e -> Assert.Fail(foldErrors e)
    
[<Fact>]
let ``Should compile class - only simple method`` () =
    let code = """
class test1 {
    method void fooMethod() {
        return;
    }
}
"""
    let expected = """function test1.fooMethod 0
push argument 0
pop pointer 0
push constant 0
return
"""
    match compileString code with
    | OK cc -> Assert.Equal(expected, cc.code)
    | Invalid e -> Assert.Fail(foldErrors e)
    
[<Fact>]
let ``Should compile class - only simple function`` () =
    let code = """
class test1 {
    function void fooFunc() {
        return;
    }
}
"""
    let expected = """function test1.fooFunc 0
push constant 0
return
"""
    match compileString code with
    | OK cc -> Assert.Equal(expected, cc.code)
    | Invalid e -> Assert.Fail(foldErrors e)
    
[<Fact>]
let ``Should compile class - only simple method that uses some variables`` () =
    let code = """
class test1 {
    method int fooMethod(int x) {
        var int j;
        let j = x + 1;
        return j;
    }
}
"""
    let expected = """function test1.fooMethod 1
push argument 0
pop pointer 0
push argument 1
push constant 1
add
pop local 0
push local 0
return
"""
    match compileString code with
    | OK cc -> Assert.Equal(expected, cc.code)
    | Invalid e -> Assert.Fail(foldErrors e)

[<Fact>]
let ``Should compile class - calling its own functions`` () =
    let code = """
class test1 {
    method int fooA(int x) {
        var int j;
        let j = x + 1;
        return j;
    }

    method void fooB() {
        do fooA(5);
        return;
    }
}
"""
    let expected = """function test1.fooA 1
push argument 0
pop pointer 0
push argument 1
push constant 1
add
pop local 0
push local 0
return
function test1.fooB 0
push argument 0
pop pointer 0
push pointer 0
push constant 5
call test1.fooA 2
pop temp 0
push constant 0
return
"""
    match compileString code with
    | OK cc -> Assert.Equal(expected, cc.code)
    | Invalid e -> Assert.Fail(foldErrors e)
    
[<Fact>]
let ``Should compile class - calling functions on specific object instances`` () =
    let code = """
class Main {

    /** Initializes a Pong game and starts running it. */
    function void main() {
        var PongGame game;
        do PongGame.newInstance();
        let game = PongGame.getInstance();
        do game.run();
        do game.dispose();
        return;
    }
}
"""
    let expected = """function Main.main 1
call PongGame.newInstance 0
pop temp 0
call PongGame.getInstance 0
pop local 0
push local 0
call PongGame.run 1
pop temp 0
push local 0
call PongGame.dispose 1
pop temp 0
push constant 0
return
"""
    match compileString code with
    | OK cc -> Assert.Equal(expected, cc.code)
    | Invalid e -> Assert.Fail(foldErrors e)
    
[<Fact>]
let ``Should compile class - field variables and complex constructor`` () =
    let code = """
class test1 {
    field int x, y;               // the ball's screen location (in pixels)
    field int lengthx, lengthy;   // distance of last destination (in pixels)

    field int d, straightD, diagonalD;   // used for straight line movement computation
    field boolean invert, positivex, positivey;   // (same)
   
    field int leftWall, rightWall, topWall, bottomWall;  // wall locations
   
    field int wall;   // last wall that the ball was bounced off of

    /** Constructs a new ball with the given initial location and wall locations. */
    constructor test1 new(int Ax, int Ay,
                         int AleftWall, int ArightWall, int AtopWall, int AbottomWall) {    	
	    let x = Ax;		
	    let y = Ay;
	    let leftWall = AleftWall;
	    let rightWall = ArightWall - 6;    // -6 for ball size
	    let topWall = AtopWall; 
	    let bottomWall = AbottomWall - 6;  // -6 for ball size
	    let wall = 0;
        do show();
        return this;
    }

    method void show() {
        return;
    }
}    
"""
    let expected = """function test1.new 0
push constant 15
call Memory.alloc 1
pop pointer 0
push argument 0
pop this 0
push argument 1
pop this 1
push argument 2
pop this 10
push argument 3
push constant 6
sub
pop this 11
push argument 4
pop this 12
push argument 5
push constant 6
sub
pop this 13
push constant 0
pop this 14
push pointer 0
call test1.show 1
pop temp 0
push pointer 0
return
function test1.show 0
push argument 0
pop pointer 0
push constant 0
return
"""
    match compileString code with
    | OK cc -> Assert.Equal(expected, cc.code)
    | Invalid e -> Assert.Fail(foldErrors e)     