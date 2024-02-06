module jackcTests.parserTests

open FParsec.CharParsers
open Xunit

open jackcTests.util
open jackc.types
open jackc.parser

[<Theory>]
[<InlineData("//comment")>]
[<InlineData("/* comment */")>]
[<InlineData("/** api comment */")>]
let ``Should parse comments`` s =
    match run pComment s with
    | Success _ -> Assert.True(true)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
type JackExpressionTestCases() =
    inherit ClassDataBase([
        [|"1";  (J_Constant_Int 1s)|]
        [|"-1"; J_NEG (J_Constant_Int 1s)|]
        [|"~1"; J_NOT (J_Constant_Int 1s)|]
        [|"(1)";  (J_Constant_Int 1s)|]
        [|"( 1 )";  (J_Constant_Int 1s)|]
        [|"\"foo\"";  (J_Constant_String "foo")|]
        [|"(\"foo\")";  (J_Constant_String "foo")|]
        [|"\"foo\" + \"bar\""; J_ADD (J_Constant_String "foo", J_Constant_String "bar") |]        
        [|"(\"foo\" + \"bar\")"; J_ADD (J_Constant_String "foo", J_Constant_String "bar") |]        
        [|"true";  (J_Constant_Boolean true)|]
        [|"true ";  (J_Constant_Boolean true)|]
        [|"false";  (J_Constant_Boolean false)|]
        [|"false ";  (J_Constant_Boolean false)|]
        [|"null";  J_Constant_Null|]
        [|"null ";  J_Constant_Null|]
        [|"this";  J_Constant_This|]
        [|"this ";  J_Constant_This|]
        [|"1 + 2";  J_ADD ((J_Constant_Int 1s), (J_Constant_Int 2s)) |]
        [|"(1 + 2)";  J_ADD ((J_Constant_Int 1s), (J_Constant_Int 2s)) |]
        [|"1+2";  J_ADD ((J_Constant_Int 1s), (J_Constant_Int 2s)) |]
        [|"(1+2)";  J_ADD ((J_Constant_Int 1s), (J_Constant_Int 2s)) |]
        [|"1 + 2 + 3"; J_ADD (J_ADD (J_Constant_Int 1s, J_Constant_Int 2s), J_Constant_Int 3s) |]
        [|"1 + 2 - 3"; J_SUB (J_ADD (J_Constant_Int 1s, J_Constant_Int 2s), J_Constant_Int 3s) |]
        [|"1 + 2 * 3"; J_ADD (J_Constant_Int 1s, (J_MUL (J_Constant_Int 2s, J_Constant_Int 3s))) |]
        [|"(1 + (2 + 3))"; J_ADD (J_Constant_Int 1s, (J_ADD (J_Constant_Int 2s, J_Constant_Int 3s))) |]
        [|"(1 + (2 * 3))"; J_ADD (J_Constant_Int 1s, (J_MUL (J_Constant_Int 2s, J_Constant_Int 3s))) |]
        [|"A & B | C"; J_OR (J_AND (J_Variable "A", J_Variable "B"), J_Variable "C") |]
        [|"A&B|C"; J_OR (J_AND (J_Variable "A", J_Variable "B"), J_Variable "C") |]
        [|"A & (B | C)"; J_AND (J_Variable "A", J_OR (J_Variable "B", J_Variable "C")) |]
        [|"A&(B|C)"; J_AND (J_Variable "A", J_OR (J_Variable "B", J_Variable "C")) |]
        [|"1 < 2"; J_LT (J_Constant_Int 1s, J_Constant_Int 2s) |]
        [|"1 > 2"; J_GT (J_Constant_Int 1s, J_Constant_Int 2s) |]
        [|"1 = 2"; J_EQ (J_Constant_Int 1s, J_Constant_Int 2s) |]
        [|"foo(1,2,3)"; J_Subroutine_Call (None, "foo", [J_Constant_Int 1s; J_Constant_Int 2s; J_Constant_Int 3s;])|]
        [|"foo()"; J_Subroutine_Call (None, "foo", [])|]
        [|"obj.foo()"; J_Subroutine_Call (Some "obj", "foo", [])|]
        [|"MyClass.foo(1,2,3)"; J_Subroutine_Call (Some "MyClass", "foo", [J_Constant_Int 1s; J_Constant_Int 2s; J_Constant_Int 3s;])|]
        [|"obj.foo(1,2,3)"; J_Subroutine_Call (Some "obj", "foo", [J_Constant_Int 1s; J_Constant_Int 2s; J_Constant_Int 3s;])|]
    ])
    
[<Theory>]
[<ClassData(typeof<JackExpressionTestCases>)>]
let ``Should parse expression`` s exp =
    match run pExpression s with
    | Success(e, _, _) -> Assert.Equal(exp, e)
    | Failure(msg, _, _) -> Assert.Fail(msg)    

[<Theory>]
[<ClassData(typeof<JackExpressionTestCases>)>]
let ``Should parse expressions used inside array indexer`` s exp =
    let newString = $"myArray[{s}]"
    let newExpected = J_Array_Index ("myArray", exp)
    match run pExpression newString with
    | Success(e, _, _) -> Assert.Equal(newExpected, e)
    | Failure(msg, _, _) -> Assert.Fail(msg)    

type JackStatementTestCases() =
    inherit ClassDataBase([
        [|"return;"; J_Return None|]
        [|"return 1;"; J_Return (Some (J_Constant_Int 1s))|]
        [|"return (1);"; J_Return (Some (J_Constant_Int 1s))|]
        [|"let x = 1;"; J_Let (J_EQ (J_Variable "x", J_Constant_Int 1s)) |]
        [|"let x=1;"; J_Let (J_EQ (J_Variable "x", J_Constant_Int 1s)) |]
        [|"let x[1]=2;"; J_Let (J_EQ (J_Array_Index ("x", J_Constant_Int 1s), J_Constant_Int 2s)) |]
        [|"let x[1] = 2;"; J_Let (J_EQ (J_Array_Index ("x", J_Constant_Int 1s), J_Constant_Int 2s)) |]
        [|"while (1) {}"; J_While (J_Constant_Int 1s, [])|]
        [|"while (true) { let x=1; return 2;}"; J_While (J_Constant_Boolean true, [J_Let (J_EQ (J_Variable "x", J_Constant_Int 1s)); J_Return (Some (J_Constant_Int 2s))])|]
        [|"if (true) {}"; J_If_Else ((J_Constant_Boolean true), [], [])|]
        [|"if (true) { return 1; } else { return 2; }"; J_If_Else ((J_Constant_Boolean true), [J_Return (Some (J_Constant_Int 1s))], [J_Return (Some (J_Constant_Int 2s))])|]
        [|"do foo();"; J_Do (None, "foo", [])|]
        [|"do Bar.foo();"; J_Do (Some "Bar", "foo", [])|]
    ])
    
[<Theory>]
[<ClassData(typeof<JackStatementTestCases>)>]
let ``Should parse statement`` s exp =
    match run pStatement s with
    | Success(st, _, _) -> Assert.Equal(exp, st)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
[<Fact>]
let ``Should parse multiline if-else statement`` () =
    let str = """if (true) {
    let x = 1;
    return x;
} else {
    let y = 2;
    return y;
}
"""
    match run pStatement str with
    | Success(J_If_Else (c, msl, esl), _, _) ->
        Assert.Equal(J_Constant_Boolean true, c)
        Assert.Equal([|J_Let (J_EQ (J_Variable "x", J_Constant_Int 1s)); J_Return (Some (J_Variable "x"))|], msl)
        Assert.Equal([|J_Let (J_EQ (J_Variable "y", J_Constant_Int 2s)); J_Return (Some (J_Variable "y"))|], esl)
    | Success _ -> Assert.Fail("Should have parsed as if-else statement")
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
    
type JackSubroutineVariableDeclarationTestCases() =
    inherit ClassDataBase([
        [| "var int foo;"; [(J_Int, "foo")] |]
        [| "var char foo;"; [(J_Char, "foo")] |]
        [| "var boolean foo;"; [(J_Boolean, "foo")] |]
        [| "var mytype foo;"; [(J_Class "mytype", "foo")] |]
        [| "var    mytype    foo1,    foo2,     foo3;"; [(J_Class "mytype", "foo1");(J_Class "mytype", "foo2");(J_Class "mytype", "foo3")] |]
        [| "var int foo;"; [(J_Int, "foo")] |]
        [| "var char foo;"; [(J_Char, "foo")] |]
        [| "var boolean foo;"; [(J_Boolean, "foo")] |]
        [| "var mytype foo;"; [(J_Class "mytype", "foo")] |]        
        [| "var int foo1,foo2;"; [(J_Int, "foo1");(J_Int, "foo2")] |]
        [| "var char foo1,foo2;"; [(J_Char, "foo1");(J_Char, "foo2")] |]
        [| "var boolean foo1,foo2;"; [(J_Boolean, "foo1");(J_Boolean, "foo2")] |]
        [| "var mytype foo1,foo2;"; [(J_Class "mytype", "foo1");(J_Class "mytype", "foo2")] |]
        [| "var int foo1,foo2;"; [(J_Int, "foo1");(J_Int, "foo2")] |]
        [| "var char foo1,foo2;"; [(J_Char, "foo1");(J_Char, "foo2")] |]
        [| "var boolean foo1,foo2;"; [(J_Boolean, "foo1");(J_Boolean, "foo2")] |]
        [| "var mytype foo1,foo2;"; [(J_Class "mytype", "foo1");(J_Class "mytype","foo2")] |]        
        ])
    
[<Theory>]
[<ClassData(typeof<JackSubroutineVariableDeclarationTestCases>)>]
let ``Should parse subroutine variable declarations`` s (exp: (JackTypes * JackVariableName) list) =
    let cmp (t1:JackTypes,n1:JackVariableName) (t2,n2) =
        Assert.Equal(t1,t2)
        Assert.Equal(n1,n2)
    match run pSubroutineVariableDeclaration s with
    | Success(jc, _, _) -> List.map2 cmp exp jc |> ignore
    | Success _ -> Assert.Fail("Should have parsed as class variable declaration")
    | Failure(msg, _, _) -> Assert.Fail(msg)
        
        
type JackSubroutineDeclarationTestCases() =
    inherit ClassDataBase([
        [| "constructor myType new(int x, char y, boolean b) {}"
           { name = "new"; subType = J_Constructor; returnType = J_ReturnType (J_Class "myType"); parameters = [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")]; body = []; variables = []; } |]
        [| "function myType fooFunc(int x, char y, boolean b) {}"
           { name = "fooFunc"; subType = J_Function; returnType = J_ReturnType (J_Class "myType"); parameters = [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")]; body = []; variables = []; } |]
        [| "function int fooFunc(int x, char y, boolean b) {}"
           { name = "fooFunc"; subType = J_Function; returnType = J_ReturnType J_Int; parameters = [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")]; body = []; variables = []; } |]
        [| "function char fooFunc(int x, char y, boolean b) {}"
           { name = "fooFunc"; subType = J_Function; returnType = J_ReturnType J_Char; parameters = [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")]; body = []; variables = []; } |]
        [| "function boolean fooFunc(int x, char y, boolean b) {}"
           { name = "fooFunc"; subType = J_Function; returnType = J_ReturnType J_Boolean; parameters = [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")]; body = []; variables = []; } |]
        [| "method myType fooFunc(int x, char y, boolean b) {}"
           { name = "fooFunc"; subType = J_Method; returnType = J_ReturnType (J_Class "myType"); parameters = [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")]; body = []; variables = []; } |]
        [| "method int fooFunc(int x, char y, boolean b) {}"
           { name = "fooFunc"; subType = J_Method; returnType = J_ReturnType J_Int; parameters = [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")]; body = []; variables = []; } |]
        [| "method char fooFunc(int x, char y, boolean b) {}"
           { name = "fooFunc"; subType = J_Method; returnType = J_ReturnType J_Char; parameters = [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")]; body = []; variables = []; } |]
        [| "method boolean fooFunc(int x, char y, boolean b) {}"
           { name = "fooFunc"; subType = J_Method; returnType = J_ReturnType J_Boolean; parameters = [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")]; body = []; variables = []; } |]
        [| "method void fooFunc(int x, char y, boolean b) {}"
           { name = "fooFunc"; subType = J_Method; returnType = J_Void; parameters = [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")]; body = []; variables = []; } |]    ])
    
[<Theory>]
[<ClassData(typeof<JackSubroutineDeclarationTestCases>)>]
let ``Should parse subroutine declaration`` s exp =
    match run pSubroutineDeclaration s with
    | Success(js, _, _) -> Assert.Equal(exp, js)
    | Success _ -> Assert.Fail("Should parse as subroutine declaration")
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
    
[<Fact>]
let ``Should parse subroutine`` () =
    let sub = """function void foo(int x, char y) {
    var int foo1,foo2;
    var boolean bar1,bar2;
    do someFunc();
    return;
}
"""
    let expected = {
        name = "foo"
        returnType = J_Void
        subType = J_Function
        parameters = [
            (J_Int, "x")
            (J_Char, "y")
        ]
        variables = [
            (J_Int, "foo1")
            (J_Int, "foo2")
            (J_Boolean, "bar1")
            (J_Boolean, "bar2")
        ]
        body = [
            (J_Do (None, "someFunc", []))
            (J_Return None)
        ] 
    }
    match run pSubroutineDeclaration sub with
    | Success(jackSubroutine, _, _) -> Assert.Equal(expected, jackSubroutine)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
    
type JackClassVariableDeclarationTestCases() =
    inherit ClassDataBase([
        [| "static int foo;"; [(J_Static, J_Int, "foo")] |]
        [| "static char foo;"; [(J_Static, J_Char, "foo")] |]
        [| "static boolean foo;"; [(J_Static, J_Boolean, "foo")] |]
        [| "static mytype foo;"; [(J_Static, J_Class "mytype", "foo")] |]
        [| "static    mytype    foo1,    foo2,     foo3;"; [(J_Static, J_Class "mytype", "foo1");(J_Static, J_Class "mytype", "foo2");(J_Static, J_Class "mytype", "foo3")] |]
        [| "field int foo;"; [(J_Field, J_Int, "foo")] |]
        [| "field char foo;"; [(J_Field, J_Char, "foo")] |]
        [| "field boolean foo;"; [(J_Field, J_Boolean, "foo")] |]
        [| "field mytype foo;"; [(J_Field, J_Class "mytype", "foo")] |]        
        [| "static int foo1,foo2;"; [(J_Static, J_Int, "foo1");(J_Static, J_Int, "foo2")] |]
        [| "static char foo1,foo2;"; [(J_Static, J_Char, "foo1");(J_Static, J_Char, "foo2")] |]
        [| "static boolean foo1,foo2;"; [(J_Static, J_Boolean, "foo1");(J_Static, J_Boolean, "foo2")] |]
        [| "static mytype foo1,foo2;"; [(J_Static, J_Class "mytype", "foo1");(J_Static, J_Class "mytype", "foo2")] |]
        [| "field int foo1,foo2;"; [(J_Field, J_Int, "foo1");(J_Field, J_Int, "foo2")] |]
        [| "field char foo1,foo2;"; [(J_Field, J_Char, "foo1");(J_Field, J_Char, "foo2")] |]
        [| "field boolean foo1,foo2;"; [(J_Field, J_Boolean, "foo1");(J_Field, J_Boolean, "foo2")] |]
        [| "field mytype foo1,foo2;"; [(J_Field, J_Class "mytype", "foo1");(J_Field, J_Class "mytype", "foo2")] |]        
        ])
    
[<Theory>]
[<ClassData(typeof<JackClassVariableDeclarationTestCases>)>]
let ``Should parse class variable declarations`` s exp =
    let cmp (s1:JackClassVariableScope,t1:JackTypes,n1:JackVariableName) (s2,t2,n2) =
        Assert.Equal(s1,s2)
        Assert.Equal(t1,t2)
        Assert.Equal(n1,n2)
    match run pClassVariableDeclaration s with
    | Success(jc, _, _) -> List.map2 cmp exp jc |> ignore
    | Success _ -> Assert.Fail("Should have parsed as class variable declaration")
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
[<Fact>]
let ``Should parse class`` () =
    let str = """class Main {
   function void main() {
      do Output.printString("Hello world!");
      do Output.println();
      return;
   }
}
"""
    let expected = {
        name = "Main"
        variables = []
        subroutines = [
            {
                name = "main"
                subType = J_Function 
                returnType = J_Void
                parameters = []
                variables = []
                body = [
                    J_Do (Some "Output", "printString", [J_Constant_String "Hello world!"])
                    J_Do (Some "Output", "println", [])
                    J_Return None
                ] 
            }            
        ] 
    }
    match run pClass str with
    | Success(c, _, _) -> Assert.Equal(expected, c)
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
[<Fact>]
let ``Should parse class that includes comments in the code`` () =
    let str = """/** Hello World program. */
class Main {
   /* comment */
   function void main() {
      var int x; //comment
      /* Prints some text using the standard library. */
      //second comment
      do Output.printString("Hello world!"); /* comment */
      do Output.println();      // New line
      let x = 1; //comment
      /*
         comment block
      */
      return; /*comment*/
   }
}
"""
    let expected = {
        name = "Main"
        variables = []
        subroutines = [
            {
                name = "main"
                subType = J_Function 
                returnType = J_Void
                parameters = []
                variables = [
                    (J_Int, "x")
                ]
                body = [
                    J_Do (Some "Output", "printString", [J_Constant_String "Hello world!"])
                    J_Do (Some "Output", "println", [])
                    J_Let (J_EQ (J_Variable "x", J_Constant_Int 1s))
                    J_Return None
                ] 
            }            
        ] 
    }
    match run pClass str with
    | Success(c, _, _) -> Assert.Equal(expected, c)
    | Failure(msg, _, _) -> Assert.Fail(msg)    