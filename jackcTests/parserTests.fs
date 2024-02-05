module jackcTests.parserTests

open System
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
    | Success(Comment, _, _) -> Assert.True(true)
    | Success _ -> Assert.Fail("Should have parsed as a comment")
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
type JackClassVariableDeclarationTestCases() =
    inherit ClassDataBase([
        [| "static int foo;"; J_ClassVariableDeclaration(J_Static, J_Int, ["foo"]) |]
        [| "static char foo;"; J_ClassVariableDeclaration(J_Static, J_Char, ["foo"]) |]
        [| "static boolean foo;"; J_ClassVariableDeclaration(J_Static, J_Boolean, ["foo"]) |]
        [| "static mytype foo;"; J_ClassVariableDeclaration(J_Static, J_Class "mytype", ["foo"]) |]
        [| "static    mytype    foo1,    foo2,     foo3;"; J_ClassVariableDeclaration(J_Static, J_Class "mytype", ["foo1";"foo2";"foo3"]) |]
        [| "field int foo;"; J_ClassVariableDeclaration(J_Field, J_Int, ["foo"]) |]
        [| "field char foo;"; J_ClassVariableDeclaration(J_Field, J_Char, ["foo"]) |]
        [| "field boolean foo;"; J_ClassVariableDeclaration(J_Field, J_Boolean, ["foo"]) |]
        [| "field mytype foo;"; J_ClassVariableDeclaration(J_Field, J_Class "mytype", ["foo"]) |]        
        [| "static int foo1,foo2;"; J_ClassVariableDeclaration(J_Static, J_Int, ["foo1";"foo2"]) |]
        [| "static char foo1,foo2;"; J_ClassVariableDeclaration(J_Static, J_Char, ["foo1";"foo2"]) |]
        [| "static boolean foo1,foo2;"; J_ClassVariableDeclaration(J_Static, J_Boolean, ["foo1";"foo2"]) |]
        [| "static mytype foo1,foo2;"; J_ClassVariableDeclaration(J_Static, J_Class "mytype", ["foo1";"foo2"]) |]
        [| "field int foo1,foo2;"; J_ClassVariableDeclaration(J_Field, J_Int, ["foo1";"foo2"]) |]
        [| "field char foo1,foo2;"; J_ClassVariableDeclaration(J_Field, J_Char, ["foo1";"foo2"]) |]
        [| "field boolean foo1,foo2;"; J_ClassVariableDeclaration(J_Field, J_Boolean, ["foo1";"foo2"]) |]
        [| "field mytype foo1,foo2;"; J_ClassVariableDeclaration(J_Field, J_Class "mytype", ["foo1";"foo2"]) |]        
        ])
    
[<Theory>]
[<ClassData(typeof<JackClassVariableDeclarationTestCases>)>]
let ``Should parse class variable declarations`` s exp =
    match run pClassVariableDeclaration s with
    | Success(jc, _, _) -> Assert.Equal(exp, jc)
    | Success _ -> Assert.Fail("Should have parsed as class variable declaration")
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
type JackLocalVariableDeclarationTestCases() =
    inherit ClassDataBase([
        [| "var int foo;"; J_LocalVariableDeclaration(J_Int, ["foo"]) |]
        [| "var char foo;"; J_LocalVariableDeclaration(J_Char, ["foo"]) |]
        [| "var boolean foo;"; J_LocalVariableDeclaration(J_Boolean, ["foo"]) |]
        [| "var mytype foo;"; J_LocalVariableDeclaration(J_Class "mytype", ["foo"]) |]
        [| "var    mytype    foo1,    foo2,     foo3;"; J_LocalVariableDeclaration(J_Class "mytype", ["foo1";"foo2";"foo3"]) |]
        [| "var int foo;"; J_LocalVariableDeclaration(J_Int, ["foo"]) |]
        [| "var char foo;"; J_LocalVariableDeclaration(J_Char, ["foo"]) |]
        [| "var boolean foo;"; J_LocalVariableDeclaration(J_Boolean, ["foo"]) |]
        [| "var mytype foo;"; J_LocalVariableDeclaration(J_Class "mytype", ["foo"]) |]        
        [| "var int foo1,foo2;"; J_LocalVariableDeclaration(J_Int, ["foo1";"foo2"]) |]
        [| "var char foo1,foo2;"; J_LocalVariableDeclaration(J_Char, ["foo1";"foo2"]) |]
        [| "var boolean foo1,foo2;"; J_LocalVariableDeclaration(J_Boolean, ["foo1";"foo2"]) |]
        [| "var mytype foo1,foo2;"; J_LocalVariableDeclaration(J_Class "mytype", ["foo1";"foo2"]) |]
        [| "var int foo1,foo2;"; J_LocalVariableDeclaration(J_Int, ["foo1";"foo2"]) |]
        [| "var char foo1,foo2;"; J_LocalVariableDeclaration(J_Char, ["foo1";"foo2"]) |]
        [| "var boolean foo1,foo2;"; J_LocalVariableDeclaration(J_Boolean, ["foo1";"foo2"]) |]
        [| "var mytype foo1,foo2;"; J_LocalVariableDeclaration(J_Class "mytype", ["foo1";"foo2"]) |]        
        ])
    
[<Theory>]
[<ClassData(typeof<JackLocalVariableDeclarationTestCases>)>]
let ``Should parse local variable declarations`` s exp =
    match run pLocalVariableDeclaration s with
    | Success(jc, _, _) -> Assert.Equal(exp, jc)
    | Success _ -> Assert.Fail("Should have parsed as class variable declaration")
    | Failure(msg, _, _) -> Assert.Fail(msg)
        
        
type JackSubroutineDeclarationTestCases() =
    inherit ClassDataBase([
        [| "constructor myType new(int x, char y, boolean b)"
           J_SubroutineDeclaration(J_Constructor, J_Return (J_Class "myType"), "new", [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")], "body") |]
        [| "function myType fooFunc(int x, char y, boolean b)"
           J_SubroutineDeclaration(J_Function, J_Return (J_Class "myType"), "fooFunc", [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")], "body") |]
        [| "function int fooFunc(int x, char y, boolean b)"
           J_SubroutineDeclaration(J_Function, J_Return J_Int, "fooFunc", [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")], "body") |]
        [| "function char fooFunc(int x, char y, boolean b)"
           J_SubroutineDeclaration(J_Function, J_Return J_Char, "fooFunc", [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")], "body") |]
        [| "function boolean fooFunc(int x, char y, boolean b)"
           J_SubroutineDeclaration(J_Function, J_Return J_Boolean, "fooFunc", [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")], "body") |]
        [| "method myType fooFunc(int x, char y, boolean b)"
           J_SubroutineDeclaration(J_Method, J_Return (J_Class "myType"), "fooFunc", [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")], "body") |]
        [| "method int fooFunc(int x, char y, boolean b)"
           J_SubroutineDeclaration(J_Method, J_Return J_Int, "fooFunc", [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")], "body") |]
        [| "method char fooFunc(int x, char y, boolean b)"
           J_SubroutineDeclaration(J_Method, J_Return J_Char, "fooFunc", [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")], "body") |]
        [| "method boolean fooFunc(int x, char y, boolean b)"
           J_SubroutineDeclaration(J_Method, J_Return J_Boolean, "fooFunc", [(J_Int, "x");(J_Char, "y");(J_Boolean, "b")], "body") |]
    ])
    
[<Theory>]
[<ClassData(typeof<JackSubroutineDeclarationTestCases>)>]
let ``Should parse subroutine declaration`` s exp =
    match run pSubroutineDeclaration s with
    | Success(js, _, _) -> Assert.Equal(exp, js)
    | Success _ -> Assert.Fail("Should parse as subroutine declaration")
    | Failure(msg, _, _) -> Assert.Fail(msg)
    
type JackExpressionTestCases() =
    inherit ClassDataBase([
        [|"1";  (J_Constant_Int 1s)|]
        [|"-1"; J_NEG (J_Constant_Int 1s)|]
        [|"~1"; J_NOT (J_Constant_Int 1s)|]
        [|"(1)";  (J_Constant_Int 1s)|]
        [|"\"foo\"";  (J_Constant_String "foo")|]
        [|"(\"foo\")";  (J_Constant_String "foo")|]
        [|"\"foo\" + \"bar\""; J_ADD (J_Constant_String "foo", J_Constant_String "bar") |]        
        [|"(\"foo\" + \"bar\")"; J_ADD (J_Constant_String "foo", J_Constant_String "bar") |]        
        [|"true";  (J_Constant_Boolean true)|]
        [|"true ";  (J_Constant_Boolean true)|]
        [|"false";  (J_Constant_Boolean false)|]
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
        [|"A & (B | C)"; J_AND (J_Variable "A", J_OR (J_Variable "B", J_Variable "C")) |]
        //[|"foo[1]";  (J_ArrayIndex ("foo", J_Expression (J_Constant_Int (int16 1), [])), []) |]
        //[|"foo[1 + 2]";  (J_ArrayIndex ("foo", J_Expression (J_Constant_Int (int16 1), [ (J_ADD, J_Constant_Int (int16 2))])), []) |]
        //[|"foo[1 + 2 + 3]";  (J_ArrayIndex ("foo", J_Expression (J_Constant_Int (int16 1), [(J_ADD, J_Constant_Int (int16 2));(J_ADD, J_Constant_Int (int16 3)) ])), []) |]
    ])
    
[<Theory>]
[<ClassData(typeof<JackExpressionTestCases>)>]
let ``Should parse expression`` s exp =
    match run pExpression s with
    | Success(e, _, _) -> Assert.Equal(exp, e)
    | Failure(msg, _, _) -> Assert.Fail(msg)    

