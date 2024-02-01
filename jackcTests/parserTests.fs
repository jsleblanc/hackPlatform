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
    
    