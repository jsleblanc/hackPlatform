module Tests

open System
open FParsec.CharParsers
open Xunit

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
    
[<Theory>]
[<InlineData("static int foo;")>]
[<InlineData("field int foo;")>]
[<InlineData("static int foo1,foo2,foo3;")>]
[<InlineData("static int foo1, foo2, foo3;")>]
[<InlineData("static   int   foo1,  foo2,  foo3  ;")>]
let ``Should parse class variable definition`` s =
    match run pClassVariableDeclaration s with
    | Success _ -> Assert.True(true)
    | Failure(msg, _,_) -> Assert.Fail(msg)
    