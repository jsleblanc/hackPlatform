module jackcTests.symbolTableTests

open Xunit
open jackc.types
open jackc.symbolTable

[<Fact>]
let ``Should build symbol table for class, simple class 1 static variable`` () =
    let c = {
        name = "MyClass"
        variables = [
            (J_Static, J_Boolean, "foo")
        ]
        subroutines = [] 
    }
    let symbols = buildSymbolsForClass c
    let table = symbolStateToTable symbols
    let var = symbolLookup table "foo"
    match var with
    | Some v ->
        Assert.Equal("foo", v.name)
        Assert.Equal(ClassScope, v.scope)
        Assert.Equal(J_Boolean, v.varType)
        Assert.Equal(Static 0, v.segment)
    | None -> Assert.Fail("Variable was not in symbol table")

[<Fact>]
let ``Should build symbol table for class, simple class 1 field variable`` () =
    let c = {
        name = "MyClass"
        variables = [
            (J_Field, J_Boolean, "foo")
        ]
        subroutines = [] 
    }
    let symbols = buildSymbolsForClass c
    let table = symbolStateToTable symbols
    let var = symbolLookup table "foo"
    match var with
    | Some v ->
        Assert.Equal("foo", v.name)
        Assert.Equal(ClassScope, v.scope)
        Assert.Equal(J_Boolean, v.varType)
        Assert.Equal(This 0, v.segment)
    | None -> Assert.Fail("Variable was not in symbol table")
   
[<Fact>]
let ``Should build symbol table for class, incrementing static segment`` () =
    let c = {
        name = "MyClass"
        variables = [
            (J_Static, J_Boolean, "foo1")
            (J_Static, J_Boolean, "foo2")
            (J_Static, J_Boolean, "foo3")
            (J_Static, J_Boolean, "foo4")
        ]
        subroutines = [] 
    }
    let symbols = buildSymbolsForClass c
    let table = symbolStateToTable symbols
    let test name index = 
        let var = symbolLookup table name
        match var with
        | Some v ->
            Assert.Equal(name, v.name)
            Assert.Equal(ClassScope, v.scope)
            Assert.Equal(J_Boolean, v.varType)
            Assert.Equal(Static index, v.segment)
        | None -> Assert.Fail("Variable was not in symbol table")
    test "foo1" 0
    test "foo2" 1
    test "foo3" 2
    test "foo4" 3

[<Fact>]
let ``Should build symbol table for class, incrementing static and this segments`` () =
    let c = {
        name = "MyClass"
        variables = [
            (J_Static, J_Boolean, "foo1")
            (J_Field, J_Boolean, "foo2")
            (J_Static, J_Boolean, "foo3")
            (J_Field, J_Boolean, "foo4")
        ]
        subroutines = [] 
    }
    let symbols = buildSymbolsForClass c
    let table = symbolStateToTable symbols
    let test name seg = 
        let var = symbolLookup table name
        match var with
        | Some v ->
            Assert.Equal(name, v.name)
            Assert.Equal(ClassScope, v.scope)
            Assert.Equal(J_Boolean, v.varType)
            Assert.Equal(seg, v.segment)
        | None -> Assert.Fail("Variable was not in symbol table")
    test "foo1" (Static 0)
    test "foo2" (This 0)
    test "foo3" (Static 1)
    test "foo4" (This 1)
   
[<Fact>]
let ``Should use subroutine scoped variable over class scoped variable when names collide`` () =
    let c = {
        name = "MyClass"
        variables = [
            (J_Static, J_Int, "x")
        ]
        subroutines = [
            {
                name = "main"
                subType = J_Method
                returnType = J_Void
                parameters = []
                variables = [
                    (J_Local, J_Int, "x")
                ]
                body = []
            }
        ] 
    }
    let table = buildSymbolsForSubroutine (buildSymbolsForClass c) "MyClass" c.subroutines[0]
    let var = symbolLookup table "x"
    match var with
    | Some v ->
        Assert.Equal("x", v.name)
        Assert.Equal(SubroutineScope, v.scope)
        Assert.Equal(J_Int, v.varType)
        Assert.Equal(Local 0, v.segment)
    | None -> Assert.Fail("Variable was not in symbol table")

[<Fact>]
let ``Should include implicit this as first parameter to subroutine`` () =
    let c = {
        name = "MyClass"
        variables = []
        subroutines = [
            {
                name = "main"
                subType = J_Method
                returnType = J_Void
                parameters = [(J_Argument, J_Int, "x")]
                variables = []
                body = []
            }
        ] 
    }
    let table = buildSymbolsForSubroutine (buildSymbolsForClass c) "MyClass" c.subroutines[0]
    match symbolLookup table "this" with
    | Some v ->
        Assert.Equal("this", v.name)
        Assert.Equal(SubroutineScope, v.scope)
        Assert.Equal(J_Class "MyClass", v.varType)
        Assert.Equal(Argument 0, v.segment)
    | None -> Assert.Fail("Implicit 'this' was not in symbol table")
    match symbolLookup table "x" with
    | Some v ->
        Assert.Equal("x", v.name)
        Assert.Equal(SubroutineScope, v.scope)
        Assert.Equal(J_Int, v.varType)
        Assert.Equal(Argument 1, v.segment)
    | None -> Assert.Fail("Variable 'x' was not in symbol table")
    
[<Fact>]
let ``Should not include implicit this as first parameter to constructor`` () =
    let c = {
        name = "MyClass"
        variables = []
        subroutines = [
            {
                name = "main"
                subType = J_Constructor
                returnType = J_ReturnType (J_Class "MyClass")
                parameters = [(J_Argument, J_Int, "x")]
                variables = []
                body = []
            }
        ] 
    }
    let table = buildSymbolsForStaticSubroutine (buildSymbolsForClass c) c.subroutines[0]
    match symbolLookup table "this" with
    | Some _ -> Assert.Fail("Constructor should not have implicit 'this' parameter")
    | None -> Assert.True(true)
    match symbolLookup table "x" with
    | Some v ->
        Assert.Equal("x", v.name)
        Assert.Equal(SubroutineScope, v.scope)
        Assert.Equal(J_Int, v.varType)
        Assert.Equal(Argument 0, v.segment)
    | None -> Assert.Fail("Variable 'x' was not in symbol table")
  