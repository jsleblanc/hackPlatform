module vmil2asmTests.codegenTests

open Xunit
open System.Text
open assembler.api
open vmil2asm.api
open hackemu

[<Fact>]
let ``Should run fibonacci program on virtual machine`` () =
    let vmil = """
function Main.fibonacci 0
	push argument 0
	push constant 2
	lt                     
	if-goto N_LT_2        
	goto N_GE_2
label N_LT_2               // if n < 2 returns n
	push argument 0        
	return
label N_GE_2               // if n >= 2 returns fib(n - 2) + fib(n - 1)
	push argument 0
	push constant 2
	sub
	call Main.fibonacci 1  // computes fib(n - 2)
	push argument 0
	push constant 1
	sub
	call Main.fibonacci 1  // computes fib(n - 1)
	add                    // returns fib(n - 1) + fib(n - 2)
	return

function Sys.init 0
	push constant 7
	call Main.fibonacci 1   // computes the 7'th fibonacci element
label END  
	goto END                // loops infinitely
"""
    let asm = vmil2asmString "Main.vm" vmil
    let input = (StringBuilder(), asm) ||> List.fold (fun sb str -> sb.AppendFormat("{0}\n", str))
    let code = assemble (input.ToString())
    let vm = HackVirtualMachine(HackComputer(code.instructions))
    vm.ComputeCycles(10000)
    Assert.Equal(int16 13, vm.Memory(int16 261))
    Assert.Equal(int16 262, vm.SP)

[<Fact>]
let ``Should run nested-call program on virtual machine`` () =
    let vmil = """
// Sys.vm. Tested by the NestedCall test script.
// Consists of three functions: Sys.init, Sys.main, and Sys.add12.

// Calls Sys.main() and stores a return value in temp 1.
// Does not return (enters infinite loop).
// The VM implementation starts running this function, by default.
function Sys.init 0
	push constant 4000	// tests that THIS and THAT are handled correctly
	pop pointer 0
	push constant 5000
	pop pointer 1
	call Sys.main 0
	pop temp 1
	label LOOP
	goto LOOP

// Sets locals 1, 2 and 3 to some values. Leaves locals 0 and 4 unchanged, 
// to test that the 'function' VM command initliazes them to 0 (the test 
// script sets them to -1 before this code starts running).
// Calls Sys.add12(123) and stores the return value (should be 135) in temp 0.
// Returns local 0 + local 1 + local 2 + local 3 + local 4 (should be 456), to 
// confirm that locals were not mangled by the function call.
function Sys.main 5
	push constant 4001
	pop pointer 0
	push constant 5001
	pop pointer 1
	push constant 200
	pop local 1
	push constant 40
	pop local 2
	push constant 6
	pop local 3
	push constant 123
	call Sys.add12 1
	pop temp 0
	push local 0
	push local 1
	push local 2
	push local 3
	push local 4
	add
	add
	add
	add
	return

// Returns (argument 0) + 12.
function Sys.add12 0
	push constant 4002
	pop pointer 0
	push constant 5002
	pop pointer 1
	push argument 0
	push constant 12
	add
	return
"""
    let asm = vmil2asmString "Main.vm" vmil
    let input = (StringBuilder(), asm) ||> List.fold (fun sb str -> sb.AppendFormat("{0}\n", str))
    let code = assemble (input.ToString())
    let vm = HackVirtualMachine(HackComputer(code.instructions))
    vm.ComputeCycles(10000)
    Assert.Equal(int16 261, vm.SP)
    Assert.Equal(int16 261, vm.LCL)
    Assert.Equal(int16 256, vm.ARG)
    Assert.Equal(int16 4000, vm.THIS)
    Assert.Equal(int16 5000, vm.THAT)
    Assert.Equal(int16 135, vm.R5)
    Assert.Equal(int16 246, vm.R6)

[<Fact>]
let ``Should run simple-function program on virtual machine`` () =
    let vmil = """
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/08/FunctionCalls/SimpleFunction/SimpleFunction.vm

function Sys.init 0
	push constant 1234
	push constant 37
	call SimpleFunction.test 2
	label LOOP
	goto LOOP

// Performs a simple calculation and returns the result.
// argument[0] and argument[1] must be set by the caller of this code.
function SimpleFunction.test 2
	push local 0
	push local 1
	add
	not
	push argument 0
	add
	push argument 1
	sub
	return
"""
    let asm = vmil2asmString "Main.vm" vmil
    let input = (StringBuilder(), asm) ||> List.fold (fun sb str -> sb.AppendFormat("{0}\n", str))
    let code = assemble (input.ToString())
    let vm = HackVirtualMachine(HackComputer(code.instructions))
    vm.ComputeCycles(10000)
    Assert.Equal(int16 1196, vm.Memory(int16 261))








let class1vm = """
// Stores two supplied arguments in static[0] and static[1].
function Class1.set 0
	push argument 0
	pop static 0
	push argument 1
	pop static 1
	push constant 0
	return

// Returns static[0] - static[1].
function Class1.get 0
	push static 0
	push static 1
	sub
	return
"""

let class2vm = """
// Stores two supplied arguments in static[0] and static[1].
function Class2.set 0
	push argument 0
	pop static 0
	push argument 1
	pop static 1
	push constant 0
	return

// Returns static[0] - static[1].
function Class2.get 0
	push static 0
	push static 1
	sub
	return
"""

let sysvm = """
// Tests that different functions, stored in two different 
// class files, manipulate the static segment correctly. 
function Sys.init 0
	push constant 6
	push constant 8
	call Class1.set 2
	pop temp 0 // dumps the return value
	push constant 23
	push constant 15
	call Class2.set 2
	pop temp 0 // dumps the return value
	call Class1.get 0
	call Class2.get 0
label END
	goto END
"""

[<Fact>]
let ``Should run statics-test program on virtual machine`` () =    
    let asm = vmil2asmStrings [
        {name = "Class1.vm"; input = class1vm }
        {name = "Class2.vm"; input = class2vm }
        {name = "Sys.vm"; input = sysvm }
    ]
    let input = (StringBuilder(), asm) ||> List.fold (fun sb str -> sb.AppendFormat("{0}\n", str))
    let code = assemble (input.ToString())
    let vm = HackVirtualMachine(HackComputer(code.instructions))
    vm.ComputeCycles(10000)
    Assert.Equal(int16 263, vm.SP)
    Assert.Equal(int16 -2, vm.Memory(int16 261))
    Assert.Equal(int16 8, vm.Memory(int16 262))
