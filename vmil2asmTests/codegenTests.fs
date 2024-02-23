module vmil2asmTests.codegenTests

open Xunit
open Faqt
open Faqt.Operators
open System.Text
open assembler.api
open vmil2asm.api
open hackemu

let fold (asm:string list) = (StringBuilder(), asm) ||> List.fold (_.AppendLine) |> (_.ToString())

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
    let input = fold asm 
    let code = assemble input
    let vm = HackVirtualMachine(code.instructions)
    vm.ComputeCycles(10000)
    Assert.Equal(13s, vm.Memory(261us))
    Assert.Equal(262s, vm.SP)

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
    let input = fold asm 
    let code = assemble input
    let vm = HackVirtualMachine(code.instructions)
    vm.SetMemory(0us, 261s)
    vm.SetMemory(1us, 261s)
    vm.SetMemory(2us, 256s)
    vm.SetMemory(3us, -3s)
    vm.SetMemory(4us, -4s)
    vm.SetMemory(5us, -1s)
    vm.SetMemory(6us, -1s)
    vm.SetMemory(256us, 1234s)
    vm.SetMemory(257us, -1s)
    vm.SetMemory(258us, -2s)
    vm.SetMemory(259us, -3s)
    vm.SetMemory(260us, -4s)
    for i in 261us .. 299us do
        vm.SetMemory(i, -1s)
        
    vm.ComputeCycles(10_000)
    %vm.SP.Should().Be(261s)
    %vm.LCL.Should().Be(261s)
    %vm.ARG.Should().Be(256s)
    %vm.THIS.Should().Be(4000s)
    %vm.THAT.Should().Be(5000s)
    %vm.R5.Should().Be(135s)
    %vm.R6.Should().Be(246s)

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
    let input = fold asm
    let code = assemble input
    let vm = HackVirtualMachine(code.instructions)
    vm.ComputeCycles(10_000)
    Assert.Equal(1196s, vm.Memory(261us))



[<Fact>]
let ``Should run simple-function program on virtual machine (without VM init code)`` () =
    let vmil = """
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/08/FunctionCalls/SimpleFunction/SimpleFunction.vm

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
    let asm = vmil2asmStringWithoutInitCode "Main.vm" vmil
    let input = fold asm
    let code = assemble input
    let vm = HackVirtualMachine(code.instructions)
    vm.SetMemory(0us, 317s)
    vm.SetMemory(1us, 317s)
    vm.SetArgumentSegmentBase(310s)
    vm.SetThisSegmentBase(3000s)
    vm.SetThatSegmentBase(4000s)
    vm.SetArgumentSegment(0s, 1234s)
    vm.SetArgumentSegment(1s, 37s)
    vm.SetArgumentSegment(2s, 1000s)
    vm.SetArgumentSegment(3s, 305s)
    vm.SetArgumentSegment(4s, 300s)
    vm.SetArgumentSegment(5s, 3010s)
    vm.SetArgumentSegment(6s, 4010s)
    vm.ComputeCycles(1_000)
    %vm.SP.Should().Be(311s)
    %vm.LCL.Should().Be(305s)
    %vm.ARG.Should().Be(300s)
    %vm.THIS.Should().Be(3010s)
    %vm.THAT.Should().Be(4010s)
    %vm.Memory(310us).Should().Be(1196s)






let staticsTestClass1vm = """
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

let staticsTestClass2vm = """
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

let staticsTestSysvm = """
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
        {name = "Class1.vm"; input = staticsTestClass1vm }
        {name = "Class2.vm"; input = staticsTestClass2vm }
        {name = "Sys.vm"; input = staticsTestSysvm }
    ]
    let input = fold asm 
    let code = assemble input
    let vm = HackVirtualMachine(code.instructions)
    vm.ComputeCycles(10_000)
    %vm.SP.Should().Be(263s)
    %vm.Stack(0s).Should().Be(8s)
    %vm.Stack(1s).Should().Be(-2s)

[<Fact>]
let ``Should test temp segment - writing`` () =
    let vmilCode = """
function Sys.init 0
	push constant 8
	push constant 7
	push constant 6
	push constant 5
	push constant 4
	push constant 3
	push constant 2
	push constant 1
	pop temp 0
	pop temp 1
	pop temp 2
	pop temp 3
	pop temp 4
	pop temp 5
	pop temp 6
	pop temp 7
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.ComputeCycles(1_000)
    %vm.R5.Should().Be(1s, "segment offset 0")
    %vm.R6.Should().Be(2s, "segment offset 1")
    %vm.R7.Should().Be(3s, "segment offset 2")
    %vm.R8.Should().Be(4s, "segment offset 3")
    %vm.R9.Should().Be(5s, "segment offset 4")
    %vm.R10.Should().Be(6s, "segment offset 5")
    %vm.R11.Should().Be(7s, "segment offset 6")
    %vm.R12.Should().Be(8s, "segment offset 7")
    %vm.SP.Should().Be(261s, "stack pointer should be expected value")

[<Fact>]
let ``Should test temp segment - reading`` () =
    let vmilCode = """
function Sys.init 0
	push temp 0
	push temp 1
	push temp 2
	push temp 3
	push temp 4
	push temp 5
	push temp 6
	push temp 7
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.SetMemory(5us, 805s)
    vm.SetMemory(6us, 806s)
    vm.SetMemory(7us, 807s)
    vm.SetMemory(8us, 808s)
    vm.SetMemory(9us, 809s)
    vm.SetMemory(10us, 810s)
    vm.SetMemory(11us, 811s)
    vm.SetMemory(12us, 812s)
    vm.ComputeCycles(1_000)
    %vm.TopOfStack.Should().Be(812s)
    %vm.Stack(1s).Should().Be(811s, "2nd on the stack")
    %vm.Stack(2s).Should().Be(810s, "3rd on the stack")
    %vm.Stack(3s).Should().Be(809s, "4th on the stack")
    %vm.Stack(4s).Should().Be(808s, "5th on the stack")
    %vm.Stack(5s).Should().Be(807s, "6th on the stack")
    %vm.Stack(6s).Should().Be(806s, "7th on the stack")
    %vm.Stack(7s).Should().Be(805s, "8th on the stack")
    %vm.SP.Should().Be(269s, "stack pointer should be expected value")
        
[<Fact>]
let ``Should test local segment`` () =
    let vmilCode = """
function Sys.init 8
	push constant 800
	pop local 0
	push local 0
	push constant 801
	pop local 1
	push local 1
	push constant 802
	pop local 2
	push local 2
	push constant 803
	pop local 3
	push local 3
	push constant 804
	pop local 4
	push local 4
	push constant 805
	pop local 5
	push local 5
	push constant 806
	pop local 6
	push local 6
	push constant 807
	pop local 7
	push local 7
label END
goto END 
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.ComputeCycles(1_000)
    %vm.TopOfStack.Should().Be(807s)
    %vm.Stack(1s).Should().Be(806s, "2nd on the stack")
    %vm.Stack(2s).Should().Be(805s, "3rd on the stack")
    %vm.Stack(3s).Should().Be(804s, "4th on the stack")
    %vm.Stack(4s).Should().Be(803s, "5th on the stack")
    %vm.Stack(5s).Should().Be(802s, "6th on the stack")
    %vm.Stack(6s).Should().Be(801s, "7th on the stack")
    %vm.Stack(7s).Should().Be(800s, "8th on the stack")
    %vm.LocalSegment(0s).Should().Be(800s, "segment offset 0")
    %vm.LocalSegment(1s).Should().Be(801s, "segment offset 0")
    %vm.LocalSegment(2s).Should().Be(802s, "segment offset 0")
    %vm.LocalSegment(3s).Should().Be(803s, "segment offset 0")
    %vm.LocalSegment(4s).Should().Be(804s, "segment offset 0")
    %vm.LocalSegment(5s).Should().Be(805s, "segment offset 0")
    %vm.LocalSegment(6s).Should().Be(806s, "segment offset 0")
    %vm.LocalSegment(7s).Should().Be(807s, "segment offset 0")
    %vm.SP.Should().Be(277s, "stack pointer should be expected value")

[<Fact>]
let ``Should test argument segment`` () =
    let vmilCode = """
function Sys.init 8
	push constant 800
	pop argument 0
	push argument 0
	push constant 801
	pop argument 1
	push argument 1
	push constant 802
	pop argument 2
	push argument 2
	push constant 803
	pop argument 3
	push argument 3
	push constant 804
 	pop argument 4
	push argument 4
	push constant 805
	pop argument 5
	push argument 5
	push constant 806
 	pop argument 6
	push argument 6
	push constant 807
 	pop argument 7
	push argument 7
label END
goto END 
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.ComputeCycles(1_000)
    %vm.ArgumentSegment(0s).Should().Be(800s, "segment offset 0")
    %vm.ArgumentSegment(1s).Should().Be(801s, "segment offset 1")
    %vm.ArgumentSegment(2s).Should().Be(802s, "segment offset 2")
    %vm.ArgumentSegment(3s).Should().Be(803s, "segment offset 3")
    %vm.ArgumentSegment(4s).Should().Be(804s, "segment offset 4")
    %vm.ArgumentSegment(5s).Should().Be(805s, "segment offset 5")
    %vm.ArgumentSegment(6s).Should().Be(806s, "segment offset 6")
    %vm.ArgumentSegment(7s).Should().Be(807s, "segment offset 7")
    %vm.SP.Should().Be(277s, "stack pointer should be expected value")

[<Fact>]
let ``Should test THIS segment - writing`` () =
    let vmilCode = """
function Sys.init 0
	push constant 8
	push constant 7
	push constant 6
	push constant 5
	push constant 4
	push constant 3
	push constant 2
	push constant 1
	pop this 0
	pop this 1
	pop this 2
	pop this 3
	pop this 4
	pop this 5
	pop this 6
	pop this 7
label END
goto END
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.SetThisSegmentBase(1000s)
    vm.ComputeCycles(1_000)
    %vm.ThisSegment(0s).Should().Be(1s, "segment offset 0")
    %vm.ThisSegment(1s).Should().Be(2s, "segment offset 1")
    %vm.ThisSegment(2s).Should().Be(3s, "segment offset 2")
    %vm.ThisSegment(3s).Should().Be(4s, "segment offset 3")
    %vm.ThisSegment(4s).Should().Be(5s, "segment offset 4")
    %vm.ThisSegment(5s).Should().Be(6s, "segment offset 5")
    %vm.ThisSegment(6s).Should().Be(7s, "segment offset 6")
    %vm.ThisSegment(7s).Should().Be(8s, "segment offset 7")
    %vm.SP.Should().Be(261s, "stack pointer should be expected value")

[<Fact>]
let ``Should test THIS segment - reading`` () =
    let vmilCode = """
function Sys.init 0
	push this 0
	push this 1
	push this 2
	push this 3
	push this 4
	push this 5
	push this 6
	push this 7
label END
goto END 
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.SetThisSegmentBase(1000s)
    vm.SetMemory(1000us, 800s)
    vm.SetMemory(1001us, 801s)
    vm.SetMemory(1002us, 802s)
    vm.SetMemory(1003us, 803s)
    vm.SetMemory(1004us, 804s)
    vm.SetMemory(1005us, 805s)
    vm.SetMemory(1006us, 806s)
    vm.SetMemory(1007us, 807s)
    vm.ComputeCycles(1_000)
    %vm.TopOfStack.Should().Be(807s)
    %vm.Stack(1s).Should().Be(806s, "2nd on the stack")
    %vm.Stack(2s).Should().Be(805s, "3rd on the stack")
    %vm.Stack(3s).Should().Be(804s, "4th on the stack")
    %vm.Stack(4s).Should().Be(803s, "5th on the stack")
    %vm.Stack(5s).Should().Be(802s, "6th on the stack")
    %vm.Stack(6s).Should().Be(801s, "7th on the stack")
    %vm.Stack(7s).Should().Be(800s, "8th on the stack")
    %vm.SP.Should().Be(269s, "stack pointer should be expected value")

[<Fact>]
let ``Should test THAT segment - writing`` () =
    let vmilCode = """
function Sys.init 0
	push constant 8
	push constant 7
	push constant 6
	push constant 5
	push constant 4
	push constant 3
	push constant 2
	push constant 1
	pop that 0
	pop that 1
	pop that 2
	pop that 3
	pop that 4
	pop that 5
	pop that 6
	pop that 7
label END
goto END 
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.SetThatSegmentBase(1000s)
    vm.ComputeCycles(1_000)
    %vm.ThatSegment(0s).Should().Be(1s, "segment offset 0")
    %vm.ThatSegment(1s).Should().Be(2s, "segment offset 1")
    %vm.ThatSegment(2s).Should().Be(3s, "segment offset 2")
    %vm.ThatSegment(3s).Should().Be(4s, "segment offset 3")
    %vm.ThatSegment(4s).Should().Be(5s, "segment offset 4")
    %vm.ThatSegment(5s).Should().Be(6s, "segment offset 5")
    %vm.ThatSegment(6s).Should().Be(7s, "segment offset 6")
    %vm.ThatSegment(7s).Should().Be(8s, "segment offset 7")
    %vm.SP.Should().Be(261s, "stack pointer should be expected value")

[<Fact>]
let ``Should test THAT segment - reading`` () =
    let vmilCode = """
function Sys.init 0
	push that 0
	push that 1
	push that 2
	push that 3
	push that 4
	push that 5
	push that 6
	push that 7
label END
goto END 
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.SetThatSegmentBase(1000s)
    vm.SetMemory(1000us, 800s)
    vm.SetMemory(1001us, 801s)
    vm.SetMemory(1002us, 802s)
    vm.SetMemory(1003us, 803s)
    vm.SetMemory(1004us, 804s)
    vm.SetMemory(1005us, 805s)
    vm.SetMemory(1006us, 806s)
    vm.SetMemory(1007us, 807s)
    vm.ComputeCycles(1_000)
    %vm.TopOfStack.Should().Be(807s)
    %vm.Stack(1s).Should().Be(806s, "2nd on the stack")
    %vm.Stack(2s).Should().Be(805s, "3rd on the stack")
    %vm.Stack(3s).Should().Be(804s, "4th on the stack")
    %vm.Stack(4s).Should().Be(803s, "5th on the stack")
    %vm.Stack(5s).Should().Be(802s, "6th on the stack")
    %vm.Stack(6s).Should().Be(801s, "7th on the stack")
    %vm.Stack(7s).Should().Be(800s, "8th on the stack")
    %vm.SP.Should().Be(269s, "stack pointer should be expected value")
                
[<Fact>]
let ``Should test static segment - writing`` () =
    let vmilCode = """
function Sys.init 0
	push constant 8
	push constant 7
	push constant 6
	push constant 5
	push constant 4
	push constant 3
	push constant 2
	push constant 1
	pop static 0
	pop static 1
	pop static 2
	pop static 3
	pop static 4
	pop static 5
	pop static 6
	pop static 7
label END
goto END 
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.ComputeCycles(1_000)
    %vm.StaticSegment(0s).Should().Be(1s, "segment offset 0")
    %vm.StaticSegment(1s).Should().Be(2s, "segment offset 1")
    %vm.StaticSegment(2s).Should().Be(3s, "segment offset 2")
    %vm.StaticSegment(3s).Should().Be(4s, "segment offset 3")
    %vm.StaticSegment(4s).Should().Be(5s, "segment offset 4")
    %vm.StaticSegment(5s).Should().Be(6s, "segment offset 5")
    %vm.StaticSegment(6s).Should().Be(7s, "segment offset 6")
    %vm.StaticSegment(7s).Should().Be(8s, "segment offset 7")
    %vm.SP.Should().Be(261s, "stack pointer should be expected value")

[<Fact>]
let ``Should test static segment - reading`` () =
    let vmilCode = """
function Sys.init 0
	push static 0
	push static 1
	push static 2
	push static 3
	push static 4
	push static 5
	push static 6
	push static 7
label END
goto END 
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.SetStaticSegment(0s, 800s)
    vm.SetStaticSegment(1s, 801s)
    vm.SetStaticSegment(2s, 802s)
    vm.SetStaticSegment(3s, 803s)
    vm.SetStaticSegment(4s, 804s)
    vm.SetStaticSegment(5s, 805s)
    vm.SetStaticSegment(6s, 806s)
    vm.SetStaticSegment(7s, 807s)
    vm.ComputeCycles(1_000)
    %vm.TopOfStack.Should().Be(807s)
    %vm.Stack(1s).Should().Be(806s, "2nd on the stack")
    %vm.Stack(2s).Should().Be(805s, "3rd on the stack")
    %vm.Stack(3s).Should().Be(804s, "4th on the stack")
    %vm.Stack(4s).Should().Be(803s, "5th on the stack")
    %vm.Stack(5s).Should().Be(802s, "6th on the stack")
    %vm.Stack(6s).Should().Be(801s, "7th on the stack")
    %vm.Stack(7s).Should().Be(800s, "8th on the stack")
    %vm.SP.Should().Be(269s, "stack pointer should be expected value")
    
[<Fact>]
let ``Should test pointer 0 segment - writing`` () =
    let vmilCode = """
function Sys.init 0
	push constant 818
	pop pointer 0
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.SetThisBase(1000s)
    vm.ComputeCycles(1_000)
    %vm.THIS.Should().Be(818s, "pointer 0 should contain the value we set")
    %vm.SP.Should().Be(261s, "stack pointer should be expected value")
    
[<Fact>]
let ``Should test pointer 1 segment - writing`` () =
    let vmilCode = """
function Sys.init 0
	push constant 818
	pop pointer 1
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let input = fold asmCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.SetThatBase(1000s)
    vm.ComputeCycles(1_000)
    %vm.THAT.Should().Be(818s, "pointer 1 should contain the value we set")
    %vm.SP.Should().Be(261s, "stack pointer should be expected value")
    
[<Fact>]
let ``Should test pointer 0 segment - reading`` () =
    let vmilCode = """
function Sys.init 0
	push pointer 0
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.SetThisBase(1000s)
    vm.SetPointer0(818s)
    vm.ComputeCycles(1_000)
    %vm.Pointer_0.Should().Be(818s, "pointer 0 should contain the value we set")
    %vm.TopOfStack.Should().Be(1000s, "value from pointer 0 should be at the top of the stack")
    %vm.SP.Should().Be(262s, "stack pointer should be expected value")
   
[<Fact>]
let ``Should test pointer 1 segment - reading`` () =
    let vmilCode = """
function Sys.init 0
	push pointer 1
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.SetThatBase(1000s)
    vm.SetPointer1(818s)
    vm.ComputeCycles(1_000)
    %vm.Pointer_1.Should().Be(818s, "pointer 1 should contain the value we set")
    %vm.TopOfStack.Should().Be(1000s, "value from pointer 1 should be at the top of the stack")
    %vm.SP.Should().Be(262s, "stack pointer should be expected value")
        
[<Fact>]
let ``Should set address of THIS segment then write a value to that segment`` () =
    let vmilCode = """
function Sys.init 0
	push constant 4001
	pop pointer 0
	push constant 5001
	pop this 0
 label END
 goto END
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.ComputeCycles(1_000)
    %vm.THIS.Should().Be(4001s)
    %vm.Pointer_0.Should().Be(5001s)

[<Fact>]
let ``Should set address of THIS segment then write a value to that segment then read it back onto the stack`` () =
    let vmilCode = """
function Sys.init 0
	push constant 4001
	pop pointer 0
	push constant 5001
	pop this 0
	push this 0
 label END
 goto END
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.ComputeCycles(1_000)
    %vm.THIS.Should().Be(4001s)
    %vm.Pointer_0.Should().Be(5001s)
    %vm.TopOfStack.Should().Be(5001s)
       
[<Fact>]
let ``Should set address of THAT segment then write a value to that segment`` () =
    let vmilCode = """
function Sys.init 0
	push constant 4001
	pop pointer 1
	push constant 5001
	pop that 0
 label END
 goto END
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.ComputeCycles(1_000)
    %vm.THAT.Should().Be(4001s)
    %vm.Pointer_1.Should().Be(5001s)
    
[<Fact>]
let ``Should set address of THAT segment then write a value to that segment then read it back onto the stack`` () =
    let vmilCode = """
function Sys.init 0
	push constant 4001
	pop pointer 1
	push constant 5001
	pop that 0
	push that 0
 label END
 goto END
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let binaryCode = assemble (fold asmCode)
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.ComputeCycles(1_000)
    %vm.THAT.Should().Be(4001s)
    %vm.Pointer_1.Should().Be(5001s)
    %vm.TopOfStack.Should().Be(5001s)
    
[<Fact>]
let ``Should run program with conditional branching`` () =
    let vmilCode = """
function Main.main 1
	push constant 1
	call Main.test1 1
	pop local 0
	push constant 0
	call Main.test1 1
	pop local 0
	push constant 0
	return
function Main.test1 0
	push argument 0
	push constant 1
	eq
	if-goto Main.test1.IF_ELSE_TRUE$1
	goto Main.test1.IF_ELSE_FALSE$2
label Main.test1.IF_ELSE_TRUE$1
	push constant 567
	return
	goto Main.test1.IF_ELSE_END$3
label Main.test1.IF_ELSE_FALSE$2
	push constant 123
	return
label Main.test1.IF_ELSE_END$3
"""
    let asmCode = vmil2asmString "foo.vm" vmilCode
    let input = fold asmCode
    let binaryCode = assemble input
    let vm = HackVirtualMachine(binaryCode.instructions)
    vm.ComputeCycles(1_000)
    %vm.THAT.Should().Be(4001s)
    %vm.Pointer_1.Should().Be(5001s)
    %vm.TopOfStack.Should().Be(5001s)