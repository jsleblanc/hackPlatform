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
    let asm = processString vmil
    let input = (StringBuilder(), asm) ||> List.fold (fun sb str -> sb.AppendFormat("{0}\n", str))
    let code = assemble (input.ToString())
    let vm = HackVirtualMachine(HackComputer(code.instructions))
    vm.ComputeCycles(10000)
    Assert.Equal(int16 13, vm.Memory(int16 261))
    Assert.Equal(int16 262, vm.SP)
    