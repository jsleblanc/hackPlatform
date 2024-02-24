module vmil2asmTests.codeGenPropertyTests

open System
open FsCheck
open FsCheck.Xunit
open hackemu
open vmil2asm.api
open assembler.api
open vmil2asmTests.util

let positiveInt16 = Gen.elements [0..int Int16.MaxValue] |> Gen.map int16

let positiveInt16Pair = positiveInt16 |> Gen.two 

let negativeInt16 = Gen.elements [int Int16.MinValue..0] |> Gen.map int16

let positiveInt16UniquePair = positiveInt16Pair |> Gen.filter (fun (a,b) -> not (a = b))

[<Property>]
let ``Should test addition instruction`` () =
    positiveInt16Pair
    |> Arb.fromGen
    |> Prop.forAll <| fun (a, b) -> 
        let vmilCode = $"""
	    function Sys.init 0
		    push constant {a}
		    push constant {b}
		    add
		    label END
		    goto END
	    """
        let asmCode = vmil2asmString "foo.vm" vmilCode
        let input = fold asmCode
        let binaryCode = assemble input
        let vm = HackVirtualMachine(binaryCode.instructions)
        vm.ComputeCycles(2_00)
        vm.TopOfStack = int16 (a + b)
        

[<Property>]
let ``Should test negate instruction`` () =
    positiveInt16
    |> Arb.fromGen
    |> Prop.forAll <| fun a ->     
        let vmilCode = $"""
	function Sys.init 0
		push constant {a}
		neg
		label END
		goto END
	"""
        let asmCode = vmil2asmString "foo.vm" vmilCode
        let input = fold asmCode
        let binaryCode = assemble input
        let vm = HackVirtualMachine(binaryCode.instructions)
        vm.ComputeCycles(2_00)
        vm.TopOfStack = -a
    
[<Property>]
let ``Should test not instruction`` () =
    positiveInt16
    |> Arb.fromGen
    |> Prop.forAll <| fun a ->
        let vmilCode = $"""
     function Sys.init 0
          push constant {a}
          not
          label END
          goto END
       """
        let asmCode = vmil2asmString "foo.vm" vmilCode
        let input = fold asmCode
        let binaryCode = assemble input
        let vm = HackVirtualMachine(binaryCode.instructions)
        vm.ComputeCycles(2_00)
        vm.TopOfStack = ~~~a
    
[<Property>]
let ``Should test or instruction`` () =
    positiveInt16Pair
    |> Arb.fromGen
    |> Prop.forAll <| fun (a, b) ->     
        let vmilCode = $"""
    function Sys.init 0
        push constant {a}
        push constant {b}
        or
        label END
        goto END
        """
        let asmCode = vmil2asmString "foo.vm" vmilCode
        let input = fold asmCode
        let binaryCode = assemble input
        let vm = HackVirtualMachine(binaryCode.instructions)
        vm.ComputeCycles(2_00)
        vm.TopOfStack = (a ||| b)
    
[<Property>]
let ``Should test and instruction`` () =
    positiveInt16Pair
    |> Arb.fromGen
    |> Prop.forAll <| fun (a, b) ->     
        let vmilCode = $"""
    function Sys.init 0
        push constant {a}
        push constant {b}
        and
        label END
        goto END
        """
        let asmCode = vmil2asmString "foo.vm" vmilCode
        let input = fold asmCode
        let binaryCode = assemble input
        let vm = HackVirtualMachine(binaryCode.instructions)
        vm.ComputeCycles(2_00)
        vm.TopOfStack = (a &&& b)
            
[<Property>]
let ``Should test subtraction instruction`` () =
    positiveInt16Pair
    |> Arb.fromGen
    |> Prop.forAll <| fun (a, b) ->     
        let vmilCode = $"""
    function Sys.init 0
        push constant {a}
        push constant {b}
        sub
        label END
        goto END
        """
        let asmCode = vmil2asmString "foo.vm" vmilCode
        let input = fold asmCode
        let binaryCode = assemble input
        let vm = HackVirtualMachine(binaryCode.instructions)
        vm.ComputeCycles(2_00)
        vm.TopOfStack = (a - b)
        
[<Property>]
let ``Should test equality (always equal) instruction`` () =
    positiveInt16
    |> Arb.fromGen
    |> Prop.forAll <| fun a ->     
        let vmilCode = $"""
    function Sys.init 0
        push constant {a}
        push constant {a}
        eq
        label END
        goto END
        """
        let asmCode = vmil2asmString "foo.vm" vmilCode
        let input = fold asmCode
        let binaryCode = assemble input
        let vm = HackVirtualMachine(binaryCode.instructions)
        vm.ComputeCycles(2_00)
        vm.TopOfStack = -1s
        
[<Property>]
let ``Should test equality (never equal) instruction`` () =
    positiveInt16UniquePair
    |> Arb.fromGen
    |> Prop.forAll <| fun (a, b) ->
        let vmilCode = $"""
    function Sys.init 0
        push constant {a}
        push constant {b}
        eq
        label END
        goto END
        """
        let asmCode = vmil2asmString "foo.vm" vmilCode
        let input = fold asmCode
        let binaryCode = assemble input
        let vm = HackVirtualMachine(binaryCode.instructions)
        vm.ComputeCycles(2_00)
        vm.TopOfStack = 0s

[<Property>]
let ``Should test less-than instruction`` () =
    positiveInt16Pair
    |> Arb.fromGen
    |> Prop.forAll <| fun (a, b) ->
        let vmilCode = $"""
    function Sys.init 0
        push constant {a}
        push constant {b}
        lt
        label END
        goto END
        """
        let asmCode = vmil2asmString "foo.vm" vmilCode
        let input = fold asmCode
        let binaryCode = assemble input
        let vm = HackVirtualMachine(binaryCode.instructions)
        vm.ComputeCycles(2_00)
        let expected = if (a < b) then -1s else 0s
        vm.TopOfStack = expected
        
[<Property>]
let ``Should test greater-than instruction`` () =
    positiveInt16Pair
    |> Arb.fromGen
    |> Prop.forAll <| fun (a, b) ->
        let vmilCode = $"""
    function Sys.init 0
        push constant {a}
        push constant {b}
        gt
        label END
        goto END
        """
        let asmCode = vmil2asmString "foo.vm" vmilCode
        let input = fold asmCode
        let binaryCode = assemble input
        let vm = HackVirtualMachine(binaryCode.instructions)
        vm.ComputeCycles(2_00)
        let expected = if (a > b) then -1s else 0s
        vm.TopOfStack = expected        