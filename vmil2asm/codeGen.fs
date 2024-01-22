module vmil2asm.codeGen

open vmil2asm.types

let ai = AssemblyInstruction

let incrementStackPointer =
    [
        ai "@SP"
        ai "M=M+1"
    ]
    
let decrementStackPointer =
    [
        ai "@SP"
        ai "M=M-1"
    ]

let loadCurrentStackValueInto_D_Reg =
    [
        ai "@SP" //address of stack pointer into A-reg
        ai "A=M" //read current stack pointer value, store in A-reg
        ai "D=M" //read memory at address in A-reg, store in D-reg
    ]

let write_D_RegToCurrentStackPointer =
    [
        ai "@SP"
        ai "A=M"
        ai "M=D"
    ]

let segmentToSegmentPointer s =
    match s with
    | Argument -> ai "@ARG"
    | Local -> ai "@LCL"
    | Static -> failwith "todo"
    | Constant -> failwith "no segment pointer for \"constant\""
    | This -> ai "@THIS"
    | That -> ai "@THAT"
    | Pointer -> failwith "todo"
    | Temp -> failwith "todo"


(*
@SP
M=M-1

@SP
A=M
D=M

@SP
M=M-1

@SP
A=M
D=D+M
@SP
A=M
M=D

@SP
M=M+1
*)
let add =
    decrementStackPointer
    @ loadCurrentStackValueInto_D_Reg
    @ decrementStackPointer
    @ [
        ai "@SP"
        ai "A=M"
        ai "D=D+M"
    ] @ write_D_RegToCurrentStackPointer
    @ incrementStackPointer

    
let codeGen cmd =
    match cmd with
    | PUSH (Constant, value) ->
        [
            ai $"@{value}" //load constant into A-reg
            ai "D=A" //Copy A-reg into D-reg
            ai "@SP" //load stack-pointer address into A-reg
            ai "A=M" //read stack pointer from memory address in A-reg and store in A-reg
            ai "M=D" //write value in D-reg into memory addressed by A-reg
        ] @ incrementStackPointer
        


    
        