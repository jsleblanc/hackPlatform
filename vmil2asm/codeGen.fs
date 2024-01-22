module vmil2asm.codeGen

open vmil2asm.types

let ai = AssemblyInstruction
let aComment s = ai $"// --- {s} ---"
let bComment s = ai $"//  {s}"
let incrementStackPointer =
    [
        bComment "INC SP"
        ai "@SP"
        ai "M=M+1"
    ]
    
let decrementStackPointer =
    [
        bComment "DEC SP"
        ai "@SP"
        ai "M=M-1"
    ]

let loadCurrentStackValueInto_D_Reg =
    [
        bComment "LOAD @SP INTO D-REG"
        ai "@SP" //address of stack pointer into A-reg
        ai "A=M" //read current stack pointer value, store in A-reg
        ai "D=M" //read memory at address in A-reg, store in D-reg
    ]

let write_D_RegToCurrentStackPointer =
    [
        bComment "WRITE D-REG INTO @SP"
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


let add =
    [aComment "ADD"]
    @ decrementStackPointer
    @ loadCurrentStackValueInto_D_Reg
    @ decrementStackPointer
    @ [
        bComment "ADD LOGIC"
        ai "@SP"
        ai "A=M"
        ai "D=D+M"
    ] @ write_D_RegToCurrentStackPointer
    @ incrementStackPointer

let codeGenArithmetic cmd =
    match cmd with
    | ADD -> add
    
let codeGenInstruction cmd =
    match cmd with
    | Arithmetic a -> codeGenArithmetic a
    | PUSH (Constant, SegmentIndex value) ->
        [
            aComment "PUSH CONSTANT"
            ai $"@{value}" //load constant into A-reg
            ai "D=A" //Copy A-reg into D-reg
            ai "@SP" //load stack-pointer address into A-reg
            ai "A=M" //read stack pointer from memory address in A-reg and store in A-reg
            ai "M=D" //write value in D-reg into memory addressed by A-reg
        ] @ incrementStackPointer
       
let codeGenInstructions cmds =
    cmds
    |> List.map codeGenInstruction
    |> List.collect id