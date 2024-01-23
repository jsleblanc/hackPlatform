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

let popStackIntoD =
    [
        bComment "POP STACK INTO D-Reg"
        ai "@SP"
        ai "M=M-1"
        ai "@SP"
        ai "A=M"
        ai "D=M"
    ]

let popStackIntoR2 =
    [
        bComment "POP STACK INTO @R2 (via D-Reg)"
        ai "@SP"
        ai "M=M-1"
        ai "@SP"
        ai "A=M"
        ai "D=M"
        ai "@R2"
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


let aAdd =
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

let aSub =
    [aComment "SUB"]
    @ decrementStackPointer
    @ loadCurrentStackValueInto_D_Reg
    @ decrementStackPointer
    @ [
        bComment "SUB LOGIC"
        ai "@SP"
        ai "A=M"
        ai "D=D-M"
    ] @ write_D_RegToCurrentStackPointer
    @ incrementStackPointer

let aAnd =
    [aComment "AND"]
    @ decrementStackPointer
    @ loadCurrentStackValueInto_D_Reg
    @ decrementStackPointer
    @ [
        bComment "AND LOGIC"
        ai "@SP"
        ai "A=M"
        ai "D=D&M"
    ] @ write_D_RegToCurrentStackPointer
    @ incrementStackPointer
    
let aOr =
    [aComment "OR"]
    @ decrementStackPointer
    @ loadCurrentStackValueInto_D_Reg
    @ decrementStackPointer
    @ [
        bComment "OR LOGIC"
        ai "@SP"
        ai "A=M"
        ai "D=D|M"
    ] @ write_D_RegToCurrentStackPointer
    @ incrementStackPointer

let aNot =
    [aComment "NOT"]
    @ decrementStackPointer
    @ [
        bComment "NOT LOGIC"
        ai "@SP"
        ai "A=M"
        ai "M=!M"
    ] @ incrementStackPointer

let aNeg =
    [aComment "NEG"]
    @ decrementStackPointer
    @ loadCurrentStackValueInto_D_Reg
    @ [
        bComment "NEG LOGIC"
        ai "D=-D"
    ] @ write_D_RegToCurrentStackPointer
    @ incrementStackPointer

let aEq i =
    [aComment "EQ"]
    @ decrementStackPointer
    @ loadCurrentStackValueInto_D_Reg
    @ decrementStackPointer
    @ [
        bComment "EQ LOGIC"
        ai "@SP"
        ai "A=M"
        ai "D=D-M"
        ai $"@TRUE{i}"
        ai "D;JEQ"
        ai "D=0" //false
        ai $"@DONE{i}"
        ai "0;JMP"
        ai $"(TRUE{i})"
        ai "D=-1" //true
        ai $"@DONE{i}"
        ai "0;JMP"
        ai $"(DONE{i})"
    ] @ write_D_RegToCurrentStackPointer
    @ incrementStackPointer

let aLt i =
    [aComment "LT"]
    @ decrementStackPointer
    @ loadCurrentStackValueInto_D_Reg
    @ decrementStackPointer
    @ [
        bComment "LT LOGIC"
        ai "@SP"
        ai "A=M"
        ai "D=D-M"
        ai $"@TRUE{i}"
        ai "D;JLT"
        ai "D=0" //false
        ai $"@DONE{i}"
        ai "0;JMP"
        ai $"(TRUE{i})"
        ai "D=-1" //true
        ai $"@DONE{i}"
        ai "0;JMP"
        ai $"(DONE{i})"
    ] @ write_D_RegToCurrentStackPointer
    @ incrementStackPointer

let aGt i =
    [aComment "GT"]
    @ decrementStackPointer
    @ loadCurrentStackValueInto_D_Reg
    @ decrementStackPointer
    @ [
        bComment "LT LOGIC"
        ai "@SP"
        ai "A=M"
        ai "D=D-M"
        ai $"@TRUE{i}"
        ai "D;JGT"
        ai "D=0" //false
        ai $"@DONE{i}"
        ai "0;JMP"
        ai $"(TRUE{i})"
        ai "D=-1" //true
        ai $"@DONE{i}"
        ai "0;JMP"
        ai $"(DONE{i})"
    ] @ write_D_RegToCurrentStackPointer
    @ incrementStackPointer

let codeGenArithmetic cmd i =
    match cmd with
    | ADD -> aAdd
    | SUB -> aSub
    | NEG -> aNeg
    | EQ -> aEq i
    | GT -> aGt i
    | LT -> aLt i
    | AND -> aAnd
    | OR -> aOr
    | NOT -> aNot
    
let codeGenInstruction cmd i =
    match cmd with
    | Arithmetic a -> codeGenArithmetic a i
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
    |> List.indexed
    |> List.map (fun (i, c) -> codeGenInstruction c i)
    |> List.collect id