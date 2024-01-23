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

let pushDIntoStack =
    [
        bComment "PUSH D-Reg ONTO STACK"
        ai "@SP"
        ai "A=M"
        ai "M=D"
        ai "@SP"
        ai "M=M+1"
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
    @ popStackIntoR2 //y
    @ popStackIntoD //x
    @ [
        bComment "ADD LOGIC"
        ai "@R2"
        ai "D=D+M"
    ] @ pushDIntoStack

let aSub =
    [aComment "SUB"]
    @ popStackIntoR2 //y
    @ popStackIntoD //x    
    @ [
        bComment "SUB LOGIC"
        ai "@R2"
        ai "D=D-M"
    ] @ pushDIntoStack

let aAnd =
    [aComment "AND"]
    @ popStackIntoR2 //y
    @ popStackIntoD //x        
    @ [
        bComment "AND LOGIC"
        ai "@R2"
        ai "D=D&M"
    ] @ pushDIntoStack
    
let aOr =
    [aComment "OR"]
    @ popStackIntoR2 //y
    @ popStackIntoD //x        
    @ [
        bComment "OR LOGIC"
        ai "@R2"
        ai "D=D|M"
    ] @ pushDIntoStack

let aNot =
    [aComment "NOT"]
    @ popStackIntoR2 
    @ [
        bComment "NOT LOGIC"
        ai "@R2"
        ai "D=!M"
    ] @ pushDIntoStack

let aNeg =
    [aComment "NEG"]
    @ popStackIntoD
    @ [
        bComment "NEG LOGIC"
        ai "D=-D"
    ] @ pushDIntoStack

let aEq i =
    [aComment "EQ"]
    @ popStackIntoR2 //y
    @ popStackIntoD //x
    @ [
        bComment "EQ LOGIC"
        ai "@R2"
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
    ] @ pushDIntoStack
    
let aLt i =
    [aComment "LT"]
    @ popStackIntoR2 //y
    @ popStackIntoD //x
    @ [
        bComment "LT LOGIC"
        ai "@R2"
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
    ] @ pushDIntoStack

let aGt i =
    [aComment "GT"]
    @ popStackIntoR2 //y
    @ popStackIntoD //x
    @ [
        bComment "LT LOGIC"
        ai "@R2"
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
    ] @ pushDIntoStack

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