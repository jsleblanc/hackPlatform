module vmil2asm.codeGen

open vmil2asm.types

let ai = AssemblyInstruction
let aComment s = ai $"// --- {s} ---"
let bComment s = ai $"//  {s}"

let popStackIntoD =
    [
        bComment "POP STACK INTO D-Reg"
        ai "@SP"
        ai "M=M-1"
        ai "@SP"
        ai "A=M"
        ai "D=M"
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

//can be used with fake "registers" R0 through R15
let popStackIntoReg reg =
    [
        bComment $"POP STACK INTO {reg} (via D-Reg)"
        ai "@SP"
        ai "M=M-1"
        ai "@SP"
        ai "A=M"
        ai "D=M"
        ai reg
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
    @ popStackIntoReg "@R2" //y
    @ popStackIntoD //x
    @ [
        bComment "ADD LOGIC"
        ai "@R2"
        ai "D=D+M"
    ] @ pushDIntoStack

let aSub =
    [aComment "SUB"]
    @ popStackIntoReg "@R2" //y
    @ popStackIntoD //x    
    @ [
        bComment "SUB LOGIC"
        ai "@R2"
        ai "D=D-M"
    ] @ pushDIntoStack

let aAnd =
    [aComment "AND"]
    @ popStackIntoReg "@R2" //y
    @ popStackIntoD //x        
    @ [
        bComment "AND LOGIC"
        ai "@R2"
        ai "D=D&M"
    ] @ pushDIntoStack
    
let aOr =
    [aComment "OR"]
    @ popStackIntoReg "@R2" //y
    @ popStackIntoD //x        
    @ [
        bComment "OR LOGIC"
        ai "@R2"
        ai "D=D|M"
    ] @ pushDIntoStack

let aNot =
    [aComment "NOT"]
    @ popStackIntoReg "@R2"
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

//logic is all the same, only the jump conditions change for eq, lt, gt
let equalityTesting jmp i =
    popStackIntoReg "@R2" //y
    @ popStackIntoD //x
    @ [
        ai "@R2"
        ai "D=D-M"
        ai $"@TRUE{i}"
        ai $"D;{jmp}"
        ai "D=0" //false
        ai $"@DONE{i}"
        ai "0;JMP"
        ai $"(TRUE{i})"
        ai "D=-1" //true
        ai $"@DONE{i}"
        ai "0;JMP"
        ai $"(DONE{i})"
    ] @ pushDIntoStack

let aEq i = [aComment "EQ"] @ equalityTesting "JEQ" i
    
let aLt i = [aComment "LT"] @ equalityTesting "JLT" i

let aGt i = [aComment "GT"] @ equalityTesting "JGT" i

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
        ] @ pushDIntoStack
       
let codeGenInstructions cmds =
    cmds
    |> List.indexed
    |> List.map (fun (i, c) -> codeGenInstruction c i)
    |> List.collect id