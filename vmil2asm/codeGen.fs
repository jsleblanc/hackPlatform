module vmil2asm.codeGen

open Microsoft.FSharp.Core
open vmil2asm.types

[<Literal>]
let S_ARG = "@ARG"

[<Literal>]
let S_LCL = "@LCL"

[<Literal>]
let S_THIS = "@THIS"

[<Literal>]
let S_THAT = "@THAT"

let SEGMENT_POINTER_BASE = uint16 0x3
let SEGMENT_TEMP_BASE = uint16 0x5

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

//can be used with fake "registers" R0 through R15, or any direct memory address
let popStackIntoAddress reg =
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

let pushConstantOntoStack c =
    [
        aComment "PUSH CONSTANT"
        ai $"@{c}" //load constant into A-reg
        ai "D=A" //Copy A-reg into D-reg
    ] @ pushDIntoStack

//to be used with segment base registers LCL, ARG, THIS, THAT
let popStackIntoRelativeSegment seg idx =
    [aComment $"POP STACK INTO ({seg} + {idx})"]
    @ popStackIntoAddress "@R13"
    @ [
        ai $"@{idx}" //load offset constant into D
        ai "D=A"
        ai seg
        ai "A=M" //load segment base into A
        ai "D=D+A" //compute segment address
        ai "@R14"
        ai "M=D" //store segment address in R14
        ai "@R13"
        ai "D=M" //load value we popped from R13 into D
        ai "@R14" 
        ai "A=M" //load segment address from R14 into A
        ai "M=D"
    ]

let pushRelativeSegmentOntoStack seg idx =
    [aComment $"PUSH SEGMENT ({seg} + {idx}) ONTO STACK"]
    @ [
        ai $"@{idx}"
        ai "D=A"
        ai seg
        ai "A=M"
        ai "A=D+A"
        ai "D=M" //load from memory into D
    ] @ pushDIntoStack

//for temp and pointer segments that do not use a relative base address but a fixed one
let popStackIntoFixedSegment (addr:uint16) (idx:uint16) =
    let segAddr = addr + idx
    [aComment $"POP STACK INTO ({addr} + {idx})"]
    @ popStackIntoAddress $"@{segAddr}"

let pushFixedSegmentOntoStack (addr:uint16) (idx:uint16) =
    let segAddr = addr + idx
    [aComment $"PUSH ({addr} + {idx}) ONTO STACK"]
    @ [
        ai $"@{segAddr}"
        ai "D=M"
    ] @ pushDIntoStack

let pushStaticSegmentOntoStack name idx =
    let var = $"@{name}.{idx}"
    [aComment $"PUSH STATIC {var} ONTO STACK"]
    @ [
        ai var
        ai "D=M"
    ] @ pushDIntoStack

let popStackIntoStaticSegment name idx =
    let var = $"@{name}.{idx}"
    [aComment $"POP STACK INTO STATIC {var}"]
    @ popStackIntoAddress var

let aAdd =
    [aComment "ADD"]
    @ popStackIntoAddress "@R13" //y
    @ popStackIntoD //x
    @ [
        bComment "ADD LOGIC"
        ai "@R13"
        ai "D=D+M"
    ] @ pushDIntoStack

let aSub =
    [aComment "SUB"]
    @ popStackIntoAddress "@R13" //y
    @ popStackIntoD //x    
    @ [
        bComment "SUB LOGIC"
        ai "@R13"
        ai "D=D-M"
    ] @ pushDIntoStack

let aAnd =
    [aComment "AND"]
    @ popStackIntoAddress "@R13" //y
    @ popStackIntoD //x        
    @ [
        bComment "AND LOGIC"
        ai "@R13"
        ai "D=D&M"
    ] @ pushDIntoStack
    
let aOr =
    [aComment "OR"]
    @ popStackIntoAddress "@R13" //y
    @ popStackIntoD //x        
    @ [
        bComment "OR LOGIC"
        ai "@R13"
        ai "D=D|M"
    ] @ pushDIntoStack

let aNot =
    [aComment "NOT"]
    @ popStackIntoAddress "@R13"
    @ [
        bComment "NOT LOGIC"
        ai "@R13"
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
let equalityTesting jmp fn i =
    popStackIntoAddress "@R13" //y
    @ popStackIntoD //x
    @ [
        ai "@R13"
        ai "D=D-M"
        ai $"@{fn}$TRUE_{i}"
        ai $"D;{jmp}"
        ai "D=0" //false
        ai $"@{fn}$DONE_{i}"
        ai "0;JMP"
        ai $"({fn}$TRUE_{i})"
        ai "D=-1" //true
        ai $"@{fn}$DONE_{i}"
        ai "0;JMP"
        ai $"({fn}$DONE_{i})"
    ] @ pushDIntoStack

let aEq fn i = [aComment "EQ"] @ equalityTesting "JEQ" fn i
    
let aLt fn i = [aComment "LT"] @ equalityTesting "JLT" fn i

let aGt fn i = [aComment "GT"] @ equalityTesting "JGT" fn i

let labelInstruction context name = [ai $"({context}${name})"]

let gotoInstruction context name =
    [aComment $"GOTO {context}${name}"]
    @ [
        ai $"@{context}${name}"
        ai "0;JMP"
    ]

let ifGotoInstruction context name =
    [aComment $"IF-GOTO {context}${name}"]
    @ popStackIntoD
    @ [
        ai $"@{context}${name}"
        ai "D=D;JNE"
    ]


let codeGenArithmetic cmd fn i =
    match cmd with
    | ADD -> aAdd
    | SUB -> aSub
    | NEG -> aNeg
    | EQ -> aEq fn i
    | GT -> aGt fn i
    | LT -> aLt fn i
    | AND -> aAnd
    | OR -> aOr
    | NOT -> aNot
    
let codeGenInstruction context fn i cmd  =
    match cmd with
    | Arithmetic a -> codeGenArithmetic a fn i
    | PUSH (Constant, SegmentIndex idx) -> pushConstantOntoStack idx
    | PUSH (Argument, SegmentIndex idx) -> pushRelativeSegmentOntoStack S_ARG idx
    | PUSH (Local, SegmentIndex idx) -> pushRelativeSegmentOntoStack S_LCL idx
    | PUSH (This, SegmentIndex idx) -> pushRelativeSegmentOntoStack S_THIS idx
    | PUSH (That, SegmentIndex idx) -> pushRelativeSegmentOntoStack S_THAT idx
    | PUSH (Pointer, SegmentIndex idx) -> pushFixedSegmentOntoStack SEGMENT_POINTER_BASE idx
    | PUSH (Temp, SegmentIndex idx) -> pushFixedSegmentOntoStack SEGMENT_TEMP_BASE idx
    | PUSH (Static, SegmentIndex idx) -> pushStaticSegmentOntoStack context idx
    | POP (Local, SegmentIndex idx) -> popStackIntoRelativeSegment S_LCL idx
    | POP (Argument, SegmentIndex idx) -> popStackIntoRelativeSegment S_ARG idx
    | POP (This, SegmentIndex idx) -> popStackIntoRelativeSegment S_THIS idx
    | POP (That, SegmentIndex idx) -> popStackIntoRelativeSegment S_THAT idx
    | POP (Pointer, SegmentIndex idx) -> popStackIntoFixedSegment SEGMENT_POINTER_BASE idx
    | POP (Temp, SegmentIndex idx) -> popStackIntoFixedSegment SEGMENT_TEMP_BASE idx
    | POP (Static, SegmentIndex idx) -> popStackIntoStaticSegment context idx
    | Label l -> labelInstruction fn l
    | Goto l -> gotoInstruction fn l
    | If_Goto l -> ifGotoInstruction fn l
    | _ -> failwith $"{cmd} not supported yet"

//need to process the list of instructions and group them by function
let groupCodeIntoFunctions commands =
    let mutable i = ""
    let f c =
        match c with
        | Function (name, _) ->
            i <- name
            (i,c)
        | _ -> (i,c)        
    commands
    |> List.map f
    |> List.groupBy fst
    |> List.map (fun (k, grp) -> (k, List.map snd grp))

let codeGenInstructionsForFunction context fn commands =
    commands
    |> List.indexed
    |> List.map (fun (idx, cmd) -> codeGenInstruction context fn idx cmd)
    |> List.collect id
    
let codeGenInstructions context commands =
    groupCodeIntoFunctions commands
    |> List.map (fun (fn, c) -> codeGenInstructionsForFunction context fn c)
    |> List.collect id
    
    