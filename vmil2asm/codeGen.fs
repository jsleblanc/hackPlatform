module vmil2asm.codeGen

open Microsoft.FSharp.Core
open vmil2asm.types

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
let equalityTesting jmp i =
    popStackIntoAddress "@R13" //y
    @ popStackIntoD //x
    @ [
        ai "@R13"
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
    | PUSH (Constant, SegmentIndex idx) -> pushConstantOntoStack idx
    | PUSH (Argument, SegmentIndex idx) -> pushRelativeSegmentOntoStack "@ARG" idx
    | PUSH (Local, SegmentIndex idx) -> pushRelativeSegmentOntoStack "@LCL" idx
    | PUSH (This, SegmentIndex idx) -> pushRelativeSegmentOntoStack "@THIS" idx
    | PUSH (That, SegmentIndex idx) -> pushRelativeSegmentOntoStack "@THAT" idx
    | PUSH (Pointer, SegmentIndex idx) -> pushFixedSegmentOntoStack SEGMENT_POINTER_BASE idx
    | PUSH (Temp, SegmentIndex idx) -> pushFixedSegmentOntoStack SEGMENT_TEMP_BASE idx
    | PUSH (Static, SegmentIndex idx) -> pushStaticSegmentOntoStack "foo" idx
    | POP (Local, SegmentIndex idx) -> popStackIntoRelativeSegment "@LCL" idx
    | POP (Argument, SegmentIndex idx) -> popStackIntoRelativeSegment "@ARG" idx
    | POP (This, SegmentIndex idx) -> popStackIntoRelativeSegment "@THIS" idx
    | POP (That, SegmentIndex idx) -> popStackIntoRelativeSegment "@THAT" idx
    | POP (Pointer, SegmentIndex idx) -> popStackIntoFixedSegment SEGMENT_POINTER_BASE idx
    | POP (Temp, SegmentIndex idx) -> popStackIntoFixedSegment SEGMENT_TEMP_BASE idx
    | POP (Static, SegmentIndex idx) -> popStackIntoStaticSegment "foo" idx
    | Label l -> labelInstruction "foo" l
    | Goto l -> gotoInstruction "foo" l
    | If_Goto l -> ifGotoInstruction "foo" l
    | _ -> failwith $"{cmd} not supported yet"

//need to process the list of instructions and group them by function
let groupCodeIntoFunctions (cmds:Command list) =
    let mutable i = ""
    let f c =
        match c with
        | Function (name, _) ->
            i <- name
            (i,c)
        | _ -> (i,c)        
    cmds
    |> List.map f
    |> List.groupBy (fun (k,_) -> k)
    |> List.map (fun (k, grp) -> (k, List.map (fun (_, c) -> c) grp))
    
let codeGenInstructions context cmds =
    let foo = groupCodeIntoFunctions cmds
    cmds
    |> List.indexed
    |> List.map (fun (i, c) -> codeGenInstruction c i)
    |> List.collect id