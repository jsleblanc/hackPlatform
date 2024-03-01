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

let SEGMENT_POINTER_BASE = 0x3us
let SEGMENT_TEMP_BASE = 0x5us

let ai = AssemblyInstruction
let aComment s = ai $"// --- {s} ---"
let bComment s = ai $"//  {s}"

let popStackIntoD =
    [
        bComment "POP STACK INTO D-Reg"
        ai "@SP"
        ai "AM=M-1"
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

let popStackIntoSegment seg =
    [aComment "POP STACK INTO THIS"]
    @ popStackIntoD
    @ [
        ai seg
        ai "M=D"
    ]

let pushSegmentOntoStack seg =
    [aComment "PUSH SEGMENT ONTO STACK"]
    @ [
        ai seg
        ai "D=M"
    ] @ pushDIntoStack

//to be used with segment base registers LCL, ARG, THIS, THAT
let popStackIntoRelativeSegment seg idx =
    [aComment $"POP STACK INTO ({seg} + {idx})"]
    @ [
        ai seg
        ai "D=M"
        ai $"@{idx}"
        ai "D=D+A"
        ai "@R14"
        ai "M=D" //segment base+idx address stored in R14
    ] @ popStackIntoD
    @ [
        ai "@R14"
        ai "A=M"
        ai "M=D"
    ]
 
let pushRelativeSegmentOntoStack seg idx =
    [aComment $"PUSH SEGMENT ({seg} + {idx}) ONTO STACK"]
    @ [
        ai seg
        ai "D=M"
        ai $"@{idx}"
        ai "A=D+A"
        ai "D=M"
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
    @ popStackIntoD //y
    @ [
        //top of stack is x
        ai "@SP"
        ai "A=M-1"
        ai "M=D+M"
    ]

let aSub =
    [aComment "SUB"]
    @ popStackIntoD //y
    @ [
        //top of stack is x
        ai "@SP"
        ai "A=M-1"
        ai "M=M-D"
    ]

let aAnd =
    [aComment "AND"]
    @ popStackIntoD //y
    @ [
        //top of stack is x
        ai "@SP"
        ai "A=M-1"
        ai "M=D&M"
    ]
    
let aOr =
    [aComment "OR"]
    @ popStackIntoD //y
    @ [
        //top of stack is x
        ai "@SP"
        ai "A=M-1"
        ai "M=D|M"
    ]

let aNot =
    [aComment "NOT"]
    @ [
        //modify top of stack in-place
        ai "@SP"
        ai "A=M-1"
        ai "M=!M"
    ]

let aNeg =
    [aComment "NEG"]
    @ [
        //modify top of stack in-place
        ai "@SP"
        ai "A=M-1"
        ai "M=-M"
    ]

//two operands, one is popped off the stack, the second is overwritten with the result
let aEquality jmp fn i =
    popStackIntoD //y
    @ [
        ai "@SP"
        ai "A=M-1" //x
        ai "D=M-D" //x - y
        ai $"@{fn}$TRUE_{i}"
        ai $"D;{jmp}"
        ai "D=0"
        ai $"@{fn}$DONE_{i}"
        ai "0;JMP"
        ai $"({fn}$TRUE_{i})"
        ai "D=-1" 
        ai $"({fn}$DONE_{i})"
        ai "@SP"
        ai "A=M-1"
        ai "M=D"
    ]

let aEq fn i = [aComment "EQ"] @ aEquality "JEQ" fn i
    
let aLt fn i = [aComment "LT"] @ aEquality "JLT" fn i

let aGt fn i = [aComment "GT"] @ aEquality "JGT" fn i

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

let callFunction context fn i f args =
    let returnLabel = $"{context}.{fn}.RETURN${i}" 
    [aComment $"FUNCTION CALL {f} ARGS {args}"]
    @ [
        ai $"@{returnLabel}" 
        ai "D=A"
    ] @ pushDIntoStack //push return address onto stack
    @ [
        ai "@LCL"
        ai "D=M"
    ] @ pushDIntoStack //push LCL onto stack
    @ [
        ai "@ARG"
        ai "D=M"
    ] @ pushDIntoStack //push ARG onto stack
    @ [
        ai "@THIS"
        ai "D=M"
    ] @ pushDIntoStack //push THIS onto stack
    @ [
        ai "@THAT"
        ai "D=M"
    ] @ pushDIntoStack //push THAT onto stack
    @ [
        //ARG = @SP-n-5
        ai "@SP"
        ai "D=M"
        ai $"@{args+5}"
        ai "D=D-A"
        ai "@ARG"
        ai "M=D"
    ] @ [
        //LCL = SP
        ai "@SP"
        ai "D=M"
        ai "@LCL"
        ai "M=D"
    ] @ [
        //GOTO f
        ai $"@{f}"
        ai "0;JMP"
    ] @ [
        ai $"({returnLabel})"
    ]

let defineFunction context fn vars =
    [aComment $"DEFINE FUNCTION {fn} VARS {vars}"]
    @ [
        ai $"({fn})"
        ai "D=0"
    ] @ (List.init vars (fun _ -> pushDIntoStack) |> List.collect id)

let returnFunction =
    [
        //jump to the common return-function code so we don't have to duplicate it for every single return
        ai "@FUNCTION_RETURN_COMMON"
        ai "0;JMP"
    ]
    
let returnFunctionCommon =
    [aComment "RETURN COMMON"]
    @ [bComment "FRAME = LCL"]
    @ [ ai "(FUNCTION_RETURN_COMMON)" ]
    @ [
        //FRAME = LCL
        ai "@LCL"
        ai "D=M"
        ai "@FRAME"
        ai "M=D"
    ] @ [bComment "RET = *(FRAME-5)"]
      @ [
        //RET = *(FRAME-5)
        ai "@5"
        ai "D=A"
        ai "@FRAME"
        ai "A=M-D"
        ai "D=M"
        ai "@RET"
        ai "M=D"
    ] @ [bComment "*ARG = pop()"]
    @ popStackIntoD
    @ [
        //*ARG = pop()
        ai "@ARG"
        ai "A=M"
        ai "M=D"
    ] @ [bComment "SP = ARG + 1"]
      @ [
        //SP = ARG + 1
        ai "@ARG"
        ai "D=M+1"
        ai "@SP"
        ai "M=D"
    ] @ [bComment "THAT = *(FRAME-1)"]
      @ [
        //THAT = *(FRAME-1)
        ai "@FRAME"
        ai "A=M-1"
        ai "D=M"
        ai "@THAT"
        ai "M=D"
    ] @ [bComment "THIS = *(FRAME-2)"]
      @ [
        //THIS = *(FRAME-2)
        ai "@FRAME"
        ai "A=M-1"
        ai "A=A-1"
        ai "D=M"
        ai "@THIS"
        ai "M=D"
    ] @ [bComment "ARG = *(FRAME-3)"]
      @ [
        //ARG = *(FRAME-3)
        ai "@3"
        ai "D=A"
        ai "@FRAME"
        ai "A=M-D"
        ai "D=M"
        ai "@ARG"
        ai "M=D"
    ] @ [bComment "LCL = *(FRAME-4)"]
      @ [
        //LCL = *(FRAME-4)
        ai "@4"
        ai "D=A"
        ai "@FRAME"
        ai "A=M-D"
        ai "D=M"
        ai "@LCL"
        ai "M=D"
    ] @ [bComment "GOTO RET"]
      @ [
        //goto RET
        ai "@RET"
        ai "A=M"
        ai "0;JMP"
    ]

let initVm =
    [aComment "INIT"]
    @ [
        ai "@256" //init SP
        ai "D=A"
        ai "@SP"
        ai "M=D"
    ] @ callFunction "" "Bootstrap" 0 "Sys.init" 0

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
    | PUSH (Pointer, SegmentIndex 0us) -> pushSegmentOntoStack S_THIS
    | PUSH (Pointer, SegmentIndex 1us) -> pushSegmentOntoStack S_THAT
    | PUSH (Temp, SegmentIndex idx) -> pushFixedSegmentOntoStack SEGMENT_TEMP_BASE idx
    | PUSH (Static, SegmentIndex idx) -> pushStaticSegmentOntoStack context idx
    | POP (Local, SegmentIndex idx) -> popStackIntoRelativeSegment S_LCL idx
    | POP (Argument, SegmentIndex idx) -> popStackIntoRelativeSegment S_ARG idx
    | POP (This, SegmentIndex idx) -> popStackIntoRelativeSegment S_THIS idx
    | POP (That, SegmentIndex idx) -> popStackIntoRelativeSegment S_THAT idx
    | POP (Pointer, SegmentIndex 0us) -> popStackIntoSegment S_THIS
    | POP (Pointer, SegmentIndex 1us) -> popStackIntoSegment S_THAT
    | POP (Temp, SegmentIndex idx) -> popStackIntoFixedSegment SEGMENT_TEMP_BASE idx
    | POP (Static, SegmentIndex idx) -> popStackIntoStaticSegment context idx
    | Label l -> labelInstruction fn l
    | Goto l -> gotoInstruction fn l
    | If_Goto l -> ifGotoInstruction fn l
    | Call (f, args) -> callFunction context fn i f args
    | Function (f, vars) -> defineFunction context f vars
    | Return -> returnFunction
    | x -> failwith $"\"{x}\" is not a supported operation"

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
    let flip f x y = f y x
    groupCodeIntoFunctions commands
    |> List.map (fun (fn, c) -> codeGenInstructionsForFunction context fn c)
    |> flip List.append [returnFunctionCommon]
    |> List.collect id

    
    