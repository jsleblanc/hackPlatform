module vmil2asm.types

open System.IO

type SegmentIndex = SegmentIndex of uint16

type Segment =
    | Argument
    | Local
    | Static
    | Constant
    | This
    | That
    | Pointer
    | Temp

type Symbol = string
type ArgumentCount = int

type ArithmeticCommand =
    | ADD
    | SUB
    | NEG
    | EQ
    | GT
    | LT
    | AND
    | OR
    | NOT 

type Command =
    | Arithmetic of ArithmeticCommand
    | PUSH of Segment * SegmentIndex
    | POP of Segment * SegmentIndex
    | Label of Symbol
    | Goto of Symbol
    | If_Goto of Symbol
    | Function of Symbol * ArgumentCount
    | Call of Symbol * ArgumentCount
    | Return

type AssemblyInstruction = AssemblyInstruction of string    
    
type ProcessFilesRequest = {
    inputFiles: FileInfo list
    outputName: string
    initVm: bool 
}

type StringRequest = {
    name: string
    input: string
}
