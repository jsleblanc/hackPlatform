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
    | Function of Symbol * nLocals: int
    | Call of Symbol * nArgs: int
    | Return
    
    
type ProcessRequest = {
    inputFiles: FileInfo list
    outputName: string
}

