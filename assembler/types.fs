module assembler.types

open System

type BuiltInSymbol =
    | SP
    | LCL
    | ARG
    | THIS
    | THAT
    | R0
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    | SCREEN
    | KBD

type Jump =
    | JGT
    | JEQ
    | JGE
    | JLT
    | JNE
    | JLE
    | JMP

[<Flags>]
type Destination =
    | None = 0
    | M = 0b001
    | D = 0b010
    | A = 0b100

type OP_Code =
    | OP_ZERO
    | OP_ONE
    | OP_NEG_ONE
    | OP_D
    | OP_A
    | OP_M
    | OP_NOT_D
    | OP_NOT_A
    | OP_NOT_M
    | OP_NEG_D
    | OP_NEG_A
    | OP_NEG_M
    | OP_D_PLUS_ONE
    | OP_A_PLUS_ONE
    | OP_M_PLUS_ONE
    | OP_D_MINUS_ONE
    | OP_A_MINUS_ONE
    | OP_M_MINUS_ONE
    | OP_D_PLUS_A
    | OP_D_PLUS_M
    | OP_D_MINUS_A
    | OP_D_MINUS_M
    | OP_A_MINUS_D
    | OP_M_MINUS_D
    | OP_D_AND_A
    | OP_D_AND_M
    | OP_D_OR_A
    | OP_D_OR_M

type Symbol =
    | Predefined of BuiltInSymbol
    | Variable of string
    | Constant of uint16
    
type Instruction =
    | A_Instruction of Symbol
    | C_Instruction of Destination option * OP_Code * Jump option
    | Label of string //not an instruction but a marker to track a ROM address to be referenced later
    
type AssembledProgram = {
    instructions: string list
    symbolTable: Map<string, uint16>
}