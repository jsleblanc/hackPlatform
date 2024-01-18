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

type Computation = string

type Symbol =
    | Predefined of BuiltInSymbol
    | Variable of string
    | Constant of uint16
    
type Instruction =
    | A_Instruction of Symbol
    | C_Instruction of Destination option * Computation * Jump option
    | Label of string //not an instruction but a marker to track a ROM address to be referenced later
    
type AssembledProgram = {
    instructions: string list
    symbolTable: Map<string, uint16>
}