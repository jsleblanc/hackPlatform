module jackc.types

type JackTypes =
    | J_Int
    | J_Char
    | J_Boolean
    | J_Class of string
    
type JackClassVariableScope =
    | J_Static
    | J_Field

type JackClassName = string
type JackVariableName = string
type JackSubroutineName = string
type JackSubroutineCallScope = string

type JackExpression =
    | J_ADD of JackExpression * JackExpression
    | J_SUB of JackExpression * JackExpression
    | J_MUL of JackExpression * JackExpression
    | J_DIV of JackExpression * JackExpression
    | J_AND of JackExpression * JackExpression
    | J_OR of JackExpression * JackExpression
    | J_LT of JackExpression * JackExpression
    | J_GT of JackExpression * JackExpression
    | J_EQ of JackExpression * JackExpression
    | J_NEG of JackExpression
    | J_NOT of JackExpression
    | J_Constant_Int of int16
    | J_Constant_String of string
    | J_Constant_Boolean of bool
    | J_Constant_Null
    | J_Constant_This
    | J_Variable of JackVariableName
    | J_Array_Index of JackVariableName * JackExpression
    | J_Subroutine_Call of JackSubroutineCallScope option * JackSubroutineName * JackExpression list

type JackStatement =
    | J_Let of JackExpression //enforce that only J_EQ is used later
    | J_If_Else of JackExpression * JackStatement list * JackStatement list //if expression then statement list else statement list
    | J_While of JackExpression * JackStatement list
    | J_Do of JackSubroutineCallScope option * JackSubroutineName * JackExpression list
    | J_Return of JackExpression option

type JackSubroutineType =
    | J_Constructor
    | J_Function
    | J_Method

type JackSubroutineReturnType =
    | J_ReturnType of JackTypes
    | J_Void

type JackSubroutineBody = JackStatement list
type JackSubroutineVariable = JackTypes * JackVariableName
type JackClassVariable = JackClassVariableScope * JackTypes * JackVariableName

type JackSubroutine = {
    name: JackSubroutineName
    subType: JackSubroutineType
    returnType: JackSubroutineReturnType
    parameters: JackSubroutineVariable list
    variables: JackSubroutineVariable list
    body: JackStatement list
}

type JackClass = {
    name: JackClassName
    variables: JackClassVariable list
    subroutines: JackSubroutine list
}

type CompiledCode = {
    name: string
    code: string
}