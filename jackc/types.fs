module jackc.types

type JackTypes =
    | J_Int
    | J_Char
    | J_Boolean
    | J_Class of string
    
type JackClassVariableScope =
    | J_Static
    | J_Field

type JackVariableName = string

type JackSubroutineName = string

type JackVariable = JackTypes * JackVariableName

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

type JackSubroutineType =
    | J_Constructor
    | J_Function
    | J_Method

type JackSubroutineReturnType =
    | J_Return of JackTypes
    | J_Void

type JackSubroutineBody = string
    
type JackLang =
    | J_ClassVariableDeclaration of JackClassVariableScope * JackTypes * JackVariableName list
    | J_LocalVariableDeclaration of JackTypes * JackVariableName list
    | J_SubroutineDeclaration of
        JackSubroutineType
        * JackSubroutineReturnType
        * JackSubroutineName
        * JackVariable list
        * JackSubroutineBody
