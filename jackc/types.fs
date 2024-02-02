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

type JackExpressionKeywords =
    | J_Bool of bool
    | J_Null
    | J_This

type JackExpressionBinaryOps =
    | J_ADD
    | J_SUB
    | J_MUL
    | J_DIV
    | J_AND
    | J_OR
    | J_LT
    | J_GT
    | J_EQ

type JackExpressionUnaryOps =
    | J_NEG
    | J_NOT

type JackExpression =
    | J_Expression of JackExpressionTerm * (JackExpressionBinaryOps * JackExpressionTerm) list

and JackExpressionTerm =
    | J_Constant_Int of int16
    | J_Constant_String of string
    | J_Constant_Keyword of JackExpressionKeywords
    | J_Variable of JackVariableName
    | J_ArrayIndex of JackVariableName * JackExpression
    | J_SubroutineCall of JackSubroutineName * JackExpression list
    | J_Parenthesis of JackExpression
    | J_UnaryOp of JackExpressionUnaryOps

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
