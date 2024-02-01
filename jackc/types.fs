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
