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
    
type JackLang =
    | J_ClassVariableDeclaration of JackClassVariableScope * JackTypes * JackVariableName list