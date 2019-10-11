local
    val reservedWords = [
        "define", "if", "while", "begin", "set"
    ]
    val unaryBuiltIns = [
        "print"
    ]
    val binaryBuiltIns = [
        "+", "-", "*", "/", "=", "<", ">"
    ]
in
    fun isReservedWord s = List.exists (fn elt => s = elt) reservedWords
    fun isUnaryBuiltIn s = List.exists (fn elt => s = elt) unaryBuiltIns
    fun isBinaryBuiltIn s = List.exists (fn elt => s = elt) binaryBuiltIns
    fun isBuiltIn s = isUnaryBuiltIn s orelse isBinaryBuiltIn s
end

datatype expression =
    ValExp of int
  | VarExp of string
  | IfExp of (expression * expression * expression)
  | WhileExp of (expression * expression)
  | SetExp of (string * expression)
  | BeginExp of expression list
  | BinaryBuiltinExp of string * expression * expression
  | UnaryBuiltinExp of string * expression
  | ApExp of string * (expression list)

datatype topLevelInput =
    FunDef of string * (string list) * expression
  | Expression of expression

fun intToString n =
    if n < 0 then "-"^(Int.toString (~n)) else (Int.toString n)

fun listToString f lst =
    let fun g [] s      = s
          | g [x] s     = s ^ f x
          | g (x::xs) s = g xs (s ^ f x ^ ", ")
    in "[" ^ g lst "" ^ "]" end

fun expressionToRepr (ValExp n) = "ValExp " ^ intToString n

  | expressionToRepr (VarExp s) = "VarExp \"" ^ s ^ "\""

  | expressionToRepr (IfExp (cond, thenPart, elsePart)) =
        "IfExp (" ^
        expressionToRepr cond ^ ", " ^
        expressionToRepr thenPart ^ ", " ^
        expressionToRepr elsePart ^ ")"

  | expressionToRepr (WhileExp (cond, body)) =
        "WhileExp (" ^
        expressionToRepr cond ^ ", " ^
        expressionToRepr body ^ ")"

  | expressionToRepr (SetExp (name, exp)) =
        "SetExp (\"" ^ name ^ "\", " ^
        expressionToRepr exp ^ ")"

  | expressionToRepr (BeginExp lst) =
        "Begin " ^ listToString expressionToRepr lst

  | expressionToRepr (BinaryBuiltinExp (name, left, right)) =
        "BinaryBuiltinExp (\"" ^ name ^ "\", " ^
        expressionToRepr left ^ ", " ^
        expressionToRepr right ^ ")"

  | expressionToRepr (UnaryBuiltinExp (name, exp)) =
        "UnaryBuiltinExp (\"" ^ name ^ "\", " ^
        expressionToRepr exp ^ ")"

  | expressionToRepr (ApExp (name, argList)) =
        "ApExp (\"" ^ name ^
        "\", " ^ listToString expressionToRepr argList ^ ")"

fun funDefToString (name, formals, body) =
    "FunDef (\"" ^ name ^
    "\", " ^ listToString (fn x => "\"" ^ x ^ "\"") formals ^ ", " ^
    expressionToRepr body ^ ")"
