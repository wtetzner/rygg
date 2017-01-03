
module Environment : Env.Environment
       with type name = string
       with type value = int

type expression =
  | Name of string
  | Plus of expression * expression
  | Minus of expression * expression
  | Times of expression * expression
  | Divide of expression * expression
  | Number of int
  | UpperByte of expression
  | LowerByte of expression

val eval : expression -> Environment.t -> int

val to_string : expression -> string



