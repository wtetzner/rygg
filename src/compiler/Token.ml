
type comment_type = Line | Multiline
type raw_string_prefix_count = int

type token_type =
  | Whitespace
  | Left_brace
  | Right_brace
  | Left_paren
  | Right_paren
  | Left_bracket
  | Right_bracket
  | Less_than
  | Less_equal
  | Greater_than
  | Greater_equal
  | Arrow
  | Back_arrow
  | Colon
  | Double_colon
  | Equal
  | Double_equal
  | Semicolon
  | Dot
  | Pipe
  | Plus
  | Dash
  | Asterisk
  | Slash
  | Comma
  | Question
  | Exclamation
  | Type
  | Namespace
  | Module
  | Let
  | True
  | False
  | Implicit
  | Val
  | Fn
  | Backtick
  | String
  | Raw_string of raw_string_prefix_count
  | Number
  | Comment of comment_type
  | Name
  | Invalid of string

type 'span t = 'span * token_type

let token_type token =
  let (_, token_type) = token in
  token_type

let span token =
  let (span, _) = token in
  span

let map token ~f =
  let (span, token_type) = token in
  (f span, token_type)

let with_type token token_type =
  let (span, _) = token in
  (span, token_type)

let string_of_type tok =
  let open Printf in
  match tok with
  | Whitespace -> "Whitespace"
  | Left_brace -> "{"
  | Right_brace -> "}"
  | Left_paren -> "("
  | Right_paren -> ")"
  | Left_bracket -> "["
  | Right_bracket -> "]"
  | Less_than -> "<"
  | Less_equal -> "<="
  | Greater_than -> ">"
  | Greater_equal -> ">="
  | Arrow -> "->"
  | Back_arrow -> "<-"
  | Colon -> ":"
  | Double_colon -> "::"
  | Equal -> "="
  | Double_equal -> "=="
  | Semicolon -> ";"
  | Dot -> "."
  | Pipe -> "|"
  | Plus -> "+"
  | Dash -> "-"
  | Asterisk -> "*"
  | Slash -> "/"
  | Comma -> ","
  | Question -> "?"
  | Exclamation -> "!"
  | Type -> "type"
  | Namespace -> "namespace"
  | Module -> "module"
  | Let -> "let"
  | True -> "true"
  | False -> "false"
  | Implicit -> "implicit"
  | Val -> "val"
  | Fn -> "fn"
  | Backtick -> "`"
  | String -> "String"
  | Raw_string prefix_count ->
     let prefix = String.make prefix_count '#' in
     sprintf "r%s\"...\"%s" prefix prefix
  | Number -> "Number"
  | Comment Line -> "Line Comment"
  | Comment Multiline -> "Multiline Comment"
  | Name -> "Name"
  | Invalid message -> sprintf "Invalid \"%s\"" message

let to_string token ~f =
  let (span, token_type) = token in
  Printf.sprintf "%s: %s" (f span) (string_of_type token_type)

let compare_value = function
  | Whitespace -> 0
  | Left_brace -> 1
  | Right_brace -> 2
  | Left_paren -> 3
  | Right_paren -> 4
  | Left_bracket -> 5
  | Right_bracket -> 6
  | Less_than -> 7
  | Greater_than -> 8
  | Arrow -> 9
  | Back_arrow -> 10
  | Colon -> 11
  | Double_colon -> 12
  | Equal -> 13
  | Double_equal -> 14
  | Semicolon -> 15
  | Dot -> 16
  | Pipe -> 17
  | Plus -> 18
  | Dash -> 19
  | Asterisk -> 20
  | Slash -> 21
  | Comma -> 22
  | Question -> 23
  | Exclamation -> 24
  | Type -> 25
  | Namespace -> 26
  | Module -> 27
  | Let -> 28
  | True -> 29
  | False -> 30
  | Implicit -> 31
  | Backtick -> 32
  | String -> 33
  | Number -> 34
  | Name -> 35
  | Raw_string _ -> 36
  | Comment _ -> 37
  | Invalid _ -> 38
  | Less_equal -> 39
  | Greater_equal -> 40
  | Val -> 41
  | Fn -> 42

let compare_type l r = Int.compare (compare_value l) (compare_value r)

let is_whitespace = function (_, Whitespace) -> true | _ -> false

let is_invalid = function (_, Invalid _) -> true | _ -> false

let is_raw_string = function (_, Raw_string _) -> true | _ -> false
let is_any_string = function (_, String)
                           | (_, Raw_string _) -> true
                           | _ -> false

let is_comment = function (_, Comment _) -> true | _ -> false

module Matchers = struct
  open Token
  let whitespace = function (_, Whitespace) -> true | _ -> false
  let left_brace = function (_, Left_brace) -> true | _ -> false
  let right_brace = function (_, Right_brace) -> true | _ -> false
  let left_paren = function (_, Left_paren) -> true | _ -> false
  let right_paren = function (_, Right_paren) -> true | _ -> false
  let left_bracket = function (_, Left_bracket) -> true | _ -> false
  let right_bracket = function (_, Right_bracket) -> true | _ -> false
  let less_than = function (_, Less_than) -> true | _ -> false
  let greater_than = function (_, Greater_than) -> true | _ -> false
  let arrow = function (_, Arrow) -> true | _ -> false
  let back_arrow = function (_, Back_arrow) -> true | _ -> false
  let colon = function (_, Colon) -> true | _ -> false
  let double_colon = function (_, Double_colon) -> true | _ -> false
  let equal = function (_, Equal) -> true | _ -> false
  let semicolon = function (_, Semicolon) -> true | _ -> false
  let dot = function (_, Dot) -> true | _ -> false
  let pipe = function (_, Pipe) -> true | _ -> false
  let plus = function (_, Plus) -> true | _ -> false
  let dash = function (_, Dash) -> true | _ -> false
  let asterisk = function (_, Asterisk) -> true | _ -> false
  let slash = function (_, Slash) -> true | _ -> false
  let comma = function (_, Comma) -> true | _ -> false
  let is_type = function (_, Type) -> true | _ -> false
  let namespace = function (_, Namespace) -> true | _ -> false
  let is_module = function (_, Module) -> true | _ -> false
  let is_let = function (_, Let) -> true | _ -> false
  let is_true = function (_, True) -> true | _ -> false
  let is_false = function (_, False) -> true | _ -> false
  let backtick = function (_, Backtick) -> true | _ -> false
  let string = function (_, String) -> true | _ -> false
  let raw_string = function (_, Raw_string _) -> true | _ -> false
  let number = function (_, Number) -> true | _ -> false
  let comment = function (_, Comment _) -> true | _ -> false
  let name = function (_, Name) -> true | _ -> false
  let invalid = function (_, Invalid _) -> true | _ -> false
  let val_kw = function (_, Val) -> true | _ -> false
  let fn = function (_, Fn) -> true | _ -> false

  let trivia = function (_, Whitespace)
                      | (_, Comment _)
                        | (_, Invalid _) -> true
                      | _ -> false
end
