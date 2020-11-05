
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

type t = Span.t * token_type

let to_string token =
  let open Printf in
  let span, tok = token in
  let tok_str = match tok with
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
  in
  sprintf "%s: %s" (Span.to_string span) tok_str

let compare_value =
  function
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

let substr text token =
  let (span, tok) = token in
  let start = Loc.offset (Span.start span) in
  let finish = Loc.offset (Span.finish span) in
  String.sub text start (finish - start)
