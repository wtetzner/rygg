
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

val is_whitespace : t -> bool
val is_invalid : t -> bool

val is_raw_string : t -> bool
val is_any_string : t -> bool

val is_comment : t -> bool

val substr : string -> t -> string
val to_string : t -> string

val compare_type : token_type -> token_type -> int
