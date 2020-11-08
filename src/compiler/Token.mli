
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

type 'span t

val token_type : 'span t -> token_type
val span : 'span t -> 'span

val map : 'a t -> f:('a -> 'b) -> 'b t
val with_type : 'span t -> token_type -> 'span t

val is_whitespace : 'span t -> bool
val is_invalid : 'span t -> bool

val is_raw_string : 'span t -> bool
val is_any_string : 'span t -> bool

val is_comment : 'span t -> bool

val string_of_type : token_type -> string

val to_string : 'span t -> f:('span -> string) -> string

val compare_type : token_type -> token_type -> int

module Matchers : sig
  open Token
  val whitespace : 'a t -> bool
  val left_brace : 'a t -> bool
  val right_brace : 'a t -> bool
  val left_paren : 'a t -> bool
  val right_paren : 'a t -> bool
  val left_bracket : 'a t -> bool
  val right_bracket : 'a t -> bool
  val less_than : 'a t -> bool
  val greater_than : 'a t -> bool
  val arrow : 'a t -> bool
  val back_arrow : 'a t -> bool
  val colon : 'a t -> bool
  val double_colon : 'a t -> bool
  val equal : 'a t -> bool
  val semicolon : 'a t -> bool
  val dot : 'a t -> bool
  val pipe : 'a t -> bool
  val plus : 'a t -> bool
  val dash : 'a t -> bool
  val asterisk : 'a t -> bool
  val slash : 'a t -> bool
  val comma : 'a t -> bool
  val is_type : 'a t -> bool
  val namespace : 'a t -> bool
  val is_module : 'a t -> bool
  val is_let : 'a t -> bool
  val is_true : 'a t -> bool
  val is_false : 'a t -> bool
  val backtick : 'a t -> bool
  val string : 'a t -> bool
  val raw_string : 'a t -> bool
  val number : 'a t -> bool
  val comment : 'a t -> bool
  val name : 'a t -> bool
  val invalid : 'a t -> bool
  val val_kw : 'a t -> bool
  val fn : 'a t -> bool

  val trivia : 'a t -> bool
end

