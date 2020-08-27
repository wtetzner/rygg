
open Location

type t

type regexp_match_result = {
    string: string;
    groups: string list
}

(* filename -> data -> input *)
val from_string : string -> string -> t
val length : t -> int
val is_empty : t -> bool
val loc : t -> Loc.t
val offset : t -> int
val full_text : t -> string
val starts_with : t -> string -> bool
val matches_char : t -> char -> bool
val matches : t -> (char -> bool) -> bool
val current_char : t -> char
val char_at : t -> int -> char
val in_bounds : t -> int -> bool
val advance_by : t -> int -> t
val advance : t -> string -> t
val end_loc : t -> int -> Loc.t
val match_regexp : t -> Str.regexp -> regexp_match_result option
val substring : t -> Span.t -> string
val to_string : t -> string
