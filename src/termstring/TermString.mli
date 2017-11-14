
type t

val styled : (ANSITerminal.style list) -> string -> t

val of_string : string -> t

val (^) : t -> t -> t

(* val concat : t list -> t *)

val to_string : t -> string

val print_string : t -> unit
