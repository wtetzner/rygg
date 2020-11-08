
type t
type source = string

val create : filename:string -> line:int -> column:int -> offset:int -> t
val unknown : t

val sources : t -> string list

val filename : t -> string
val line : t -> int
val column : t -> int
val offset : t -> int

val with_filename : t -> filename:string -> t

val inc_column : t -> amount:int -> t
val advance_to : t -> str:string -> end_pos:int -> t

val equal : t -> t -> bool
val compare : t -> t -> int
val to_string : t -> string
val debug_string : t -> string
