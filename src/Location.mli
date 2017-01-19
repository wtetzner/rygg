

type t

val source : t -> string
val line : t -> int
val column : t -> int
val create : string -> int -> int -> t

val to_string : t -> string
val empty : t
