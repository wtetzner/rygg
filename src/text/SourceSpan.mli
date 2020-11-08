
type t
type source = string

val from : SourceLoc.t -> SourceLoc.t -> t
val unknown : t

val start : t -> SourceLoc.t
val finish : t -> SourceLoc.t

val sources : t -> source list

val singular : SourceLoc.t -> t
val is_singular : t -> bool

(* span -> offset * length *)
val slice : t -> int * int
val substr : t -> string -> string

val length : t -> int

val equal : t -> t -> bool
val compare : t -> t -> int
val merge : t -> t -> t
val to_string : t -> string
val debug_string : t -> string

val to_span : t -> Span.t
