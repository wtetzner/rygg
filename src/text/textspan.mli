
type t

val from : ~start:int -> ~length:int -> t
val from_bounds : ~start:int -> ~ending:int -> t

(* Start position of the span *)
val start : t -> int

(* End position of the span *)
val ending : t -> int

(* Length of the span *)
val length : t -> int

(* Check if the span is empty *)
val is_empty : t -> bool

(* Check if the given span contains the given position *)
val contains : t -> ~pos:int -> bool

(* Check if the given span contains `span` *)
val contains_span : t -> ~span:t -> bool

(* Check if `span` overlaps with the given span *)
val overlaps_with : t -> ~span:t -> bool

(* The overlap, or None if the spans don't overlap *)
val overlap : t -> ~span:t -> t option

(* Check if `span` intersects with the given span *)
val intersects_with : t -> ~span:t -> bool

(* Check if `pos` intersects with the given span *)
val intersects_with_pos : t -> ~pos:int -> bool

(* The intersection, or None if they don't intersect *)
val intersection : t -> ~span:t -> t option

val equal : t -> t -> bool
val compare : t -> t -> int
