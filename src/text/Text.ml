
module type INPUT = sig
  type t
  type loc

  val length : t -> int
  val is_empty : t -> bool
  val loc : t -> loc
  val offset : t -> int
  val starts_with : t -> string -> bool
  val current_char : t -> char
  val in_bounds : t -> int -> bool
  val advance_by : t -> int -> t
end

module Span = Span

module SourceLoc = SourceLoc
module SourceSpan = SourceSpan
