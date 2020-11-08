
module type S = sig
  type t
  type loc
  type span

  val length : t -> int
  val is_empty : t -> bool
  val loc : t -> loc
  val offset : t -> int
  val starts_with : t -> bool
  val get : t -> index:int -> char
  val advance_by : t -> amount:int -> t
  val current_char : t -> char
  val in_bounds : t -> index:int -> bool
  val substr : t -> span:span -> string
  val to_string : t -> string
end

module LocStringSource : sig
  type t
  type loc = Text.SourceLoc.t
  type span = Text.SourceSpan.t

  val from_string : filename:string -> string:string -> t

  include S with type t := t
          with type loc := loc
          with type span := span
end = struct
  type t = {
      source: string;
      data: string;
      pos: int;
      line: int;
      column: int
    }

  type loc = Text.SourceLoc.t
  type span = Text.SourceSpan.t

  module Loc = Text.SourceLoc
  module Span = Text.SourceSpan

  let from_string ~filename ~string = {
      source = filename;
      data = string;
      pos = 0;
      line = 1;
      column = 0
    }

  let length input =
    (String.length input.data) - input.pos

  let is_empty input = (length input) <= 0

  let loc input =
    Loc.create input.source input.line input.column input.pos

  let offset input = input.pos
  let full_text input = input.data

  let advance_loc = Loc.advance_to

  let get input ~index =
    String.get input.data (input.pos + index)

  let end_loc input amount =
    let str_len = String.length input.data in
    let end_pos = (input.pos + amount) in
    let end_pos = if end_pos > str_len then str_len else end_pos in
    advance_loc (loc input) input.data end_pos

  let advance_by input ~amount =
    let new_loc = end_loc input amount in
    { input with
      pos = Loc.offset new_loc;
      line = Loc.line new_loc;
      column = Loc.column new_loc }

  let advance input substr =
    advance_by input (String.length substr)

  let starts_with input substr =
    let slen = (String.length substr) in
    let ilen = length input in
    if ilen < slen then
      false
    else
      starts_with_at input.data substr input.pos 0

  let current_char input = String.get input.data input.pos

  let in_bounds input ~index = index >= 0 && index < (length input)

  let substr input ~span =
    let start = Loc.offset (Span.start span) in
    let finish = Loc.offset (Span.finish span) in
    String.sub input.data start (finish - start)

  let to_string input =
    String.sub input.data input.pos (length input)
end
