
module type S = sig
  type t
  type loc
  type span

  val span : loc -> loc -> span

  module Span: sig
    type t

    val start : t -> loc
    val ending : t -> loc
    val merge : t -> t -> t
  end with type t = span
  
  val length : t -> int
  val is_empty : t -> bool
  val loc : t -> loc
  val offset : t -> int
  val starts_with : t -> substr:string -> bool
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

  let from_string ~filename ~string = {
      source = filename;
      data = string;
      pos = 0;
      line = 1;
      column = 0
    }

  let span l r = Text.SourceSpan.from l r

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

  let rec starts_with_at string substr str_idx sub_idx =
    let sub_len = String.length substr in
    if sub_idx >= sub_len then
      true
    else if (String.get string str_idx) != (String.get substr sub_idx) then
      false
    else
      starts_with_at string substr (str_idx + 1) (sub_idx + 1)

  let starts_with input ~substr =
    let slen = (String.length substr) in
    let ilen = length input in
    if ilen < slen then
      false
    else
      starts_with_at input.data substr input.pos 0

  let current_char input = String.get input.data input.pos

  let in_bounds input ~index = index >= 0 && index < (length input)

  let substr input ~span =
    let start = Loc.offset (Text.SourceSpan.start span) in
    let finish = Loc.offset (Text.SourceSpan.finish span) in
    String.sub input.data start (finish - start)

  let to_string input =
    String.sub input.data input.pos (length input)

  module Span = struct
    type t = span
    let start = Text.SourceSpan.start
    let ending = Text.SourceSpan.finish
    let merge = Text.SourceSpan.merge
  end
end
