
module StringName = struct
  type t = string

  let equal = String.equal
  let compare = String.compare
  let to_string str = str
  let debug_string str = "\"" ^ str ^ "\""
end

module Loc: sig
  type t
  type source = string

  val create : string -> int -> int -> int -> t

  val sources : t -> string list

  val filename : t -> string
  val line : t -> int
  val column : t -> int
  val offset : t -> int

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val debug_string : t -> string
end = struct
  type t = {
      filename: string;
      line: int;
      column: int;
      offset: int
  }
  type source = string

  let create filename line column offset = {
      filename;
      line;
      column;
      offset
  }

  let sources loc = [loc.filename]

  let filename loc = loc.filename
  let line loc = loc.line
  let column loc = loc.column
  let offset loc = loc.offset

  let equal loc1 loc2 =
    String.equal loc1.filename loc2.filename
    && loc1.line = loc2.line
    && loc1.column = loc2.column
    && loc1.offset = loc2.offset

  let compare loc1 loc2 =
    let file_cmp = String.compare loc1.filename loc2.filename in
    if file_cmp = 0 then
      Int.compare loc1.offset loc2.offset
    else
      file_cmp

  let to_string loc =
    loc.filename ^ ":" ^ (string_of_int loc.line) ^ ":" ^ (string_of_int loc.column)

  let debug_string loc =
    Printf.sprintf "{ filename = %s; line = %d; column = %d; offset = %d }"
      loc.filename
      loc.line
      loc.column
      loc.offset
end

module Span: sig
  type t
  type source = string

  val from : Loc.t -> Loc.t -> t

  val start : t -> Loc.t
  val finish : t -> Loc.t

  val sources : t -> source list

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val debug_string : t -> string
end = struct
  type t = { start: Loc.t; finish: Loc.t }
  type source = string

  let from start finish = { start; finish }

  let start span = span.start
  let finish span = span.finish

  let sources span = [Loc.filename span.start; Loc.filename span.finish]

  let equal span1 span2 =
    Loc.equal span1.start span2.start
    && Loc.equal span1.finish span2.finish

  let compare span1 span2 =
    let start_cmp = Loc.compare span1.start span2.start in
    if start_cmp = 0 then
      Loc.compare span1.finish span2.finish
    else
      start_cmp

  let to_string span =
    let { start; finish } = span in
    if Loc.equal start finish then
      Loc.to_string start
    else if String.equal (Loc.filename start) (Loc.filename finish) then
      if (Loc.line start) = (Loc.line finish) then
        Printf.sprintf "%s:%d:%d-%d"
          (Loc.filename start)
          (Loc.line start)
          (Loc.column start)
          (Loc.column finish)
      else
        Printf.sprintf "%s:%d:%d-%d:%d"
          (Loc.filename start)
          (Loc.line start)
          (Loc.column start)
          (Loc.line finish)
          (Loc.column finish)
    else
      Printf.sprintf "%s-%s" (Loc.to_string start) (Loc.to_string finish)

  let debug_string span = Printf.sprintf "{ start = %s; finish = %s }"
                            (Loc.debug_string span.start)
                            (Loc.debug_string span.finish)
end
