
module Loc = SourceLoc

type t = { start: Loc.t; finish: Loc.t }
type source = string

let from start finish = { start; finish }

let unknown = { start = Loc.unknown; finish = Loc.unknown }

let start span = span.start
let finish span = span.finish

let sources span = [Loc.filename span.start; Loc.filename span.finish]

let singular loc = from loc loc
let is_singular span = Loc.equal span.start span.finish

let slice span =
  let start_idx = Loc.offset (start span) in
  (start_idx, (Loc.offset (finish span)) - start_idx)

let substr span text =
  let (start, length) = slice span in
  String.sub text start length

let length span =
  (Loc.offset span.finish) - (Loc.offset span.start)

let equal span1 span2 =
  Loc.equal span1.start span2.start
  && Loc.equal span1.finish span2.finish

let compare span1 span2 =
  let start_cmp = Loc.compare span1.start span2.start in
  if start_cmp = 0 then
    Loc.compare span1.finish span2.finish
  else
    start_cmp

let merge span1 span2 = { start = span1.start; finish = span2.finish }

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
