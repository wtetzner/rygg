
type t = { start: int; length: int }

let to_string span =
  "{ start = " ^ (string_of_int span.start) ^ ", length = " ^ (string_of_int span.length) ^ " }"

let from ~start ~length =
  if start < 0 then
    raise (Invalid_argument (Printf.sprintf "Start of span is less than 0: start = %d, length = %d" start length))
  else if start + length < start then
    raise (Invalid_argument (Printf.sprintf "End of span must not be less than start: start = %d, length = %d" start length))
  else
  { start; length }

let from_bounds ~start ~ending =
  if start < 0 then
    raise (Invalid_argument (Printf.sprintf "Start of span is less than 0: %d" start))
  else if ending < start then
    raise (Invalid_argument (Printf.sprintf "End of span must not be less than start: start = %d, ending = %d" start ending))
  else
    from start (ending - start)

(* Start position of the span *)
let start span = span.start

(* End position of the span *)
let ending span = span.start + span.length

(* Length of the span *)
let length span = span.length

(* Check if the span is empty *)
let is_empty span = span.length = 0

(* Check if the given span contains the given position *)
let contains span ~pos = pos >= span.start && pos < (span.start + span.length)

(* Check if the given span contains `span` *)
let contains_span this ~span = span.start >= this.start && (ending span) <= (ending this)

(* Check if `span` overlaps with the given span *)
let overlaps_with this ~span =
  let start = max this.start span.start in
  let ending = min (ending this) (ending span) in
  start < ending

(* The overlap, or None if the spans don't overlap *)
let overlap this ~span =
  let start = max this.start span.start in
  let ending = min (ending this) (ending span) in
  if start < ending then
    Some (from_bounds start ending)
  else
    None

(* Check if `span` intersects with the given span *)
let intersects_with this ~span = span.start <= (ending this) && (ending span) >= this.start

(* Check if `pos` intersects with the given span *)
let intersects_with_pos this ~pos = pos >= this.start && pos <= (ending this)

(* The intersection, or None if they don't intersect *)
let intersection this ~span =
  let start = max this.start span.start in
  let ending = min (ending this) (ending span) in
  if start <= ending then
    Some (from_bounds start ending)
  else
    None

let equal span1 span2 =
  span1.start = span2.start && span1.length = span2.length

let compare span1 span2 =
  let start_diff = span1.start - span2.start in
  if diff = 0 then
    span1.length - span2.length
  else
    diff
