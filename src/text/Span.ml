
type t = { start: int; length: int }

let to_string span =
  "[" ^ (string_of_int span.start) ^ ".." ^ (string_of_int (span.start + span.length)) ^ ")"

let debug_string span =
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
let contains span ~pos =
  pos >= span.start && pos < (span.start + span.length)

(* Check if the given span contains `span` *)
let contains_span this ~span =
  span.start >= this.start && (ending span) <= (ending this)

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
let intersects_with this ~span =
  span.start <= (ending this) && (ending span) >= this.start

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
  if start_diff = 0 then
    span1.length - span2.length
  else
    start_diff

(********** Unit Tests **********)
let%test_unit "from negative length" =
  try begin
      ignore (from ~start:10 ~length:(-1));
      raise (Failure "Expected Invalid_argument exception")
    end
  with Invalid_argument _ -> ()

(* from *)
let%test_unit "from negative start" =
  try begin
      ignore (from ~start:(-10) ~length:10);
      raise (Failure "Expected Invalid_argument exception")
    end
  with Invalid_argument _ -> ()

let%test_unit "from" = ignore (from ~start:0 ~length:10)
let%test_unit "from empty" = ignore (from ~start:3 ~length:0)

(* from_bounds *)
let%test_unit "from_bounds" = ignore (from_bounds ~start:0 ~ending:10)
let%test_unit "from_bounds empty" = ignore (from_bounds ~start:5 ~ending:5)

let%test_unit "from_bounds negative start" =
  try begin
      ignore (from_bounds ~start:(-1) ~ending:10);
      raise (Failure "Expected Invalid_argument exception")
    end
  with Invalid_argument _ -> ()

let%test_unit "from_bounds start greater than end" =
  try begin
      ignore (from_bounds ~start:10 ~ending:5);
      raise (Failure "Expected Invalid_argument exception")
    end
  with Invalid_argument _ -> ()

(* to_string *)
let%test "to_string" =
  let span = from 4 6 in
  String.equal (to_string span) "[4..10)"

let%test "to_string empty" =
  let span = from 4 0 in
  String.equal (to_string span) "[4..4)"

(* start *)
let%test "start of span = 2" =
  let span = from 1 3 in
  (start span) = 1

(* ending *)
let%test "ending of span" =
  let span = from 1 3 in
  (ending span) = 4

(* length *)
let%test "length of span" =
  let span = from 1 3 in
  (length span) = 3

let%test "length of empty span" =
  let span = from 1 0 in
  (length span) = 0

(* is_empty *)
let%test "is_empty" =
  let span = from 1 0 in
  (is_empty span)

let%test "is_empty" =
  let span = from 1 3 in
  (is_empty span) = false

(* contains *)
let%test "contains [1..5) ~pos:5 = false" =
  let span = from 1 4 in
  (contains span ~pos:5) = false

let%test "contains [1..5) ~pos:4" =
  let span = from 1 4 in
  (contains span ~pos:4)

let%test "contains [1..1) ~pos:1" =
  let span = from 1 0 in
  (contains span ~pos:1) = false

(* contains_span *)
let%test "contains_span s s" =
  let outer = from_bounds 3 12 in
  let inner = from_bounds 3 12 in
  (contains_span outer inner)

let%test "contains_span left" =
  let outer = from_bounds 3 12 in
  let inner = from_bounds 2 12 in
  (contains_span outer inner) = false

let%test "contains_span right" =
  let outer = from_bounds 3 12 in
  let inner = from_bounds 3 13 in
  (contains_span outer inner) = false

(* overlaps_with *)
let%test "overlaps_with s s" =
  let span1 = from_bounds 3 7 in
  let span2 = from_bounds 3 7 in
  overlaps_with span1 span2

let%test "overlaps_with left" =
  let span1 = from_bounds 3 7 in
  let span2 = from_bounds 2 4 in
  overlaps_with span1 span2

let%test "overlaps_with right" =
  let span1 = from_bounds 3 7 in
  let span2 = from_bounds 6 9 in
  overlaps_with span1 span2

let%test "overlaps_with edge = false" =
  let span1 = from_bounds 3 7 in
  let span2 = from_bounds 7 9 in
  (overlaps_with span1 span2) = false

(* overlap *)
let%test "overlap s s" =
  let span1 = from_bounds 3 7 in
  let span2 = from_bounds 3 7 in
  let Some overlap_span = overlap span1 span2 in
  (equal overlap_span span1) && (equal overlap_span span2)

let%test "overlap left" =
  let span1 = from_bounds 3 7 in
  let span2 = from_bounds 4 9 in
  let Some overlap_span = overlap span1 span2 in
  (equal overlap_span (from_bounds 4 7))

let%test "overlap right" =
  let span1 = from_bounds 3 7 in
  let span2 = from_bounds 2 5 in
  let Some overlap_span = overlap span1 span2 in
  (equal overlap_span (from_bounds 3 5))

(* intersects_with *)
let%test "intersects_with s s" =
  let span1 = from_bounds 3 7 in
  let span2 = from_bounds 3 7 in
  intersects_with span1 span2
