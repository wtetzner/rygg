
type t = {
    source: string;
    data: string;
    pos: int;
    line: int;
    column: int
  }

type regexp_match_result = {
    string: string;
    groups: string list
}

let from_string filename string = {
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

let end_loc input amount =
  let str_len = String.length input.data in
  let end_pos = (input.pos + amount) in
  let end_pos = if end_pos > str_len then str_len else end_pos in
  advance_loc (loc input) input.data end_pos

let advance_by input amount =
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

let starts_with input substr =
  let slen = (String.length substr) in
  let ilen = length input in
  if ilen < slen then
    false
  else
    starts_with_at input.data substr input.pos 0

let matches_char input chr = (String.get input.data input.pos) = chr

let matches input pred = pred (String.get input.data input.pos)

let current_char input = String.get input.data input.pos

let char_at input idx = String.get input.data (input.pos + idx)

let in_bounds input idx = idx >= 0 && idx < (length input)

let substring input span =
  let start = Loc.offset (Span.start span) in
  let finish = Loc.offset (Span.finish span) in
  String.sub input.data start (finish - start)

let to_string input =
  String.sub input.data input.pos (length input)
