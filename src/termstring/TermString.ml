
type part =
  | PlainString of string
  | StyleString of (ANSITerminal.style list) * string
  | Parts of part list

type t = part list

let styled styles str = [StyleString (styles, str)]

let of_string str = [PlainString str]

let (^) left right = [Parts [Parts left; Parts right]]

let concat parts = [Parts parts]

let is_empty list =
  match list with
  | [] -> true
  | _ -> false

let rec append_string buffer parts =
  let parts = ref parts in
  while not (is_empty !parts) do
    let part = List.hd !parts in
    parts := List.tl !parts;
    match part with
    | PlainString str -> Buffer.add_string buffer str
    | StyleString (_, str) -> Buffer.add_string buffer str
    | Parts ps -> append_string buffer ps
  done

let to_string parts =
  let buffer = Buffer.create 50 in
  append_string buffer parts;
  Buffer.contents buffer

let rec print_string_int parts =
  let parts = ref parts in
  while not (is_empty !parts) do
    let part = List.hd !parts in
    parts := List.tl !parts;
    match part with
    | PlainString str -> print_string str
    | StyleString (styles, str) -> ANSITerminal.(print_string styles str)
    | Parts ps -> print_string_int !parts
  done

let print_string parts = print_string_int parts

