
type t = {
    filename: string;
    line: int;
    column: int;
    offset: int
}

type source = string

let create ~filename ~line ~column ~offset = {
    filename;
    line;
    column;
    offset
}

let unknown = {
    filename = "unknown";
    line = -1;
    column = -1;
    offset = -1
}

let sources loc = [loc.filename]

let filename loc = loc.filename
let line loc = loc.line
let column loc = loc.column
let offset loc = loc.offset

let inc_column loc amount =
  { loc with column = loc.column + amount;
             offset = loc.offset + amount }

let with_filename loc filename =
  { loc with filename = filename }

let advance_to loc str end_pos =
  let line = ref (line loc) in
  let column = ref (column loc) in
  let pos = ref (offset loc) in
  let str_len = (String.length str) in
  while !pos < end_pos && !pos < str_len do
    let chr = String.get str !pos in
    match chr with
    | '\n' -> (line := !line + 1; column := 0; pos := !pos + 1)
    | '\r' -> if !pos < end_pos - 1 && String.get str (!pos + 1) = '\n' then
                (line := !line + 1; column := 0; pos := !pos + 2)
              else
                (column := !column + 1; pos := !pos + 1)
    | _ -> (column := !column + 1; pos := !pos + 1)
  done;
  create (filename loc) !line !column !pos

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

