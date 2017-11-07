
module Location = struct
  type t = {
      source : string;
      line   : int;
      column : int;
      offset : int
    }

  let source loc = loc.source
  let line loc = loc.line
  let column loc = loc.column
  let offset loc = loc.offset

  let create source line column offset = {
      source = source;
      line = line;
      column = column;
      offset = offset
    }

  let to_string loc =
    Printf.sprintf "%s:%d:%d" loc.source loc.line loc.column

  let empty = {
      source = "unknown";
      line = 1;
      column = 0;
      offset = 0;
    }

  let update loc str end_pos =
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
    create (source loc) !line !column !pos

  let inc_line loc = { loc with line = loc.line + 1; column = 0 }

  let inc_column loc = { loc with column = loc.column + 1; offset = loc.offset + 1 }

  let with_source loc source = { loc with source = source }

  let merge left right = left
end

type t = { start_pos: Location.t; end_pos: Location.t }

let make start_pos end_pos = {
    start_pos = start_pos;
    end_pos = end_pos
  }

let merge left right =
  make left.start_pos right.end_pos

let to_string span =
  let start = span.start_pos in
  let endp = span.end_pos in
  if start.line = endp.line then
    Printf.sprintf "%s:%d:%d-%d"
      start.source
      start.line
      start.column
      endp.column
  else
    Printf.sprintf "%s:%d:%d-%d:%d"
      start.source
      start.line
      start.column
      endp.line
      endp.column

let to_colored_string span =
  let start = span.start_pos in
  let endp = span.end_pos in
  if start.line = endp.line then
    ANSITerminal.(sprintf [yellow] "%s" start.source) ^ ":" ^
      ANSITerminal.(sprintf [magenta] "%d:%d-%d"
                      start.line
                      start.column
                      endp.column)
  else
    ANSITerminal.(sprintf [yellow] "%s" start.source) ^ ":" ^
      ANSITerminal.(sprintf [Bold] "%d:%d-%d:%d"
                      start.line
                      start.column
                      endp.line
                      endp.column)
