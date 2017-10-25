
type t = {
    source : string;
    line   : int;
    column : int
}

let source loc = loc.source
let line loc = loc.line
let column loc = loc.column

let create source line column = {
    source = source;
    line = line;
    column = column
}

let to_string loc =
  Printf.sprintf "%s:%d:%d" loc.source loc.line loc.column

let empty = {
    source = "unknown";
    line = 0;
    column = 0
}
