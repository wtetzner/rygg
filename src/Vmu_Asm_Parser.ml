
open Vmu_Asm

module Location = Span.Location

exception Lexer_failure of Location.t * string
exception Parse_failure of Span.t * string

let fail_lex loc msg =
  raise (Lexer_failure (loc, msg))

let fail_parse start endp msg =
  raise (Parse_failure (Span.make start endp, msg))

module Token = struct
  type t = { span: Span.t; tok: token_type }
  and token_type =
    | Comma
    | Colon
    | Name of string
    | Literal of string
    | Directive of string
    | Equals
    | String of string
    | R0
    | R1
    | R2
    | R3

  let make typ start_pos end_pos =
    { span = Span.make start_pos end_pos; tok = typ }

  let string_of_token_type tok =
    match tok with
    | Comma -> "Comma[,]"
    | Colon -> "Colon[:]"
    | Name name -> Printf.sprintf "Name[%s]" name
    | Literal num -> Printf.sprintf "Literal[%s]" num
    | Directive dir -> Printf.sprintf "Directive[%s]" dir
    | Equals -> "Equals[=]"
    | String str -> Printf.sprintf "String[%s]" str
    | R0 -> "@R0"
    | R1 -> "@R1"
    | R2 -> "@R2"
    | R3 -> "@R3"

  let to_string tok =
    Printf.sprintf "%s:%s"
      (Span.to_string tok.span)
      (string_of_token_type tok.tok)

  let list_to_string toks =
    let joined = String.concat ", " (List.map to_string toks) in
    Printf.sprintf "[%s]" joined
end

module Lexer = struct
  let matches str pos compare =
    let str_len = String.length str in
    let comp_len = String.length compare in
    if str_len - pos < comp_len then
      false
    else
      let matches = ref true in
      for index = 0 to comp_len - 1 do
        let first = String.get str (pos + index) in
        let second = String.get compare index in
        if first != second then
          matches := false
        else
          ()
      done;
      !matches

  let is_newline str pos =
    let chr = String.get str !pos in
    chr = '\n' || matches str !pos "\r\n"

  let is_whitespace chr =
    chr = '\n' || chr = '\r' || chr = ' ' || chr = '\t'

  let is_name_start chr =
    (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z')
    || chr = '_'

  let is_name_char chr =
    (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z')
    || (chr >= '0' && chr <= '9') || chr = '_'

  let is_comment_char chr = chr = ';' || chr = '*'

  let read_indirection_mode str loc =
    let str_len = (String.length str) in
    let start = (Location.offset loc) in
    if str_len - start < 3 then
      fail_lex loc (Printf.sprintf "Invalid indirection mode: %s" (String.sub str start str_len))
    else
      let endp = Location.update loc str (start + 3) in
      let r = String.get str (start + 1) in
      if r = 'r' || r = 'R' then
        match String.get str (start + 2) with
        | '0' -> Token.make Token.R0 loc endp
        | '1' -> Token.make Token.R1 loc endp
        | '2' -> Token.make Token.R2 loc endp
        | '3' -> Token.make Token.R3 loc endp
        | _ -> fail_parse loc endp (Printf.sprintf "Invalid indirection mode: %s" (String.sub str start (Location.offset endp)))
      else
        fail_parse loc endp (Printf.sprintf "Invalid indirection mode: %s" (String.sub str start (Location.offset endp)))

  let read_name str loc =
    let start = (Location.offset loc) in
    let pos = ref (Location.offset loc) in
    pos := !pos + 1;
    while !pos < String.length str && is_name_char (String.get str !pos) do
      pos := !pos + 1
    done;
    let text = String.sub str start (!pos - start) in
    let start_chr = String.get str start in
    if start_chr = '.' then
      Token.make (Token.Directive text) loc (Location.update loc str !pos)
    else
      Token.make (Token.Name text) loc (Location.update loc str !pos)

  let read_string str loc =
    let start = (Location.offset loc) in
    let pos = ref (Location.offset loc) in
    pos := !pos + 1;
    while !pos < String.length str
          && (matches str !pos "\\\"" || (String.get str !pos != '"')) do
      if matches str !pos "\\\"" then
        pos := !pos + 2
      else
        pos := !pos + 1
    done;
    (if !pos <= (String.length str) && (String.get str !pos) = '"' then
       pos := !pos + 1
     else
       fail_parse loc (Location.update loc str !pos) (Printf.sprintf "Invalid string: %s" (String.sub str start (!pos - start))));
    let text = String.sub str start (!pos - start) in
    Token.make (Token.String text) loc (Location.update loc str !pos)

  let skip_whitespace str loc =
    let start = (Location.offset loc) in
    let pos = ref (Location.offset loc) in
    while !pos < String.length str && is_whitespace (String.get str !pos) do
      pos := !pos + 1
    done;
    (Location.update loc str !pos)

  let read_token str loc =
    let loc = skip_whitespace str loc in
    let start = (Location.offset loc) in
    if start < (String.length str) - 1 then
      match String.get str start with
      | ',' -> Some (Token.make Token.Comma loc (Location.inc_column loc))
      | ':' -> Some (Token.make Token.Colon loc (Location.inc_column loc))
      | '"' -> Some (read_string str loc)
      | '@' -> Some (read_indirection_mode str loc)
      | '.' -> Some (read_name str loc)
      | chr when is_name_start chr -> Some (read_name str loc)
      | chr -> fail_lex loc (Printf.sprintf "Unexpected character: '%c'" chr)
    else
      None

  let is_some opt =
    match opt with
    | Some _ -> true
    | None -> false

  let get_opt opt =
    match opt with
    | Some x -> x
    | None -> raise Not_found

  let read_tokens str loc results =
    let tok = ref (read_token str loc) in
    while is_some !tok do
      let token = get_opt !tok in
      results := token :: !results;
      tok := read_token str token.span.end_pos
    done

  let lex str filename =
    let loc = ref (Location.with_source Location.empty filename) in
    let lines = Str.split (Str.regexp "\r\n\\|\n") str in
    let results = ref [] in
    List.iter
      (fun line -> read_tokens line !loc results; loc := Location.inc_line !loc)
      lines;
    List.rev !results
end
