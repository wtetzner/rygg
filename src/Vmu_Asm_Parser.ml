
open Vmu_Asm

module Location = Span.Location

module Token = struct
  type t = { span: Span.t; tok: token_type }
  and token_type =
    | Comma
    | Colon
    | Name of string
    | Literal of int
    | Directive of string
    | Equals
    | String of string

  let make typ start_pos end_pos =
    { span = Span.make start_pos end_pos; tok = typ }
end

module Lexer = struct
  let matches str pos compare =
    let str_len = String.length str in
    let comp_len = String.length compare in
    if str_len - pos < comp_len then
      false
    else
      let matches = ref true in
      for index = 0 to comp_len do
        let first = String.get str (pos + index) in
        let second = String.get compare pos in
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

  let read_name str loc =
    let start = (Location.offset loc) in
    let pos = ref (Location.offset loc) in
    pos := !pos + 1;
    while !pos < String.length str && is_name_char (String.get str !pos) do
      pos := !pos + 1
    done;
    let text = String.sub str start (!pos - start) in
    Token.make (Token.Name text) loc (Location.update loc str !pos)

  let read_string str loc =
    let start = (Location.offset loc) in
    let pos = ref (Location.offset loc) in
    pos := !pos + 1;
    while !pos < String.length str
          && (matches str !pos "\\\"" || (String.get str !pos != '"')) do
      pos := !pos + 1
    done;
    let text = String.sub str start (!pos - start) in
    Token.make (Token.String text) loc (Location.update loc str !pos)
  (* let rec read_token string loc = *)
      
end
