
open Asm
module S = Statement
module D = Directive
module E = Expression
module I = Instruction

module Location = Span.Location

exception Lexer_failure of Location.t * string
exception Parse_failure of Span.t * string

let fail_lex loc msg =
  raise (Lexer_failure (loc, msg))

let fail_parse start endp msg =
  raise (Parse_failure (Span.make start endp, msg))

let span = function | span, tok -> span
let token = function | span, tok -> tok

module Token2 = struct
  type t = Span.t * token_type
  and token_type =
    | LeftParen
    | RightParen
    | Hash
    | Comma
    | Colon
    | Name of string
    | Number of int
    | Directive of string
    | Equals
    | Plus
    | Times
    | Minus
    | Divide
    | UpperByte
    | LowerByte
    | String of string
    | R0
    | R1
    | R2
    | R3

  let starts_with str prefix =
    if (String.length str) >= (String.length prefix) then
      let capture = String.sub str 0 (String.length prefix) in
      String.equal capture prefix
    else
      false

  let str str = String str
  let name str = Name str
  let directive str = Directive str
  let number str =
    let sub s = String.sub s 1 ((String.length s) - 1) in
    match str with
    | s when starts_with str "$" ->
       Number (Scanf.sscanf (sub s) "%x" (fun x -> x))
    | s when starts_with str "%" ->
       Number (Scanf.sscanf ("0b" ^ (sub s)) "%i" (fun x -> x))
    | _ -> Number (int_of_string str)

  let indirection_mode str =
    let num = String.sub str 2 1 in
    match num with
    | "0" -> R0
    | "1" -> R1
    | "2" -> R2
    | "3" -> R3
    
  let simple str =
    match str with
    | "(" -> LeftParen
    | ")" -> RightParen
    | "#" -> Hash
    | "," -> Comma
    | ":" -> Colon
    | "=" -> Equals
    | "+" -> Plus
    | "*" -> Times
    | "-" -> Minus
    | "/" -> Divide
    | ">" -> UpperByte
    | "<" -> LowerByte

  let string_of_token_type tok =
    match tok with
    | LeftParen -> "LeftParen[(]"
    | RightParen -> "RightParen[)]"
    | Hash -> "Hash[#]"
    | Comma -> "Comma[,]"
    | Colon -> "Colon[:]"
    | Name name -> Printf.sprintf "Name[%s]" name
    | Number num -> Printf.sprintf "Number[%d]" num
    | Directive dir -> Printf.sprintf "Directive[%s]" dir
    | Equals -> "Equals[=]"
    | Plus -> "Plus[+]"
    | Times -> "Times[*]"
    | Minus -> "Minus[-]"
    | Divide -> "Divide[/]"
    | UpperByte -> "UpperByte[>]"
    | LowerByte -> "LowerByte[<]"
    | String str -> Printf.sprintf "String[%s]" str
    | R0 -> "@R0"
    | R1 -> "@R1"
    | R2 -> "@R2"
    | R3 -> "@R3"

  let to_string tok =
    Printf.sprintf "%s:%s"
      (Span.to_string (span tok))
      (string_of_token_type (token tok))

  let list_to_string toks =
    let joined = String.concat ", " (List.map to_string toks) in
    Printf.sprintf "[%s]" joined
end

module Lexer2 : sig
  val tokens : string -> Token2.t Stream.t
end = struct
  type t = {
      str: string;
      lexbuf: Sedlexing.lexbuf;
      mutable loc: Location.t
    }

  let from_string str = {
      str = str;
      lexbuf = Sedlexing.Utf8.from_string str;
      loc = Location.empty
    }

  let read_lexeme lexer =
    let buf = lexer.lexbuf in
    let lexeme = Sedlexing.Utf8.lexeme buf in
    let len = String.length lexeme in
    let loc = lexer.loc in
    let new_loc =  Location.update loc lexer.str ((Location.offset loc) + len) in
    lexer.loc <- new_loc;
    (Span.make loc new_loc, lexeme)

  let indirection_mode = [%sedlex.regexp? "@", (Chars "rR"), (Chars "0123")]
  let ident_start = [%sedlex.regexp? 'a'..'z'|'A'..'Z'|'_' ]
  let ident_char = [%sedlex.regexp? 'a'..'z'|'A'..'Z'|'_'|'0'..'9' ]
  let identifier = [%sedlex.regexp? ident_start, Star ident_char ]
  let hex_num = [%sedlex.regexp? "$", Plus ascii_hex_digit ]
  let bin_num = [%sedlex.regexp? "%", Plus ('0' | '1') ]
  let dec_num = [%sedlex.regexp? Plus ('0'..'9') ]
  let string_re = [%sedlex.regexp? "\"", Star ("\\\\" | "\\\"" | Compl ('"' | '\\')), "\"" ]
  let comment = [%sedlex.regexp? ";", Star (('\r' | Compl '\n') | (Compl (eof | '\n' | "\r"))) ]

  let token lexer =
    let buf = lexer.lexbuf in
    (match%sedlex buf with
     | Star (white_space | comment) ->
        let lexeme = Sedlexing.Utf8.lexeme buf in
        let len = String.length lexeme in
        let loc = lexer.loc in
        let new_loc = Location.update loc lexer.str ((Location.offset loc) + len) in
        lexer.loc <- new_loc
     | _ -> ());
    let read func = let span, lexeme = read_lexeme lexer in (span, (func lexeme)) in
    match%sedlex buf with
    | string_re -> Some (read Token2.str)
    | Chars "()#,:=+*-/><" -> Some (read Token2.simple)
    | indirection_mode -> Some (read Token2.indirection_mode)
    | identifier -> Some (read Token2.name)
    | hex_num | bin_num | dec_num -> Some (read Token2.number)
    | '.', identifier -> Some (read Token2.directive)
    | eof -> None
    | _ -> fail_lex lexer.loc "Unexpected character"

  let tokens str =
    let lexer = from_string str in
    Stream.from (fun _ -> token lexer)
end

module Token = struct
  type t = Span.t * token_type
  and token_type =
    | LeftParen
    | RightParen
    | Hash
    | Comma
    | Colon
    | Name of string
    | Number of int
    | Directive of string
    | Equals
    | Plus
    | Times
    | Minus
    | Divide
    | UpperByte
    | LowerByte
    | String of string
    | R0
    | R1
    | R2
    | R3

  let make typ start_pos end_pos =
    (Span.make start_pos end_pos, typ)

  let string_of_token_type tok =
    match tok with
    | LeftParen -> "LeftParen[(]"
    | RightParen -> "RightParen[)]"
    | Hash -> "Hash[#]"
    | Comma -> "Comma[,]"
    | Colon -> "Colon[:]"
    | Name name -> Printf.sprintf "Name[%s]" name
    | Number num -> Printf.sprintf "Number[%d]" num
    | Directive dir -> Printf.sprintf "Directive[%s]" dir
    | Equals -> "Equals[=]"
    | Plus -> "Plus[+]"
    | Times -> "Times[*]"
    | Minus -> "Minus[-]"
    | Divide -> "Divide[/]"
    | UpperByte -> "UpperByte[>]"
    | LowerByte -> "LowerByte[<]"
    | String str -> Printf.sprintf "String[%s]" str
    | R0 -> "@R0"
    | R1 -> "@R1"
    | R2 -> "@R2"
    | R3 -> "@R3"

  let to_string tok =
    Printf.sprintf "%s:%s"
      (Span.to_string (span tok))
      (string_of_token_type (token tok))

  let list_to_string toks =
    let joined = String.concat ", " (List.map to_string toks) in
    Printf.sprintf "[%s]" joined
end

let is_empty = function
  | [] -> true
  | _ -> false

let reverse_list list =
  let results = ref [] in
  List.iter (fun item -> results := item :: !results) list;
  !results

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

  let is_digit chr = chr >= '0' && chr <= '9'

  let is_hex_digit chr = (chr >= '0' && chr <= '9') || (chr >= 'a' && chr <= 'f') || (chr >= 'A' && chr <= 'F')

  let is_comment_char chr = chr = ';' || chr = '*'

  let read_indirection_mode str loc =
    let str_len = (String.length str) in
    let start = (Location.column loc) in
    if str_len - start < 3 then
      fail_lex loc (Printf.sprintf "Invalid indirection mode: %s" (String.sub str start str_len))
    else
      let endp = Location.update_line loc str (start + 3) in
      let r = String.get str (start + 1) in
      if r = 'r' || r = 'R' then
        match String.get str (start + 2) with
        | '0' -> Token.make Token.R0 loc endp
        | '1' -> Token.make Token.R1 loc endp
        | '2' -> Token.make Token.R2 loc endp
        | '3' -> Token.make Token.R3 loc endp
        | _ -> fail_parse loc endp (Printf.sprintf "Invalid indirection mode: %s" (String.sub str start (Location.column endp)))
      else
        fail_parse loc endp (Printf.sprintf "Invalid indirection mode: %s" (String.sub str start (Location.column endp)))

  let read_decimal str loc =
    let start = (Location.column loc) in
    let pos = ref (Location.column loc) in
    pos := !pos + 1;
    while !pos < String.length str && is_digit (String.get str !pos) do
      pos := !pos + 1
    done;
    let num_str = String.sub str start (!pos - start) in
    let num = Scanf.sscanf num_str "%d" (fun x -> x) in
    let endp = (Location.update_line loc str !pos) in
    Token.make (Token.Number num) loc endp

  let read_hex str loc =
    let start = (Location.column loc) in
    let pos = ref (Location.column loc) in
    pos := !pos + 1;
    while !pos < String.length str && is_hex_digit (String.get str !pos) do
      pos := !pos + 1
    done;
    let endp = (Location.update_line loc str !pos) in
    if !pos > start + 1 then
      let num_str = String.sub str (start + 1) (!pos - (start + 1)) in
      let num = Scanf.sscanf num_str "%x" (fun x -> x) in
      Token.make (Token.Number num) loc endp
    else
      fail_parse loc endp (Printf.sprintf "Invalid hexadecimal: %s" ((String.sub str start (!pos - start))))

  let read_binary str loc =
    let start = (Location.column loc) in
    let pos = ref (Location.column loc) in
    pos := !pos + 1;
    while !pos < String.length str && is_hex_digit (String.get str !pos) do
      pos := !pos + 1
    done;
    let endp = (Location.update_line loc str !pos) in
    if !pos > start + 1 then
      let num_str = String.sub str (start + 1) (!pos - (start + 1)) in
      let num = Scanf.sscanf ("0b" ^ num_str) "%i" (fun x -> x) in
      Token.make (Token.Number num) loc endp
    else
      fail_parse loc endp (Printf.sprintf "Invalid binary: %s" ((String.sub str start (!pos - start))))

  let read_name str loc =
    let start = (Location.column loc) in
    let pos = ref (Location.column loc) in
    pos := !pos + 1;
    while !pos < String.length str && is_name_char (String.get str !pos) do
      pos := !pos + 1
    done;
    let text = String.sub str start (!pos - start) in
    let start_chr = String.get str start in
    if start_chr = '.' then
      Token.make (Token.Directive (String.lowercase_ascii text)) loc (Location.update_line loc str !pos)
    else
      Token.make (Token.Name (String.lowercase_ascii text)) loc (Location.update_line loc str !pos)

  let read_string str loc =
    let start = (Location.column loc) in
    let pos = ref (Location.column loc) in
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
       fail_parse loc (Location.update_line loc str !pos) (Printf.sprintf "Invalid string: %s" (String.sub str start (!pos - start))));
    let text = String.sub str start (!pos - start) in
    Token.make (Token.String text) loc (Location.update_line loc str !pos)

  let skip_comment str loc =
    Location.update_line loc str ((String.length str) + 1)

  let skip_whitespace str loc =
    let start = (Location.column loc) in
    let pos = ref (Location.column loc) in
    while !pos < String.length str && is_whitespace (String.get str !pos) do
      pos := !pos + 1
    done;
    (Location.update_line loc str !pos)    

  let skip_all_whitespace str loc =
    let loc = skip_whitespace str loc in
    let offset = Location.column loc in
    let len = String.length str in
    if offset < len && (String.get str offset) = ';' then
      skip_comment str loc
    else
      loc

  let read_token str loc =
    let loc = skip_all_whitespace str loc in
    let start = (Location.column loc) in
    if start < (String.length str) - 1 then
      match String.get str start with
      | '(' -> Some (Token.make Token.LeftParen loc (Location.inc loc str))
      | ')' -> Some (Token.make Token.RightParen loc (Location.inc loc str))
      | '$' -> Some (read_hex str loc)
      | '#' -> Some (Token.make Token.Hash loc (Location.inc loc str))
      | ',' -> Some (Token.make Token.Comma loc (Location.inc loc str))
      | ':' -> Some (Token.make Token.Colon loc (Location.inc loc str))
      | '"' -> Some (read_string str loc)
      | '@' -> Some (read_indirection_mode str loc)
      | '.' -> Some (read_name str loc)
      | '=' -> Some (Token.make Token.Equals loc (Location.inc loc str))
      | '+' -> Some (Token.make Token.Plus loc (Location.inc loc str))
      | '-' -> Some (Token.make Token.Minus loc (Location.inc loc str))
      | '*' -> Some (Token.make Token.Times loc (Location.inc loc str))
      | '/' -> Some (Token.make Token.Divide loc (Location.inc loc str))
      | '>' -> Some (Token.make Token.UpperByte loc (Location.inc loc str))
      | '<' -> Some (Token.make Token.LowerByte loc (Location.inc loc str))
      | '%' -> Some (read_binary str loc)
      | chr when is_digit chr -> Some (read_decimal str loc)
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

  let read_tokens str loc =
    let results = ref [] in
    let tok = ref (read_token str loc) in
    while is_some !tok do
      let token = get_opt !tok in
      results := token :: !results;
      tok := read_token str (span token).end_pos
    done;
    reverse_list !results

  let split_lines str start_loc =
    let lines = ref [] in
    let len = (String.length str) in
    let loc = ref start_loc in
    let start = ref start_loc in
    while (Location.offset !loc) < len do
      let pos = (Location.offset !loc) in
      let p2 = pos + 1 in
      let chr = String.get str pos in
      let startp = Location.offset !start in
      match chr with
      | '\n' | '\r' when pos < len - 1 && (String.get str p2) = '\n' -> begin
          lines := (!start, (String.sub str startp (pos - startp))) :: !lines;
          loc := Location.inc (Location.inc !loc str) str;
          start := !loc
        end
      | '\n' -> begin
          lines := (!start, (String.sub str startp (pos - startp))) :: !lines;
          loc := Location.inc !loc str;
          start := !loc
        end
      | _ -> loc := Location.inc !loc str
    done;
    if (Location.offset !loc) > (Location.offset !start) then
      let pos = (Location.offset !loc) in
      let startp = Location.offset !start in
      lines := (!start, (String.sub str startp (pos - startp))) :: !lines;
    else
      ();
    reverse_list !lines

  let lex str filename =
    let start_loc = Location.with_source Location.empty filename in
    let lines: (Location.t * string) list = split_lines str start_loc in
    let results = ref [] in
    List.iter
      (function
       | loc, line ->
        let trimmed = String.trim line in
        if (String.length trimmed) > 0 && (String.get trimmed 0) != '*' then
          let tokens = read_tokens line loc in
          (if not (is_empty tokens) then
             results := (read_tokens line loc) :: !results
           else
             ())
        else
          ()
      )
      lines;
    reverse_list !results

  let load_string filename =
    let module In = Core.In_channel in
    In.with_file filename ~f:(fun f -> In.input_all f)

  let lex_file filename =
    let str = load_string filename in
    lex str filename
end

module Parser = struct
  type 'a t = (Token.t list) -> ((Token.t list) * 'a)

  let stmt span1 span2 s =
    { S.pos = Position.Span (Span.merge span1 span2); stmt = s }

  module TokenStream : sig
    type t

    val of_list : (Token.t list) -> t
    val peek : t -> (Token.t option)
    val next : t -> (Token.t option)
  end = struct
    type t = (Token.t list) ref

    let of_list toks = ref toks

    let peek tokens =
      let toks = !tokens in
      if is_empty toks then
        None
      else
        Some (List.hd toks)

    let next tokens =
      let toks = !tokens in
      if is_empty toks then
        None
      else
        let head = List.hd toks in
        tokens := List.tl toks;
        Some head
  end

end

