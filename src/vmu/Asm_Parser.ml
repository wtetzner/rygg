
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

module Lexer : sig
  val tokens : string -> Token.t Stream.t
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
    | string_re -> Some (read Token.str)
    | Chars "()#,:=+*-/><" -> Some (read Token.simple)
    | indirection_mode -> Some (read Token.indirection_mode)
    | identifier -> Some (read Token.name)
    | hex_num | bin_num | dec_num -> Some (read Token.number)
    | '.', identifier -> Some (read Token.directive)
    | eof -> None
    | _ -> fail_lex lexer.loc "Unexpected character"

  let tokens str =
    let lexer = from_string str in
    Stream.from (fun _ -> token lexer)
end

module Parser = struct
  let stmt span1 span2 s =
    { S.pos = Position.Span (Span.merge span1 span2); stmt = s }
end

