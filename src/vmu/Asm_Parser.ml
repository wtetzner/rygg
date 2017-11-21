
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

let fail_parse span msg = raise (Parse_failure (span, msg))

let get_span = function | span, tok -> span
let get_token = function | span, tok -> tok
let get_some opt =
  match opt with
  | Some x -> x
  | None -> raise (Failure "Called get_some on None")

module Token = struct
  type t = Span.t * token_type [@@deriving show {with_path=false}, ord, eq]
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
    | EOF [@@deriving show {with_path=false}, ord, eq]

  let precedence token =
    match get_token token with
    | Plus | Minus -> 1
    | Times | Divide -> 2
    | _ -> raise (Failure "Not an operator")

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
  let eof str = EOF

  let indirection_mode str =
    let num = String.sub str 2 1 in
    match num with
    | "0" -> R0
    | "1" -> R1
    | "2" -> R2
    | "3" -> R3
    | _ -> raise (Failure (Printf.sprintf "Unknown indirection_mode: %s" str))
    
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
    | _ -> raise (Failure (Printf.sprintf "Unknown simple token: %s" str))

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
    | EOF -> "<EOF>"

  let to_string tok =
    Printf.sprintf "%s:%s"
      (Span.to_string (get_span tok))
      (string_of_token_type (get_token tok))
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
    | eof -> Some (read Token.eof)
    | _ -> fail_lex lexer.loc "Unexpected character"

  let tokens str =
    let lexer = from_string str in
    let seen_eof = ref false in
    let next _ =
      let result = token lexer in
      match result with
      | Some tok ->
         if Token.equal_token_type (get_token tok) Token.EOF then
           (if !seen_eof then
              None
            else
              (seen_eof := true;
               result))
         else
           result
      | None -> None in
    Stream.from next
end

module Parser = struct
  let stmt span1 span2 s =
    { S.pos = Position.Span (Span.merge span1 span2); stmt = s }

  let drop1 tokens = ignore (Stream.next tokens)
  let dropn n tokens =
    let current = ref 0 in
    while !current < n do
      ignore (Stream.next tokens);
      current := !current + 1
    done

  let do_match token_type tokens =
    let tok: Token.t option = Stream.peek tokens in
    match tok with
    | Some t when Token.equal_token_type token_type (get_token t) -> ()
    | Some t when Token.equal_token_type Token.EOF (get_token t) ->
       fail_parse (get_span t) "Unexpected EOF"
    | Some t -> fail_parse (get_span t) "Unexpected token"
    | None -> raise (Failure "Unexpected EOF")

  let is_binary tok =
    let tok_type = match tok with
      | Some t -> Some (get_token t)
      | _ -> None in
    match tok_type with
    | Some Token.Plus
      | Some Token.Times
      | Some Token.Minus
      | Some Token.Divide -> true
    | _ -> false

  module ExprParser = struct
    let rec expression tokens =
    and term tokens =
    and product tokens =
      let tok = Stream.peek tokens in
      match tok with
      | Some tok ->
         (match get_token tok with
          | Token.LeftParen ->
             (let result = expression tokens in
              do_match Token.RightParen tokens;
              dropn 3 tokens;
              result)
          | Token.Number n ->
             (drop1 tokens;
              Some (E.(from (get_span tok) (Number n))))
          | Token.Name n ->
             (drop1 tokens;
              Some (E.(from (get_span tok) (Name n)))))
      | None -> raise (Failure "Unexpected EOF")
  end

  let rec parse_expr tokens precedence =
    let expr = parse_basic tokens in
    match expr with
    | Some exp ->
       let tok = Stream.peek tokens in
       if is_binary tok then
         parse_binary exp tokens precedence
       else
         expr
    | None -> None
  and parse_basic tokens =
    match Stream.peek tokens with
    | Some tok ->
       (match get_token tok with
        | Token.LeftParen ->
           (let result = parse_expr tokens 3 in
            do_match Token.RightParen tokens;
            dropn 3 tokens;
            result)
        | Token.Number n ->
           (drop1 tokens;
            Some (E.(from (get_span tok) (Number n))))
        | Token.Name n ->
           (drop1 tokens;
            Some (E.(from (get_span tok) (Name n)))))
    | None -> None
  and parse_binary left tokens precedence =
    Printf.printf "parse_binary [%s] _ %d" (Expression.to_string left) precedence;
    print_newline ();
    let tok = get_some (Stream.peek tokens) in
    let new_prec = (Token.precedence tok) in
    if new_prec <= precedence then
      (drop1 tokens;
       let right = get_some (parse_basic tokens) in
       let nspan = Span.merge
                     (Expression.span left)
                     (Expression.span right) in
       let expr = match get_token tok with
         | Token.Plus ->
            (Some E.(from nspan (Plus (left, right))))
         | Token.Minus ->
            (Some E.(from nspan (Minus (left, right))))
         | Token.Times ->
            (Some E.(from nspan (Times (left, right))))
         | Token.Divide ->
            (Some E.(from nspan (Divide (left, right)))) in
       match expr with
       | Some exp ->
          let tok = Stream.peek tokens in
          if is_binary tok then
            parse_binary exp tokens new_prec
          else
            expr
       | None -> None)
    else
      (drop1 tokens;
       let right = get_some (parse_expr tokens new_prec) in
       let nspan = Span.merge
                     (Expression.span left)
                     (Expression.span right) in
       match get_token tok with
       | Token.Plus ->
          (Some E.(from nspan (Plus (left, right))))
       | Token.Minus ->
          (Some E.(from nspan (Minus (left, right))))
       | Token.Times ->
          (Some E.(from nspan (Times (left, right))))
       | Token.Divide ->
          (Some E.(from nspan (Divide (left, right)))))
end

