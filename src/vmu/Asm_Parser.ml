
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
  let name str = Name (String.lowercase_ascii str)
  let directive str = Directive (String.lowercase_ascii str)
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
    let eof = ref None in
    let next _ =
      let result = token lexer in
      match result with
      | Some tok ->
         if Token.equal_token_type (get_token tok) Token.EOF then
           (eof := Some tok;
            result)
         else
           result
      | None -> !eof in
    Stream.from next
end

module Parser = struct
  let stmt span1 span2 s =
    { S.pos = Position.Span (Span.merge span1 span2); stmt = s }

  let drop1 tokens = Stream.junk tokens
  let dropn n tokens =
    let current = ref 0 in
    while !current < n do
      Stream.junk tokens;
      current := !current + 1
    done

  let peek tokens =
    let tok = Stream.peek tokens in
    match tok with
    | Some tok -> tok
    | None -> raise (Failure "Unexpected End of Stream")

  let npeek n tokens = Stream.npeek n tokens

  let do_match token_type tokens =
    let tok: Token.t = peek tokens in
    match tok with
    | t when Token.equal_token_type token_type (get_token t) ->
       drop1 tokens
    | t when Token.equal_token_type Token.EOF (get_token t) ->
       fail_parse (get_span t) "Unexpected EOF"
    | t -> fail_parse (get_span t) "Unexpected token"

  let rec expression tokens =
    let left = term tokens in
    let tok = peek tokens in
    match tok with
    | span, Token.Plus ->
       (drop1 tokens;
        let right = expression tokens in
        let nspan = Span.merge
                      (Expression.span left)
                      (Expression.span right) in
        E.(from nspan (Plus (left, right))))
    | span, Token.Minus ->
       (drop1 tokens;
        let right = expression tokens in
        let nspan = Span.merge
                      (Expression.span left)
                      (Expression.span right) in
        E.(from nspan (Minus (left, right))))
    | _ -> left
  and term tokens =
    let left = factor tokens in
    let tok = peek tokens in
    match tok with
    | span, Token.Times ->
       (drop1 tokens;
        let right = term tokens in
        let nspan = Span.merge
                      (Expression.span left)
                      (Expression.span right) in
        E.(from nspan (Times (left, right))))
    | span, Token.Divide ->
       (drop1 tokens;
        let right = term tokens in
        let nspan = Span.merge
                      (Expression.span left)
                      (Expression.span right) in
        E.(from nspan (Divide (left, right))))
    | _ -> left
  and factor tokens =
    let tok = peek tokens in
    match get_token tok with
    | Token.LeftParen ->
       (drop1 tokens;
        let result = expression tokens in
        do_match Token.RightParen tokens;
        result)
    | Token.Number n ->
       (drop1 tokens;
        E.(from (get_span tok) (Number n)))
    | Token.Name n ->
       (drop1 tokens;
        E.(from (get_span tok) (Name n)))
    | Token.UpperByte ->
       (drop1 tokens;
        let expr = factor tokens in
        E.(from (get_span tok) (UpperByte expr)))
    | Token.LowerByte ->
       (drop1 tokens;
        let expr = factor tokens in
        E.(from (get_span tok) (LowerByte expr)))
    | Token.EOF -> fail_parse (get_span tok) "Unexpected EOF"
    | _ -> fail_parse (get_span tok) "Unexpected Token"

  let indirection_mode tokens =
    let tok = peek tokens in
    match get_token tok with
    | Token.R0 -> IndirectionMode.R0
    | Token.R1 -> IndirectionMode.R1
    | Token.R2 -> IndirectionMode.R2
    | Token.R3 -> IndirectionMode.R3
    | Token.EOF -> fail_parse (get_span tok) "Unexpected EOF"
    | _ -> fail_parse (get_span tok) "Expected Indirection Mode"

  let i8 tokens =
    let tok: Token.t = peek tokens in
    match get_token tok with
    | Token.Hash ->
       (drop1 tokens;
        expression tokens)
    | Token.EOF -> fail_parse (get_span tok) "Unexpected EOF"
    | _ -> fail_parse (get_span tok) "Expected Immediate value"

  let is_empty list =
    match list with
    | [] -> true
    | _ -> false

  module ArgType = struct
    type t =
      | I8
      | D9
      | Ri
  end

  let match_triple_expr tokens constr =
    let name = peek tokens in
    drop1 tokens;
    let expr1 = expression tokens in
    do_match Token.Comma tokens;
    let expr2 = expression tokens in
    do_match Token.Comma tokens;
    let expr3 = expression tokens in
    let instr = S.Instruction (constr expr1 expr2 expr3) in
    stmt (get_span name) (E.span expr3) instr

  let match_single_3 tokens constr_i8 constr_d9 constr_ind =
    let name :: arg :: [] = npeek 2 tokens in
    match get_token arg with
    | Token.Hash ->
       (drop1 tokens;
        let expr = i8 tokens in
        let inst = S.Instruction (constr_i8 expr) in
        stmt (get_span name) (E.span expr) inst)
    | Token.R0 | Token.R1 | Token.R2 | Token.R3 ->
       (drop1 tokens;
        let indir = indirection_mode tokens in
        let inst = S.Instruction (constr_ind indir) in
        stmt (get_span name) (get_span arg) inst)
    | Token.EOF -> fail_parse (get_span arg) "Unexpected EOF"
    | _ ->
       (drop1 tokens;
        let expr = expression tokens in
        let instr = S.Instruction (constr_d9 expr) in
        stmt (get_span name) (E.span expr) instr)

  let match_single_2 tokens constr_d9 constr_ind =
    let name :: arg :: [] = npeek 2 tokens in
    match get_token arg with
    | Token.R0 | Token.R1 | Token.R2 | Token.R3 ->
       (drop1 tokens;
        let indir = indirection_mode tokens in
        let inst = S.Instruction (constr_ind indir) in
        stmt (get_span name) (get_span arg) inst)
    | Token.EOF -> fail_parse (get_span arg) "Unexpected EOF"
    | _ ->
       (drop1 tokens;
        let expr = expression tokens in
        let instr = S.Instruction (constr_d9 expr) in
        stmt (get_span name) (E.span expr) instr)

  let match_single_expr tokens constr =
    let name :: arg :: [] = npeek 2 tokens in
    match get_token arg with
    | Token.EOF -> fail_parse (get_span arg) "Unexpected EOF"
    | _ ->
       (drop1 tokens;
        let expr = expression tokens in
        let instr = S.Instruction (constr expr) in
        stmt (get_span name) (E.span expr) instr)

  let match_double_expr tokens constr =
    let name = peek tokens in
    drop1 tokens;
    let expr1 = expression tokens in
    do_match Token.Comma tokens;
    let expr2 = expression tokens in
    

  let match_dbnz tokens constr_d9 constr_ri =
    let name = peek tokens in
    drop1 tokens;
    let tok = peek tokens in
    match get_token tok with
    | Token.EOF -> fail_parse (get_span tok) "Unexpected EOF"
    | Token.R0 | Token.R1 | Token.R2 | Token.R3 -> begin
        let indir = indirection_mode tokens in
        do_match Token.Comma tokens;
        let expr = expression tokens in
        let instr = S.Instruction (constr_ri indir expr) in
        stmt (get_span name) (E.span expr) instr
      end
    | _ ->
       let expr1 = expression tokens in
       do_match Token.Comma tokens;
       let expr2 = expression tokens in
       let instr = S.Instruction (constr_d9 expr1 expr2) in
       stmt (get_span name) (E.span expr2) instr

  let match_move tokens constr_exp constr_indir =
    let name = peek tokens in
    drop1 tokens;
    let expr1 = i8 tokens in
    do_match Token.Comma tokens;
    let tok = peek tokens in
    match get_token tok with
    | Token.R0 | Token.R1 | Token.R2 | Token.R3 -> begin
        let indir = indirection_mode tokens in
        let instr = S.Instruction (constr_indir expr1 indir) in
        stmt (get_span name) (get_span tok) instr
      end
    | Token.EOF -> fail_parse (get_span tok) "Unexpected EOF"
    | _ ->
       let expr2 = expression tokens in
       let instr = S.Instruction (constr_exp expr1 expr2) in
       stmt (get_span name) (E.span expr2) instr

  let instruction tokens =
    let tok = peek tokens in
    let make expr instr =
      stmt (get_span tok) (E.span expr) (S.Instruction instr) in
    match get_token tok with
    | Token.Name name -> begin
        match name with
        | "add" -> match_single_3 tokens
                     (fun e -> I.Add_i8 e)
                     (fun e -> I.Add_d9 e)
                     (fun e -> I.Add_Ri e)
        | "addc" -> match_single_3 tokens
                      (fun e -> I.Addc_i8 e)
                      (fun e -> I.Addc_d9 e)
                      (fun e -> I.Addc_Ri e)
        | "sub" -> match_single_3 tokens
                     (fun e -> I.Sub_i8 e)
                     (fun e -> I.Sub_d9 e)
                     (fun e -> I.Sub_Ri e)
        | "subc" -> match_single_3 tokens
                      (fun e -> I.Subc_i8 e)
                      (fun e -> I.Subc_d9 e)
                      (fun e -> I.Subc_Ri e)
        | "inc" -> match_single_2 tokens
                     (fun e -> I.Inc_d9 e)
                     (fun e -> I.Inc_Ri e)
        | "dec" -> match_single_2 tokens
                     (fun e -> I.Inc_d9 e)
                     (fun e -> I.Inc_Ri e)
        | "mul" -> stmt (get_span tok) (get_span tok) (S.Instruction I.Mul)
        | "div" -> stmt (get_span tok) (get_span tok) (S.Instruction I.Div)
        | "and" -> match_single_3 tokens
                     (fun e -> I.And_i8 e)
                     (fun e -> I.And_d9 e)
                     (fun e -> I.And_Ri e)
        | "or" -> match_single_3 tokens
                    (fun e -> I.Or_i8 e)
                    (fun e -> I.Or_d9 e)
                    (fun e -> I.Or_Ri e)
        | "xor" -> match_single_3 tokens
                     (fun e -> I.Xor_i8 e)
                     (fun e -> I.Xor_d9 e)
                     (fun e -> I.Xor_Ri e)
        | "rol" -> stmt (get_span tok) (get_span tok) (S.Instruction I.Rol)
        | "rolc" -> stmt (get_span tok) (get_span tok) (S.Instruction I.Rolc)
        | "ror" -> stmt (get_span tok) (get_span tok) (S.Instruction I.Ror)
        | "rorc" -> stmt (get_span tok) (get_span tok) (S.Instruction I.Rorc)
        | "ld" -> match_single_2 tokens
                    (fun e -> I.Ld_d9 e)
                    (fun e -> I.Ld_Ri e)
        | "st" -> match_single_2 tokens
                    (fun e -> I.St_d9 e)
                    (fun e -> I.St_Ri e)
        | "mov" -> match_move tokens
                     (fun e1 e2 -> I.Mov_d9 (e1, e2))
                     (fun e1 e2 -> I.Mov_Rj (e1, e2))
        | "ldc" -> stmt (get_span tok) (get_span tok) (S.Instruction I.Ldc)
        | "push" -> match_single_expr tokens (fun e -> I.Push e)
        | "pop" -> match_single_expr tokens (fun e -> I.Pop e)
        | "xch" -> match_single_2 tokens
                     (fun e -> I.Xch_d9 e)
                     (fun e -> I.Xch_Ri e)
        | "jmp" -> match_single_expr tokens (fun e -> I.Jmp e)
        | "jmpf" -> match_single_expr tokens (fun e -> I.Jmpf e)
        | "br" -> match_single_expr tokens (fun e -> I.Br e)
        | "brf" -> match_single_expr tokens (fun e -> I.Jmpf e)
        | "bz" -> match_single_expr tokens (fun e -> I.Jmpf e)
        | "bnz" -> match_single_expr tokens (fun e -> I.Jmpf e)
        | "bp" -> match_triple_expr tokens (fun e1 e2 e3 -> I.Bp (e1, e2, e3))
        | "bpc" -> match_triple_expr tokens (fun e1 e2 e3 -> I.Bpc (e1, e2, e3))
        | "bn" -> match_triple_expr tokens (fun e1 e2 e3 -> I.Bn (e1, e2, e3))
        | "dbnz" -> match_dbnz tokens
                      (fun e1 e2 -> I.Dbnz_d9 (e1, e2))
                      (fun e1 e2 -> I.Dbnz_ri (e1, e2))
        (* TODO !!! Be and Bne *)
        | "call" -> match_single_expr tokens (fun e -> E.Call e)
        | "callf" -> match_single_expr tokens (fun e -> E.Callf e)
        | "callr"-> match_single_expr tokens (fun e -> E.Callr e)
        | "ret" -> stmt (get_span tok) (get_span tok) (S.Instruction I.Ret)
        | "reti" -> stmt (get_span tok) (get_span tok) (S.Instruction I.Reti)

      end
    | Token.EOF -> fail_parse (get_span tok) "Unexpected EOF"
    | _ -> fail_parse (get_span tok) "Expected Instruction"

  let statement tokens =
    ()
end

