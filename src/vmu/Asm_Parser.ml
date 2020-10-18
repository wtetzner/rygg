
module Out_channel = Core.Out_channel
module In_channel = Core.In_channel

open Asm
module S = Statement
module D = Directive
module E = Expression
module I = Instruction

module Span = Compiler.Span
module Loc = Compiler.Loc

module Files = Compiler.Files

exception Lexer_failure of Loc.t * string
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

let is_some opt =
  match opt with
  | Some _ -> true
  | None -> false

let starts_with str prefix =
  if (String.length str) >= (String.length prefix) then
    let capture = String.sub str 0 (String.length prefix) in
    String.equal capture prefix
  else
    false

let maybe_get str pos =
  if pos >= 0 && pos < (String.length str) then
    Some (String.get str pos)
  else
    None

module Token = struct
  type t = Span.t * token_type [@@deriving ord, eq]
  and token_type =
    | LeftParen
    | RightParen
    | Hash
    | Comma
    | Colon
    | Name of string
    | Number of int
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
    | EOF [@@deriving ord, eq]

  let clean_str str =
    let inner = String.sub str 1 ((String.length str) - 2) in
    let len = String.length inner in
    let result = Buffer.create 16 in
    let pos = ref 0 in
    while !pos < len do
      let chr = String.get inner !pos in
      let next = maybe_get inner (!pos + 1) in
      if chr = '\\' then
        match next with
        | Some next ->
           let append = (match next with
                         | 'r' -> '\r'
                         | 'n' -> '\n'
                         | 't' -> '\t'
                         | 'b' -> '\b'
                         | c -> c) in
           (Buffer.add_char result append;
            pos := !pos + 2)
        | None ->
           (Buffer.add_char result chr;
            pos := !pos + 1)
      else
        (Buffer.add_char result chr;
         pos := !pos + 1)
    done;
    Buffer.contents result

  let str str = String (clean_str str)
  let name str = Name (String.lowercase_ascii str)
  let directive str = Name (String.lowercase_ascii str)
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
  val tokens : string -> string -> Token.t Stream.t
end = struct
  type t = {
      str: string;
      lexbuf: Sedlexing.lexbuf;
      mutable loc: Loc.t
    }

  let from_string name str = {
      str = str;
      lexbuf = Sedlexing.Utf8.from_string str;
      loc = Loc.with_filename Loc.unknown name
    }

  let read_lexeme lexer =
    let buf = lexer.lexbuf in
    let lexeme = Sedlexing.Utf8.lexeme buf in
    let len = String.length lexeme in
    let loc = lexer.loc in
    let new_loc =  Loc.advance_to loc lexer.str ((Loc.offset loc) + len) in
    lexer.loc <- new_loc;
    (Span.from loc new_loc, lexeme)

  let indirection_mode = [%sedlex.regexp? "@", (Chars "rR"), (Chars "0123")]
  let ident_start = [%sedlex.regexp? 'a'..'z'|'A'..'Z'|'_' ]
  let ident_char = [%sedlex.regexp? 'a'..'z'|'A'..'Z'|'_'|'0'..'9' ]
  let identifier = [%sedlex.regexp? ident_start, Star ident_char ]
  let hex_num = [%sedlex.regexp? "$", Plus ascii_hex_digit ]
  let bin_num = [%sedlex.regexp? "%", Plus ('0' | '1') ]
  let dec_num = [%sedlex.regexp? Plus ('0'..'9') ]
  let string_re = [%sedlex.regexp? "\"", Star (("\\", any) | Compl ('"' | '\\')), "\"" ]
  let comment = [%sedlex.regexp? ";", Star (('\r' | Compl '\n') | (Compl (eof | '\n' | "\r"))) ]

  let token lexer =
    let buf = lexer.lexbuf in
    (match%sedlex buf with
     | Star (white_space | comment) ->
        let lexeme = Sedlexing.Utf8.lexeme buf in
        let len = String.length lexeme in
        let loc = lexer.loc in
        let new_loc = Loc.advance_to loc lexer.str ((Loc.offset loc) + len) in
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

  let replace_asterisks str =
    let pos = ref 0 in
    let len = String.length str in
    let buffer = Buffer.create len in
    while !pos < len do
      let chr = String.get str !pos in
      let next = maybe_get str (!pos + 1) in
      match chr, next with
      | '\n', Some '*' ->
         (Buffer.add_string buffer "\n;";
          pos := !pos + 2)
      | _, _ ->
         (Buffer.add_char buffer chr;
          pos := !pos + 1)
    done;
    Buffer.contents buffer

  let tokens name str =
    let str = replace_asterisks str in
    let lexer = from_string name str in
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
    drop1 tokens;
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
    let instr = S.Instruction (constr expr1 expr2) in
    stmt (get_span name) (E.span expr2) instr

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

  let match_be_bne tokens constr_i8 constr_d9 constr_im =
    let name = peek tokens in
    drop1 tokens;
    let tok = peek tokens in
    match get_token tok with
    | Token.Hash ->
       (let expr1 = i8 tokens in
        do_match Token.Comma tokens;
        let expr2 = expression tokens in
        let instr = S.Instruction (constr_i8 expr1 expr2) in
        stmt (get_span name) (E.span expr2) instr)
    | Token.R0 | Token.R1 | Token.R2 | Token.R3 ->
       (let indir = indirection_mode tokens in
        do_match Token.Comma tokens;
        let expr1 = i8 tokens in
        do_match Token.Comma tokens;
        let expr2 = expression tokens in
        let instr = S.Instruction (constr_im indir expr1 expr2) in
        stmt (get_span name) (E.span expr2) instr)
    | Token.EOF -> fail_parse (get_span tok) "Unexpected EOF"
    | _ ->
       (let expr1 = expression tokens in
        do_match Token.Comma tokens;
        let expr2 = expression tokens in
        let instr = S.Instruction (constr_d9 expr1 expr2) in
        stmt (get_span name) (E.span expr2) instr)

  let match_single tokens value =
    let tok = peek tokens in
    let span = (get_span tok) in
    drop1 tokens;
    stmt span span value

  let instruction tokens =
      let tok = peek tokens in
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
                       (fun e -> I.Dec_d9 e)
                       (fun e -> I.Dec_Ri e)
          | "mul" -> match_single tokens (S.Instruction I.Mul)
          | "div" -> match_single tokens (S.Instruction I.Div)
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
          | "rol" -> match_single tokens (S.Instruction I.Rol)
          | "rolc" -> match_single tokens (S.Instruction I.Rolc)
          | "ror" -> match_single tokens (S.Instruction I.Ror)
          | "rorc" -> match_single tokens (S.Instruction I.Rorc)
          | "ld" -> match_single_2 tokens
                      (fun e -> I.Ld_d9 e)
                      (fun e -> I.Ld_Ri e)
          | "st" -> match_single_2 tokens
                      (fun e -> I.St_d9 e)
                      (fun e -> I.St_Ri e)
          | "mov" -> match_move tokens
                       (fun e1 e2 -> I.Mov_d9 (e1, e2))
                       (fun e1 e2 -> I.Mov_Rj (e1, e2))
          | "ldc" -> match_single tokens (S.Instruction I.Ldc)
          | "push" -> match_single_expr tokens (fun e -> I.Push e)
          | "pop" -> match_single_expr tokens (fun e -> I.Pop e)
          | "xch" -> match_single_2 tokens
                       (fun e -> I.Xch_d9 e)
                       (fun e -> I.Xch_Ri e)
          | "jmp" -> match_single_expr tokens (fun e -> I.Jmp e)
          | "jmpf" -> match_single_expr tokens (fun e -> I.Jmpf e)
          | "br" -> match_single_expr tokens (fun e -> I.Br e)
          | "brf" -> match_single_expr tokens (fun e -> I.Brf e)
          | "bz" -> match_single_expr tokens (fun e -> I.Bz e)
          | "bnz" -> match_single_expr tokens (fun e -> I.Bnz e)
          | "bp" -> match_triple_expr tokens (fun e1 e2 e3 -> I.Bp (e1, e2, e3))
          | "bpc" -> match_triple_expr tokens (fun e1 e2 e3 -> I.Bpc (e1, e2, e3))
          | "bn" -> match_triple_expr tokens (fun e1 e2 e3 -> I.Bn (e1, e2, e3))
          | "dbnz" -> match_dbnz tokens
                        (fun e1 e2 -> I.Dbnz_d9 (e1, e2))
                        (fun e1 e2 -> I.Dbnz_Ri (e1, e2))
          | "be" -> match_be_bne tokens
                      (fun e1 e2 -> I.Be_i8 (e1, e2))
                      (fun e1 e2 -> I.Be_d9 (e1, e2))
                      (fun e1 e2 e3 -> I.Be_Rj (e1, e2, e3))
          | "bne" -> match_be_bne tokens
                       (fun e1 e2 -> I.Bne_i8 (e1, e2))
                       (fun e1 e2 -> I.Bne_d9 (e1, e2))
                       (fun e1 e2 e3 -> I.Bne_Rj (e1, e2, e3))
          | "call" -> match_single_expr tokens (fun e -> I.Call e)
          | "callf" -> match_single_expr tokens (fun e -> I.Callf e)
          | "callr"-> match_single_expr tokens (fun e -> I.Callr e)
          | "ret" -> match_single tokens (S.Instruction I.Ret)
          | "reti" -> match_single tokens (S.Instruction I.Reti)
          | "clr1" -> match_double_expr tokens (fun e1 e2 -> I.Clr1 (e1, e2))
          | "set1" -> match_double_expr tokens (fun e1 e2 -> I.Set1 (e1, e2))
          | "not1" -> match_double_expr tokens (fun e1 e2 -> I.Not1 (e1, e2))
          | "nop" -> match_single tokens (S.Instruction I.Nop)
          | _ -> fail_parse (get_span tok) "Unknown Instruction"
        end
      | Token.EOF -> fail_parse (get_span tok) "Unexpected EOF"
      | _ -> fail_parse (get_span tok) "Expected Instruction"

  let next_comma tokens =
    let tok = peek tokens in
    match get_token tok with
    | Token.Comma -> (drop1 tokens; true)
    | _ -> false

  let expr_list tokens =
    let expr1 = expression tokens in
    let list = ref [expr1] in
    let comma = ref (next_comma tokens) in
    while !comma do
      let expr = expression tokens in
      list := expr :: !list;
      comma := next_comma tokens
    done;
    List.rev !list

  let directive tokens =
    let name_tok = peek tokens in
    let Token.Name name = get_token name_tok in
    drop1 tokens;
    match name with
    | ".byte" ->
       (let tok = peek tokens in
        match get_token tok with
        | Token.String string ->
           (drop1 tokens;
            let bitstring = Bitstring.bitstring_of_string string in
            let dir = S.Directive (D.ByteString bitstring) in
            stmt (get_span name_tok) (get_span tok) dir)
        | Token.EOF -> fail_parse (get_span tok) "Unexpected EOF"
        | _ ->
           (let exprs = expr_list tokens in
            let dir = S.Directive (D.Byte exprs) in
            stmt (get_span name_tok) (get_span tok) dir))
    | ".org" ->
       (let tok = peek tokens in
        match get_token tok with
        | Number num ->
           drop1 tokens;
           let org = S.Directive (D.Org num) in
           stmt (get_span name_tok) (get_span tok) org
        | _ -> fail_parse (get_span tok) "Expected Integer")
    | ".word" ->
       (let exprs = expr_list tokens in
        let word = S.Directive (D.Word exprs) in
        stmt (get_span name_tok) (get_span name_tok) word)
    | ".cnop" ->
       (let expr1 = expression tokens in
        do_match Token.Comma tokens;
        let expr2 = expression tokens in
        let cnop = S.Directive (D.Cnop (expr1, expr2)) in
        stmt (get_span name_tok) (E.span expr2) cnop)
    | _ -> fail_parse (get_span name_tok) "Unknown Directive"

  let label tokens =
    let tok = peek tokens in
    drop1 tokens;
    match get_token tok with
    | Token.Name name ->
       (let colon = peek tokens in
        do_match Token.Colon tokens;
        let label = S.Label name in
        stmt (get_span tok) (get_span colon) label)
    | _ -> fail_parse (get_span tok) "Expected Label"

  let variable tokens =
    let tok = peek tokens in
    drop1 tokens;
    match get_token tok with
    | Token.Name name ->
       (do_match Token.Equals tokens;
        let expr = expression tokens in
        let var = S.Variable (name, expr) in
        stmt (get_span tok) (E.span expr) var)
    | _ -> fail_parse (get_span tok) "Expected Variable"

  let alias tokens =
    let tok = peek tokens in
    drop1 tokens;
    match get_token tok with
    | Token.Name name ->
       (do_match (Token.Name "equ") tokens;
        let expr = expression tokens in
        let var = S.Variable (name, expr) in
        stmt (get_span tok) (E.span expr) var)
    | _ -> fail_parse (get_span tok) "Expected Alias"

  let stream_concat streams =
    let current_stream = ref None in
    let rec next i =
      try
        let stream =
          match !current_stream with
          | Some stream -> stream
          | None ->
             let stream = Stream.next streams in
             current_stream := Some stream;
             stream in
        try Some (Stream.next stream)
        with Stream.Failure -> (current_stream := None; next i)
      with Stream.Failure -> None in
    Stream.from next

  let single_stream item =
    Stream.from (fun idx -> if idx = 0 then Some item else None)

  let rec statement files inc_dir tokens =
    let tok1 :: tok2 :: rest = npeek 2 tokens in
    match (get_token tok1), (get_token tok2) with
    | Token.Name ".include", Token.String file ->
       (dropn 2 tokens;
        let path = inc_dir ^ "/" ^ file in
        let text = Files.get files path in
        Some (parse files inc_dir path text))
    | Token.Name _, Token.Colon -> Some (single_stream (label tokens))
    | Token.Name name, _ when starts_with name "." ->
       Some (single_stream (directive tokens))
    | Token.Name _, Token.Equals -> Some (single_stream (variable tokens))
    | Token.Name _, Token.Name "equ" -> Some (single_stream (alias tokens))
    | Token.Name _, _ -> Some (single_stream (instruction tokens))
    | Token.EOF, _ -> None
    | _ -> fail_parse (get_span tok1) "Unexpected Token"
  and parse files inc_dir filename text =
    let tokens = Lexer.tokens filename text in
    statements files inc_dir tokens
  and statements files inc_dir tokens =
    stream_concat (Stream.from (fun _ -> statement files inc_dir tokens))
end

let print_error pos msg files =
  Compiler.Message.(print_msgln Error pos msg files);
  exit 2

let write_bytes_to_file file bytes =
  Out_channel.with_file file ~f:(fun f -> Out_channel.output_string f (Bytes.to_string bytes));
  ()

let list_of_stream stream =
  let result = ref [] in
  Stream.iter (fun value -> result := value :: !result) stream;
  List.rev !result

let do_assemble filename inc_dir output =
  let inc_dir = match inc_dir with
    | Some dir -> dir
    | None -> Core.Filename.dirname filename in
  let files = Files.empty in
  let text = Files.get files filename in
  try
    let statements = list_of_stream (Parser.parse files inc_dir filename text) in
    (* List.iter (fun s -> print_endline (S.to_string s)) statements; *)
    let bytes = Asm.assemble statements in
    write_bytes_to_file output bytes;
    print_string "[";
    ANSITerminal.(printf [green] "%s" "OK");
    Printf.printf "] Generated %s\n" output;
  with
  | Asm.Asm_failure (pos,msg) -> print_error pos msg files
  | Lexer_failure (loc, msg) ->
     let pos = Compiler.Position.Loc loc in
     print_error pos msg files
  | Parse_failure (span, msg) ->
     let pos = Compiler.Position.Span span in
     print_error pos msg files

let assemble filename inc_dir output =
  try
    do_assemble filename inc_dir output
  with
  | Sys_error msg -> (Compiler.Message.print_tag_msg Compiler.Message.Error msg; exit 1)
