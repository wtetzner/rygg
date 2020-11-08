
module type S = sig
  type input
  type loc
  type token = loc Token.t

  val lex : input -> token Stream.t
end

module Make(Input: Input.S) = struct
  open Token

  let is_name_start chr =
    (chr >= 'a' && chr <= 'z')
    || (chr >= 'A' && chr <= 'Z')
    || chr = '_'

  let is_name_body chr =
    (chr >= 'a' && chr <= 'z')
    || (chr >= 'A' && chr <= 'Z')
    || (chr >= '0' && chr <= '9')
    || chr = '_' || chr = '-'

  let handle_simple input length token_type = begin
      let start = Input.loc !input in
      input := Input.advance_by !input length;
      Some (Span.from start (Input.loc !input), token_type)
    end

  let in_bounds text idx = idx < (String.length text)

  let rec read_name text curr =
    if in_bounds text curr then
      match String.get text curr with
      | c when is_name_body c -> read_name text (curr + 1)
      | _ -> curr
    else
      curr

  let lex_name input =
    let text = (Input.full_text !input) in
    let curr = (Input.offset !input) in
    let start = Input.loc !input in
    let end_idx = read_name text curr in
    let old_input = !input in
    let len = (end_idx - (Loc.offset start)) in
    input := Input.advance_by !input len;
    let span = Span.from start (Input.loc !input) in
    let matches str = len = (String.length str) && Input.starts_with old_input str in
    let tok = match len with
      | _ when matches "type" -> Type
      | _ when matches "namespace" -> Namespace
      | _ when matches "module" -> Module
      | _ when matches "let" -> Let
      | _ when matches "true" -> True
      | _ when matches "false" -> False
      | _ when matches "implicit" -> Implicit
      | _ when matches "val" -> Val
      | _ when matches "fn" -> Fn
      | _ -> Name
    in
    Some (span, tok)

  let rec read_multiline_comment text curr nesting =
    if in_bounds text curr then
      match String.get text curr with
      | '*' -> if in_bounds text (curr + 1) then
                 match String.get text (curr + 1) with
                 | '/' -> if nesting = 1 then
                            (curr + 2, true)
                          else
                            read_multiline_comment text (curr + 2) (nesting - 1)
                 | _ -> read_multiline_comment text (curr + 2) nesting
               else
                 (curr + 1, false)
      | '/' -> if in_bounds text (curr + 1) then
                 match String.get text (curr + 1) with
                 | '*' -> read_multiline_comment text (curr + 2) (nesting + 1)
                 | _ -> read_multiline_comment text (curr + 1) nesting
               else
                 read_multiline_comment text (curr + 1) nesting
      | _ -> read_multiline_comment text (curr + 1) nesting
    else
      (curr, false)

  let lex_multiline_comment input =
    let text = Input.full_text !input in
    let start = Input.loc !input in
    let start_idx = Loc.offset start in
    let end_idx, successful = read_multiline_comment text start_idx 0 in
    let len = end_idx - start_idx in
    input := Input.advance_by !input len;
    let finish = Input.loc !input in
    let span = Span.from start finish in
    if successful then
      Some (span, Comment Multiline)
    else
      Some (span, Invalid "Invalid multiline comment")

  let rec read_line_comment text curr =
    if in_bounds text curr then
      match String.get text curr with
      | '\n' -> curr
      | _ -> read_line_comment text (curr + 1)
    else
      curr

  let lex_line_comment input =
    let text = Input.full_text !input in
    let start = Input.loc !input in
    let start_idx = Loc.offset start in
    let end_idx = read_line_comment text (start_idx + 2) in
    let len = end_idx - start_idx in
    input := Input.advance_by !input len;
    let finish = Input.loc !input in
    let span = Span.from start finish in
    Some (span, Comment Line)

  let rec read_whitespace text curr =
    if in_bounds text curr then
      match String.get text curr with
      | '\n' -> curr + 1
      | '\r' -> begin
          if in_bounds text (curr + 1) then
            match String.get text (curr + 1) with
            | '\n' -> curr + 2
            | _ -> read_whitespace text (curr + 1)
          else
            curr + 1
        end
      | ' ' | '\t'| '\b' -> read_whitespace text (curr + 1)
      | _ -> curr
    else
      curr

  let lex_whitespace input =
    let text = Input.full_text !input in
    let start = Input.loc !input in
    let start_idx = Loc.offset start in
    let end_idx = read_whitespace text (start_idx + 1) in
    let len = end_idx - start_idx in
    input := Input.advance_by !input len;
    let finish = Input.loc !input in
    let span = Span.from start finish in
    Some (span, Whitespace)

  let rec read_raw_prefix text curr =
    if in_bounds text curr then
      match String.get text curr with
      | '#' -> read_raw_prefix text (curr + 1)
      | '"' -> (curr + 1, true)
      | _ -> (curr, false)
    else
      (curr, false)

  let rec matches_suffix text curr len =
    if len = 0 then
      true
    else
      if in_bounds text curr then
        match String.get text curr with
        | '#' -> matches_suffix text (curr + 1) (len - 1)
        | _ -> false
      else
        false

  let rec read_raw_body text curr suffix =
    if in_bounds text curr then
      match String.get text curr with
      | '"' -> if matches_suffix text (curr + 1) suffix then
                 (curr, true)
               else
                 read_raw_body text (curr + 1) suffix
      | _ -> read_raw_body text (curr + 1) suffix
    else
      (curr, false)

  let lex_raw_string input =
    let text = Input.full_text !input in
    let start_idx = Input.offset !input in
    let (pos, successful_prefix) = read_raw_prefix text (start_idx + 1) in
    let start = Input.loc !input in
    if successful_prefix then
      begin
        let suffix = (pos - start_idx - 2) in
        let (end_pos, successful) = read_raw_body text pos suffix in
        if successful then
          begin
            let len = (end_pos + suffix + 1) - start_idx in
            input := Input.advance_by !input len;
            let span = Span.from start (Input.loc !input) in
            Some (span, Raw_string suffix)
          end
        else
          begin
            let len = end_pos - start_idx in
            input := Input.advance_by !input len;
            let span = Span.from start (Input.loc !input) in
            Some (span, Invalid "Invalid raw string")
          end
      end
    else
      begin
        input := Input.advance_by !input (pos - start_idx);
        let span = Span.from start (Input.loc !input) in
        Some (span, Invalid "Expected \"")
      end

  let rec read_string text curr =
    if in_bounds text curr then
      match String.get text curr with
      | '\\' ->
         begin
           if in_bounds text curr then
             read_string text (curr + 2)
           else
             (curr + 1, false)
         end
      | '"' -> (curr, true)
      | _ -> read_string text (curr + 1)
    else
      (curr, false)    

  let lex_string input =
    let text = Input.full_text !input in
    let start_idx = Input.offset !input in
    let finish, successful = read_string text (start_idx + 1) in
    let start = Input.loc !input in
    if successful then
      begin
        let len = (finish + 1) - start_idx in
        input := Input.advance_by !input len;
        let span = Span.from start (Input.loc !input) in
        Some (span, String)
      end
    else
      begin
        let len = finish - start_idx in
        input := Input.advance_by !input len;
        let span = Span.from start (Input.loc !input) in
        Some (span, Invalid "Expected \"")
      end

  let is_decimal_digit chr = chr >= '0' && chr <= '9'
  let is_hex_digit chr = (chr >= '0' && chr <= '9')
                         || (chr >= 'a' && chr <= 'f')
                         || (chr >= 'A' && chr <= 'F')
  let is_octal_digit chr = chr >= '0' && chr <= '7'
  let is_binary_digit chr = chr = '0' || chr = '1'

  let rec read_digits is_digit text curr =
    if in_bounds text curr then
      match String.get text curr with
      | c when is_digit c -> read_digits is_digit text (curr + 1)
      | _ -> curr
    else
      curr

  let read_decimal_digits text curr =
    if in_bounds text curr then
      let end_digits = read_digits is_decimal_digit text curr in
      if in_bounds text end_digits then
        match String.get text end_digits with
        | '.' -> if in_bounds text (end_digits + 1) then
                   match String.get text (end_digits + 1) with
                   | c when is_decimal_digit c ->
                      read_digits is_decimal_digit text (end_digits + 1)
                   | _ -> end_digits
                 else
                   end_digits
        | _ -> end_digits
      else
        end_digits
    else
      curr

  let lex_number input prefix_len read_digits =
    let text = Input.full_text !input in
    let start_idx = Input.offset !input in
    let start = Input.loc !input in
    let end_idx = read_digits text (start_idx + prefix_len) in
    let len = end_idx - start_idx in
    input := Input.advance_by !input len;
    let span = Span.from start (Input.loc !input) in
    if len = prefix_len then
      Some (span, Invalid "Invalid number")
    else
      Some (span, Number)

  let is_valid_token_start chr =
    match chr with
    | ' ' | '\t' | '\r' | '\n' | '\b' -> true
    | '{' | '}' | '(' | ')' | '[' | ']' | ':' | '=' | ';'
      | '.' | '|' | '+' | '-' | '*' | '/' | '<' | '>' | '`'
      | ',' | '"' | '?' | '!' | '=' -> true
    | c when is_name_start c -> true
    | _ -> false

  let rec read_invalid text curr =
    if in_bounds text curr then
      if is_valid_token_start (String.get text curr) then
        curr
      else
        read_invalid text (curr + 1)
    else
      curr

  let lex_invalid input =
    let text = Input.full_text !input in
    let start = Input.loc !input in
    let start_idx = Loc.offset start in
    let end_idx = read_invalid text (start_idx + 1) in
    let len = end_idx - start_idx in
    input := Input.advance_by !input len;
    let finish = Input.loc !input in
    let span = Span.from start finish in
    Some (span, Invalid "Unknown token")

  let lex_token input =
    let single token_type = handle_simple input 1 token_type in
    match Input.current_char !input with
    | ' ' | '\t' | '\r' | '\n' | '\b' -> lex_whitespace input
    | '{' -> single Left_brace
    | '}' -> single Right_brace
    | '(' -> single Left_paren
    | ')' -> single Right_paren
    | '[' -> single Left_bracket
    | ']' -> single Right_bracket
    | '=' -> if Input.starts_with !input "==" then
               handle_simple input 2 Double_equal
             else
               single Equal
    | '<' -> if Input.starts_with !input "<-" then
               handle_simple input 2 Back_arrow
             else if Input.starts_with !input "<=" then
               handle_simple input 2 Less_equal
             else
               single Less_than
    | '>' -> if Input.starts_with !input ">=" then
               handle_simple input 2 Greater_equal
             else
               single Greater_than
    | ':' -> if Input.starts_with !input "::" then
               handle_simple input 2 Double_colon
             else
               single Colon
    | '=' -> single Equal
    | ';' -> single Semicolon
    | '.' -> single Dot
    | '|' -> single Pipe
    | '+' -> single Plus
    | '`' -> single Backtick
    | '-' -> if Input.starts_with !input "->" then
               handle_simple input 2 Arrow
             else
               single Dash
    | '*' -> single Asterisk
    | '/' -> if Input.starts_with !input "//" then
               lex_line_comment input
             else if Input.starts_with !input "/*" then
               lex_multiline_comment input
             else
               single Slash
    | ',' -> single Comma
    | '?' -> single Question
    | '!' -> single Exclamation
    | '"' -> lex_string input
    | 'r' -> if Input.starts_with !input "r\"" || Input.starts_with !input "r#" then
               lex_raw_string input
             else
               lex_name input
    | '0' -> if Input.starts_with !input "0x" then
               lex_number input 2 (read_digits is_hex_digit)
             else if Input.starts_with !input "0b" then
               lex_number input 2 (read_digits is_binary_digit)
             else if Input.starts_with !input "0o" then
               lex_number input 2 (read_digits is_octal_digit)
             else
               lex_number input 0 read_decimal_digits
    | c when is_decimal_digit c -> lex_number input 0 read_decimal_digits
    | c when is_name_start c -> lex_name input
    | _ -> lex_invalid input

  let lex input =
    let iref = ref input in
    Stream.from (fun idex -> if Input.is_empty !iref then
                               None
                             else
                               lex_token iref)

end
