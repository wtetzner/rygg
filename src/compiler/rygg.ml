
module type LOC = sig
  type t
  type source

  val merge : t -> t -> t
  val sources : t -> source list
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val debug_string : t -> string
end

module type NAME = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val debug_string : t -> string
end

module Name: sig
  type t

  val input : string -> t
  val internal : string -> t

  val is_input : t -> bool
  val is_internal : t -> bool

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val debug_string : t -> string
end = struct
  type t = { tag: int; name: string }
  type tag = Input | Internal

  let input_tag = 0
  let current_internal_tag = ref (input_tag + 1)

  let input name = { tag = input_tag; name }
  let internal name =
    let tag = !current_internal_tag in
    current_internal_tag := !current_internal_tag + 1;
    { tag; name }

  let is_input name = name.tag = input_tag
  let is_internal name = name.tag != input_tag

  let equal n1 n2 =
    n1.tag = n2.tag
    && String.equal n1.name n2.name

  let compare n1 n2 =
    let tag_cmp = Int.compare n1.tag n2.tag in
    if tag_cmp = 0 then
      String.compare n1.name n2.name
    else
      tag_cmp

  let to_string name =
    if name.tag = input_tag then
      name.name
    else
      Printf.sprintf "%s%%%d" name.name name.tag

  let debug_string name = Printf.sprintf "{ tag = %d; name = %s }" name.tag name.name
end

module Token: sig
  type comment_type = Line | Multiline
  type raw_string_prefix_count = int

  type token_type =
    | Whitespace
    | Left_brace
    | Right_brace
    | Left_paren
    | Right_paren
    | Left_bracket
    | Right_bracket
    | Less_than
    | Less_equal
    | Greater_than
    | Greater_equal
    | Arrow
    | Back_arrow
    | Colon
    | Double_colon
    | Equal
    | Double_equal
    | Semicolon
    | Dot
    | Pipe
    | Plus
    | Dash
    | Asterisk
    | Slash
    | Comma
    | Question
    | Exclamation
    | Type
    | Namespace
    | Module
    | Let
    | True
    | False
    | Implicit
    | Val
    | Fn
    | Backtick
    | String
    | Raw_string of raw_string_prefix_count
    | Number
    | Comment of comment_type
    | Name
    | Invalid of string

  type t = Span.t * token_type

  val is_whitespace : t -> bool
  val is_invalid : t -> bool

  val is_raw_string : t -> bool
  val is_any_string : t -> bool

  val is_comment : t -> bool

  val substr : string -> t -> string
  val to_string : t -> string

  val compare_type : token_type -> token_type -> int
end = struct
  type comment_type = Line | Multiline
  type raw_string_prefix_count = int

  type token_type =
    | Whitespace
    | Left_brace
    | Right_brace
    | Left_paren
    | Right_paren
    | Left_bracket
    | Right_bracket
    | Less_than
    | Less_equal
    | Greater_than
    | Greater_equal
    | Arrow
    | Back_arrow
    | Colon
    | Double_colon
    | Equal
    | Double_equal
    | Semicolon
    | Dot
    | Pipe
    | Plus
    | Dash
    | Asterisk
    | Slash
    | Comma
    | Question
    | Exclamation
    | Type
    | Namespace
    | Module
    | Let
    | True
    | False
    | Implicit
    | Val
    | Fn
    | Backtick
    | String
    | Raw_string of raw_string_prefix_count
    | Number
    | Comment of comment_type
    | Name
    | Invalid of string

  type t = Span.t * token_type

  let to_string token =
    let open Printf in
    let span, tok = token in
    let tok_str = match tok with
      | Whitespace -> "Whitespace"
      | Left_brace -> "{"
      | Right_brace -> "}"
      | Left_paren -> "("
      | Right_paren -> ")"
      | Left_bracket -> "["
      | Right_bracket -> "]"
      | Less_than -> "<"
      | Less_equal -> "<="
      | Greater_than -> ">"
      | Greater_equal -> ">="
      | Arrow -> "->"
      | Back_arrow -> "<-"
      | Colon -> ":"
      | Double_colon -> "::"
      | Equal -> "="
      | Double_equal -> "=="
      | Semicolon -> ";"
      | Dot -> "."
      | Pipe -> "|"
      | Plus -> "+"
      | Dash -> "-"
      | Asterisk -> "*"
      | Slash -> "/"
      | Comma -> ","
      | Question -> "?"
      | Exclamation -> "!"
      | Type -> "type"
      | Namespace -> "namespace"
      | Module -> "module"
      | Let -> "let"
      | True -> "true"
      | False -> "false"
      | Implicit -> "implicit"
      | Val -> "val"
      | Fn -> "fn"
      | Backtick -> "`"
      | String -> "String"
      | Raw_string prefix_count ->
         let prefix = String.make prefix_count '#' in
         sprintf "r%s\"...\"%s" prefix prefix
      | Number -> "Number"
      | Comment Line -> "Line Comment"
      | Comment Multiline -> "Multiline Comment"
      | Name -> "Name"
      | Invalid message -> sprintf "Invalid \"%s\"" message
    in
    sprintf "%s: %s" (Span.to_string span) tok_str

  let compare_value =
    function
    | Whitespace -> 0
    | Left_brace -> 1
    | Right_brace -> 2
    | Left_paren -> 3
    | Right_paren -> 4
    | Left_bracket -> 5
    | Right_bracket -> 6
    | Less_than -> 7
    | Greater_than -> 8
    | Arrow -> 9
    | Back_arrow -> 10
    | Colon -> 11
    | Double_colon -> 12
    | Equal -> 13
    | Double_equal -> 14
    | Semicolon -> 15
    | Dot -> 16
    | Pipe -> 17
    | Plus -> 18
    | Dash -> 19
    | Asterisk -> 20
    | Slash -> 21
    | Comma -> 22
    | Question -> 23
    | Exclamation -> 24
    | Type -> 25
    | Namespace -> 26
    | Module -> 27
    | Let -> 28
    | True -> 29
    | False -> 30
    | Implicit -> 31
    | Backtick -> 32
    | String -> 33
    | Number -> 34
    | Name -> 35
    | Raw_string _ -> 36
    | Comment _ -> 37
    | Invalid _ -> 38
    | Less_equal -> 39
    | Greater_equal -> 40
    | Val -> 41
    | Fn -> 42

  let compare_type l r = Int.compare (compare_value l) (compare_value r)

  let is_whitespace = function (_, Whitespace) -> true | _ -> false

  let is_invalid = function (_, Invalid _) -> true | _ -> false

  let is_raw_string = function (_, Raw_string _) -> true | _ -> false
  let is_any_string = function (_, String)
                             | (_, Raw_string _) -> true
                             | _ -> false

  let is_comment = function (_, Comment _) -> true | _ -> false

  let substr text token =
    let (span, tok) = token in
    let start = Loc.offset (Span.start span) in
    let finish = Loc.offset (Span.finish span) in
    String.sub text start (finish - start)

end

module Source = struct
  module Metadata: sig
    type t = {
        span: Span.t;
        full_span: Span.t;
        tokens: Token.t list;
        leading_tokens: Token.t list;
        trailing_tokens: Token.t list
    }

    val from_span : Span.t -> t
    val span : t -> Span.t
    val full_span : t -> Span.t
    val merge : t -> t -> t
    val tokens : t -> Token.t list
    val leading_tokens : t -> Token.t list
    val trailing_tokens : t -> Token.t list
  end = struct
    type t = {
        span: Span.t;
        full_span: Span.t;
        tokens: Token.t list;
        leading_tokens: Token.t list;
        trailing_tokens: Token.t list
    }

    let from_span span =
      {
        span = span;
        full_span = span;
        tokens = [];
        leading_tokens = [];
        trailing_tokens = []
      }
    let span md = md.span
    let full_span md = md.full_span
    let merge md1 md2 =
      {
        span = Span.merge (span md1) (span md2);
        full_span = Span.merge (full_span md1) (full_span md2);
        tokens =
          (let left = List.rev_append (List.rev md1.tokens) md1.trailing_tokens in
           let right = List.rev_append (List.rev md2.leading_tokens) md2.tokens in
           List.rev_append (List.rev left) right);
        leading_tokens = md1.leading_tokens;
        trailing_tokens = md2.trailing_tokens
      }
    let tokens md = md.tokens
    let leading_tokens md = md.leading_tokens
    let trailing_tokens md = md.trailing_tokens
  end
  type metadata = Metadata.t

  type deftype = unit
  type kind = unit
  type type_decl = {
      decl_kind: kind;
      decl_concrete: deftype option
  }

  type type_def = {
      def_kind: kind;
      def_concrete: deftype option
  }

  module ValueType = struct
    type node =
      | Name of string
      | Invalid
    type t = Metadata.t * node

    let to_string value =
      let (_, node) = value in
      match node with
      | Name name -> name
      | Invalid -> "<INVALID>"
  end
  type value_type = ValueType.t

  type module_decl = unit

  module Expr = struct
    module Binop = struct
      type t =
        | Plus
        | Minus
        | Times
        | Divide
        | Dot
        | Equal
        | Less_than
        | Less_equal
        | Greater_than
        | Greater_equal

      let to_string = function
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
        | Divide -> "/"
        | Dot -> "."
        | Equal -> "=="
        | Less_than -> "<"
        | Less_equal -> "<="
        | Greater_than -> ">"
        | Greater_equal -> ">="
    end

    module PrefixOp = struct
      type t =
        | Plus
        | Minus

      let to_string = function
        | Plus -> "+"
        | Minus -> "-"
    end

    module PostfixOp = struct
      type t =
        | Question
        | Colon
        | Left_paren

      let to_string = function
        | Question -> "?"
        | Colon -> ":"
        | Left_paren -> "("
    end

    type ident = metadata * Name.t

    type name =
      | Simple_name of ident
      | Qualified_name of ident * ident
      | Invalid_qualified_name of ident

    type node =
      | Name of name
      | Type_ann of t * value_type
      | Binop of Binop.t * t * t
      | Prefix_op of PrefixOp.t * t
      | Postfix_op of PostfixOp.t * t
      | Application of t * arg list
      | Number of string
      | String of string
      | Boolean of bool
      | Invalid
    and arg =
      | Named_arg of metadata * Name.t * t
      | Unnamed_arg of t
    and t = metadata * node

    let is_complex expr =
      let (_, node) = expr in
      match node with
      | Name _ -> false
      | Type_ann _ -> true
      | Binop (_, _, _) -> true
      | Prefix_op (_, _) -> true
      | Postfix_op (_, _) -> true
      | Application (_, _) -> false
      | Number _ | String _ | Boolean _ | Invalid -> false

    let rec to_string expr =
      let open Printf in
      let (_, node) = expr in
      match node with
      | Name (Simple_name (_, name)) -> Name.to_string name
      | Name (Qualified_name ((_, ns), (_, name))) -> sprintf "%s::%s" (Name.to_string ns) (Name.to_string name)
      | Name (Invalid_qualified_name (_, ns)) -> sprintf "%s::<INVALID>" (Name.to_string ns)
      | Type_ann (e, v) -> sprintf "%s:%s" (wrap e) (ValueType.to_string v)
      | Binop (Binop.Dot, l, r) ->
         sprintf "%s.%s" (wrap l) (wrap r)
      | Binop (op, l, r) ->
         sprintf "%s %s %s" (wrap l) (Binop.to_string op) (wrap r)
      | Prefix_op (op, e) -> sprintf "%s%s" (PrefixOp.to_string op) (wrap e)
      | Postfix_op (op, e) -> sprintf "%s%s" (wrap e) (PostfixOp.to_string op)
      | Application (c, a) ->
         let args = String.concat ", " (List.map arg_to_string a) in
         sprintf "%s(%s)" (wrap c) args
      | Number num -> num
      | String str -> sprintf "\"%s\"" (String.escaped str)
      | Boolean bool -> if bool then "true" else "false"
      | Invalid -> "<INVALID>"

    and arg_to_string arg =
      let open Printf in
      match arg with
      | Named_arg (_, name, expr) ->
         sprintf "%s = %s" (Name.to_string name) (to_string expr)
      | Unnamed_arg expr -> to_string expr

    and wrap expr =
      let open Printf in
      if is_complex expr then
        sprintf "(%s)" (to_string expr)
      else
        to_string expr

    let metadata = function (metadata, _) -> metadata
    let span expr = Metadata.span (metadata expr)
    let full_span expr = Metadata.full_span (metadata expr)
    let leading_tokens expr = Metadata.leading_tokens (metadata expr)
    let trailing_tokens expr = Metadata.trailing_tokens (metadata expr)
  end

  type term = unit

  type ident = metadata * Name.t

  type implicitness = Implicit | Explicit

  type import_entry = [
    | `Name of ident
    | `Rename of ident * (implicitness option * ident)
  ]

  type import_line = [
    | `Type_import of metadata * import_entry list
    | `Value_import of metadata * import_entry list
    | `Module_type_import of metadata * import_entry list
    | `Module_import of metadata * import_entry list
    | `All_import of metadata * import_entry list
  ]

  type open_mask = import_line list

  type t = {
      
      namespace_entries: namespace_entry list
  }
  and signature_entry = [
    | `Type_decl of ident * type_decl
    | `Value_type of ident * value_type
    | `Module_type of ident * module_type
    | `Module_decl of ident * module_decl
    | `Module_open of module_term * open_mask option
  ]

  and signature = signature_entry list

  and module_type = [
    | `Signature of Span.t * signature
    | `Functor_type of Span.t * ident * module_type * module_type
    | `Signature_union of module_type * module_type
  ]

  and module_entry = [
    | `Type_def of ident * type_def
    | `Term of ident * term
    | `Module_type of ident * module_type
    | `Module_term of ident * module_term
    | `Module_open of module_term * open_mask option
  ]

  and module_term = [
    | `Path of module_path
    | `Structure of Span.t * structure
    | `Functor of ident * module_type * module_term
    | `Apply of module_term * module_term
    | `Constraint of module_term * module_term
  ]

  and structure = module_entry list
  and namespace_entry = [
    | `Module_type of ident * module_type
    | `Module_term of ident * module_term
    | `Module_open of module_term * open_mask option
    | `Open of module_path * open_mask option
  ]
  and module_path = [
    | `Ident of namespace option * ident
    | `Dot of module_path * ident
  ]
  and export_entry = [
    | `Name of ident
    | `Rename of ident * ident
  ]
  and export_line = [
    | `Module_export of metadata * export_entry list
    | `Module_type_export of metadata * export_entry list
  ]
  and export = [
    | `All
    | `Only of export_line list
    ]
  and hide = [
    | `All
    | `Only of ident list
  ]
  and namespace = {
      namespace: string;
      export: export option;
      hide: hide option;
      namespace_entries: namespace_entry list;
  }

end

type source = Source.t

module Lexer: sig
  val lex : Input.t -> Token.t Stream.t
end = struct
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

module ParseError = struct
  type t = [
    (* | `Unexpected_token of Token.t *)
    | `Invalid_token of Span.t
    | `Backslash_at_end_of_string of Span.t
    | `Expected_comma_or_right_paren_args_list of Span.t
    | `Invalid_escape_sequence of Span.t
    | `Ascii_value_out_of_range of Span.t
    | `Unicode_value_out_of_range of Span.t
    | `Lone_surrogate_unicode_escape of Span.t
    | `Missing_closing_paren of Span.t * Span.t
    | `Expected_expression of Span.t
    | `Expected_type_expression of Span.t
    | `Expected_identifier of Span.t
  ]

  let to_string error =
    let open Printf in
    match error with
    | `Invalid_token span -> sprintf "Invalid_token %s" (Span.to_string span)
    | `Backslash_at_end_of_string span -> sprintf "Backslash_at_end_of_string %s" (Span.to_string span)
    | `Expected_comma_or_right_paren_args_list span -> sprintf "Expected_comma_or_right_paren_args_list %s" (Span.to_string span)
    | `Invalid_escape_sequence span -> sprintf "Invalid_escape_sequence %s" (Span.to_string span)
    | `Ascii_value_out_of_range span -> sprintf "Ascii_value_out_of_range %s" (Span.to_string span)
    | `Unicode_value_out_of_range span -> sprintf "Unicode_value_out_of_range %s" (Span.to_string span)
    | `Lone_surrogate_unicode_escape span -> sprintf "Lone_surrogate_unicode_escape %s" (Span.to_string span)
    | `Missing_closing_paren (paren_span, span) -> sprintf "Missing_closing_paren %s" (Span.to_string span)
    | `Expected_expression span -> sprintf "Expected_expression %s" (Span.to_string span)
    | `Expected_type_expression span -> sprintf "Expected_type_expression %s" (Span.to_string span)
    | `Expected_identifier span -> sprintf "Expected_identifier %s" (Span.to_string span)

  let string_of_errors errors =
    let result = ref "[" in
    let first = ref true in
    List.iter (fun e -> begin
                   (if !first then
                     first := false
                    else
                      result := !result ^ ", ");
                   result := !result ^ (to_string e)
                 end) errors;
    !result ^ "]"
end

module type PARSER_STATE = sig
  type t
  type tnode = {
      token: Token.t;
      leading_tokens: Token.t list;
      trailing_tokens: Token.t list
  }

  val create : string -> string -> t
  val text : t -> string
  val get : t -> int -> tnode
  val next : t -> tnode * t
  val npeek : int -> t -> tnode list
  val peek : t -> tnode option
  val peek_type : t -> Token.token_type option
  val is_empty : t -> bool
  val token_type : tnode -> Token.token_type
  val starts_with : t -> (Token.t -> bool) list -> bool
  val span : tnode -> Span.t
  val full_span : tnode -> Span.t
  val substr : t -> tnode -> string
  val full_substr : t -> tnode -> string
  val to_string : t -> string
end

module SourceParser: sig
  type parse_error = ParseError.t

  module ParserState: PARSER_STATE

  val parse_expr : ParserState.t -> ((parse_error list) * Source.Expr.t * ParserState.t) option
end = struct
  type parse_error = ParseError.t

  module TokenMatchers = struct
    open Token
    let whitespace = function (_, Whitespace) -> true | _ -> false
    let left_brace = function (_, Left_brace) -> true | _ -> false
    let right_brace = function (_, Right_brace) -> true | _ -> false
    let left_paren = function (_, Left_paren) -> true | _ -> false
    let right_paren = function (_, Right_paren) -> true | _ -> false
    let left_bracket = function (_, Left_bracket) -> true | _ -> false
    let right_bracket = function (_, Right_bracket) -> true | _ -> false
    let less_than = function (_, Less_than) -> true | _ -> false
    let greater_than = function (_, Greater_than) -> true | _ -> false
    let arrow = function (_, Arrow) -> true | _ -> false
    let back_arrow = function (_, Back_arrow) -> true | _ -> false
    let colon = function (_, Colon) -> true | _ -> false
    let double_colon = function (_, Double_colon) -> true | _ -> false
    let equal = function (_, Equal) -> true | _ -> false
    let semicolon = function (_, Semicolon) -> true | _ -> false
    let dot = function (_, Dot) -> true | _ -> false
    let pipe = function (_, Pipe) -> true | _ -> false
    let plus = function (_, Plus) -> true | _ -> false
    let dash = function (_, Dash) -> true | _ -> false
    let asterisk = function (_, Asterisk) -> true | _ -> false
    let slash = function (_, Slash) -> true | _ -> false
    let comma = function (_, Comma) -> true | _ -> false
    let is_type = function (_, Type) -> true | _ -> false
    let namespace = function (_, Namespace) -> true | _ -> false
    let is_module = function (_, Module) -> true | _ -> false
    let is_let = function (_, Let) -> true | _ -> false
    let is_true = function (_, True) -> true | _ -> false
    let is_false = function (_, False) -> true | _ -> false
    let backtick = function (_, Backtick) -> true | _ -> false
    let string = function (_, String) -> true | _ -> false
    let raw_string = function (_, Raw_string _) -> true | _ -> false
    let number = function (_, Number) -> true | _ -> false
    let comment = function (_, Comment _) -> true | _ -> false
    let name = function (_, Name) -> true | _ -> false
    let invalid = function (_, Invalid _) -> true | _ -> false
    let val_kw = function (_, Val) -> true | _ -> false
    let fn = function (_, Fn) -> true | _ -> false

    let trivia = function (_, Whitespace)
                        | (_, Comment _)
                          | (_, Invalid _) -> true
                        | _ -> false
  end

  module ParserState: PARSER_STATE = struct
    type tnode = {
        token: Token.t;
        leading_tokens: Token.t list;
        trailing_tokens: Token.t list
    }

    type t = {
        text: string;
        tokens: tnode array;
        index: int
    }

    let array_of_stream stream =
      let list = ref [] in
      Stream.iter (fun tok -> list := tok :: !list) stream;
      Array.of_list (List.rev !list)

    let rec last list =
      match list with
      | [] -> None
      | [item] -> Some item
      | _ :: tail -> last tail

    let span = function { token = (span, _) } -> span
    let full_span tnode =
      let (start_span, _) = try List.hd (tnode.leading_tokens)
                            with Failure _ -> tnode.token in
      let (end_span, _) = match last tnode.trailing_tokens with
        | Some tok -> tok
        | None -> tnode.token in
      Span.merge start_span end_span

    let substr state tnode = Span.substr (span tnode) state.text
    let full_substr state tnode = Span.substr (full_span tnode) state.text

    let rec read_trivia_for_line stream line so_far =
      match Stream.peek stream with
      | Some token ->
         let (span, _) = token in
         let start_line = Loc.line (Span.start span) in
         if TokenMatchers.trivia token && start_line = line then
           read_trivia_for_line stream line ((Stream.next stream) :: so_far)
         else
           List.rev so_far
      | None -> List.rev so_far

    let rec read_trivia stream so_far =
      match Stream.peek stream with
      | Some token ->
         if TokenMatchers.trivia token then
           read_trivia stream ((Stream.next stream) :: so_far)
         else
           List.rev so_far
      | None -> List.rev so_far

    let read_token stream =
      try
        let leading_trivia = read_trivia stream [] in
        let token = Stream.next stream in
        let (span, _) = token in
        let line = Loc.line (Span.finish span) in
        let trailing_trivia = read_trivia_for_line stream line [] in
        Some {
            token = token;
            leading_tokens = leading_trivia;
            trailing_tokens = trailing_trivia
        }
      with Stream.Failure -> None

    let tnodes_of_tokens stream =
      Stream.from (fun idx -> read_token stream)

    let create filename text =
      let input = Input.from_string filename text in
      {
        text = text;
        tokens = array_of_stream (tnodes_of_tokens (Lexer.lex input));
        index = 0
      }

    let text state = state.text
    let tokens state = state.tokens

    let rec pred_match toks preds =
      match (toks, preds) with
      | ([], []) -> true
      | (tok :: rtoks, pred :: rpreds) ->
         if pred tok.token then
           pred_match rtoks rpreds
         else
           false
      | _ -> false

    let get state idx = Array.get state.tokens idx

    let next state = (get state state.index, { state with index = state.index + 1 })

    let npeek num_toks state =
      let list = ref [] in
      let idx = ref state.index in
      let len = Array.length state.tokens in
      while (!idx - state.index) < num_toks && (!idx - state.index) < len do
        list := (get state !idx) :: !list;
        idx := !idx + 1
      done;
      List.rev !list

    let peek state =
      if state.index < Array.length state.tokens then
        Some (get state state.index)
      else
        None

    let is_empty state =
      (state.index >= Array.length state.tokens)

    let to_string state =
      let a = Array.sub state.tokens state.index (Array.length state.tokens - state.index) in
      let list = Array.to_list a in
      list
      |> List.map (fun t -> Printf.sprintf "\"%s\"" (String.escaped (substr state t)))
      |> String.concat " "

    let token_type tnode = let (_, tt) = tnode.token in tt

    let peek_type state =
      peek state |> (Option.map token_type)

    let starts_with state predicates =
      let num_toks = List.length predicates in
      let start_toks = npeek num_toks state in
      if num_toks != (List.length start_toks) then
        false
      else
        pred_match start_toks predicates
  end
  type parser_state = ParserState.t

  type 'a parser = parser_state -> ('a, parse_error) result

  let is_hex_digit chr = (chr >= '0' && chr <= '9')
                        || (chr >= 'a' && chr <= 'f')
                        || (chr >= 'A' && chr <= 'F')

  let rec read_hex_digits text idx end_idx =
    if idx < end_idx then
      if is_hex_digit (String.get text idx) then
        read_hex_digits text (idx + 1) end_idx
      else
        idx
    else
      idx

  let are_hex_digits text start_idx len =
    let rec check_hex_digits text idx len =
      if idx < (start_idx + len) then
        match String.get text idx with
        | c when is_hex_digit c -> check_hex_digits text (idx + 1) len
        | _ -> false
      else
        idx > start_idx in
    check_hex_digits text start_idx len

  let encode_utf8 buffer codepoint =
    let add_char chr = Buffer.add_char buffer (Char.chr chr) in
    match codepoint with
    | c when c >= 0x0000 && c <= 0x007F ->
        add_char (0b0111111 land c)
    | c when c >= 0x0080 && c <= 0x07FF -> begin
        add_char (0b11000000 lor (0b00011111 land (c lsr 6)));
        add_char (0b10000000 lor (0b00111111 land c))
      end
    | c when c >= 0x0800 && c <= 0xFFFF -> begin
        add_char (0b11100000 lor (0b00001111 land (c lsr 12)));
        add_char (0b10000000 lor (0b00111111 land (c lsr 6)));
        add_char (0b10000000 lor (0b00111111 land c))
      end
    | c when c >= 0x10000 && c <= 0x10FFFF -> begin
        add_char (0b11110000 lor (0b00000111 land (c lsr 18)));
        add_char (0b10000000 lor (0b00111111 land (c lsr 12)));
        add_char (0b10000000 lor (0b00111111 land (c lsr 6)));
        add_char (0b10000000 lor (0b00111111 land c))
      end
    | c -> raise (Failure (Printf.sprintf "Invalid Unicode codepoint: %d" codepoint))

  let rec read_string_contents span text idx max buffer errors =
    if idx < max then
      let sub_span len =
        let start_loc = Loc.advance_to (Span.start span) text idx in
        let len = min (max - idx) len in
        let end_idx = idx + len in
        let end_loc = Loc.advance_to start_loc text end_idx in
        Span.from start_loc end_loc in
      let error chr len err =
        Buffer.add_char buffer chr;
        read_string_contents span text (idx + len) max buffer (err :: errors) in
      let error_str len err =
        let substr = String.sub text idx len in
        Buffer.add_string buffer substr;
        read_string_contents span text (idx + len)
          max buffer (err :: errors) in
      let cont chr len =
        Buffer.add_char buffer chr;
        read_string_contents span text (idx + len) max buffer errors in
      match String.get text idx with
      | '\\' ->
         if (idx + 1) < max then
           let chr = String.get text (idx + 1) in
           match chr with
           | 'n' -> cont '\n' 2
           | 'r' -> cont '\r' 2
           | 't' -> cont '	' 2
           | '\\' -> cont chr 2
           | '0' -> cont (Char.chr 0) 2
           | '\'' -> cont chr 2
           | '"' -> cont chr 2
           | 'x' ->
              if (idx + 3) < max && are_hex_digits text (idx + 2) 2 then
                let value = int_of_string ("0" ^ (String.sub text (idx + 1) 3)) in
                let char = (Char.chr value) in
                if value > 127 then
                  let esc_span = sub_span 4 in
                  error char 4 (`Ascii_value_out_of_range esc_span)
                else
                  cont char 4
              else if (idx + 2) < max && is_hex_digit (String.get text (idx + 2)) then
                let esc_span = sub_span 3 in
                error_str (Span.length esc_span) (`Invalid_escape_sequence esc_span)
              else
                let esc_span = sub_span 2 in
                error_str (Span.length esc_span) (`Invalid_escape_sequence esc_span)
           | 'u' ->
              if (idx + 2) < max && (String.get text (idx + 2)) = '{' then
                let end_idx = read_hex_digits text (idx + 3) max in
                if end_idx < max then
                  if (String.get text end_idx) = '}' then
                    let len = end_idx - (idx + 3) in
                    if len < 1 || len > 6 then
                        let len = (end_idx + 1) - idx in
                        error_str len (`Unicode_value_out_of_range (sub_span len))
                    else
                      let value = int_of_string ("0x" ^ (String.sub text (idx + 3) len)) in
                      if value > 0x10FFFF then
                        let len = (end_idx + 1) - idx in
                        error_str len (`Unicode_value_out_of_range (sub_span len))
                      else if value >= 0xD800 && value <= 0xDFFF then
                        let len = (end_idx + 1) - idx in
                        error_str len (`Lone_surrogate_unicode_escape (sub_span len))
                      else
                        begin
                          encode_utf8 buffer value;
                          read_string_contents span text (end_idx + 1) max buffer errors
                        end
                  else
                    let len = end_idx - idx in
                    error_str len (`Invalid_escape_sequence (sub_span len))
                else
                  let len = end_idx - idx in
                  error_str len (`Invalid_escape_sequence (sub_span len))
              else
                error_str 2 (`Invalid_escape_sequence (sub_span 2))
           | c -> error c 2 (`Invalid_escape_sequence (sub_span 2))
         else
           error '\\' 1 (`Backslash_at_end_of_string span)
      | c -> cont c 1
      else
        List.rev errors

  let string_contents state tnode =
    let span = (ParserState.span tnode) in
    let (start, length) = Span.slice span in
    let buffer = Buffer.create length in
    let errors = read_string_contents span (ParserState.text state) (start + 1) (start + length - 1) buffer [] in
    (errors, Buffer.contents buffer)

  let metadata_of_tnode tnode =
    let open Source.Metadata in
    let open ParserState in
    let (span, _) = tnode.token in
    { span = span;
      full_span = ParserState.full_span tnode;
      tokens = [tnode.token];
      leading_tokens = tnode.leading_tokens;
      trailing_tokens = tnode.trailing_tokens }

  let parse_value_type state =
    if ParserState.starts_with state [TokenMatchers.name] then
      let (tnode, state) = ParserState.next state in
      match ParserState.token_type tnode with
      | Name ->
         let metadata = metadata_of_tnode tnode in
         let name_text = ParserState.substr state tnode in
         Some ([], (metadata, Source.ValueType.Name name_text), state)
      | _ -> None
    else
      None

  module Precedence: sig
    type t

    val prefix : Token.token_type -> ((unit * int) * Source.Expr.PrefixOp.t) option
    val infix : Token.token_type -> ((int * int) * Source.Expr.Binop.t) option
    val postfix : Token.token_type -> ((int * unit) * Source.Expr.PostfixOp.t) option
  end = struct
    module OpType = struct
      type op_type = Infix | Prefix | Postfix

      let value = function
        | Infix -> 0
        | Prefix -> 1
        | Postfix -> 2

      let compare l r = Int.compare (value l) (value r)
    end

    module TokenTypeMap = Map.Make(struct
                              type t = Token.token_type
                              let compare = Token.compare_type
                            end)

    type t = {
        prefix: ((unit * int) * Source.Expr.PrefixOp.t) TokenTypeMap.t;
        infix: ((int * int) * Source.Expr.Binop.t) TokenTypeMap.t;
        postfix: ((int * unit) * Source.Expr.PostfixOp.t) TokenTypeMap.t
    }

    let empty = {
        prefix = TokenTypeMap.empty;
        infix = TokenTypeMap.empty;
        postfix = TokenTypeMap.empty
      }

    let plus_prefix t tok bp out =
      let (_, rbp) = bp in
      { t with prefix = TokenTypeMap.add tok (((), rbp), out) t.prefix }

    let plus_infix t tok bp out =
      { t with infix = TokenTypeMap.add tok (bp, out) t.infix }

    let plus_postfix t tok bp out =
      let (lbp, _) = bp in
      { t with postfix = TokenTypeMap.add tok ((lbp, ()), out) t.postfix }

    let mapping: t =
      let current_chunk = ref 1 in
      let next_group () = current_chunk := !current_chunk + 2 in
      let group () =
        let value = !current_chunk in
        (value, value + 1) in
      let record = ref empty in
      let prefix tok out =
        let bp = group () in
        let new_val = plus_prefix !record tok bp out in
        record := new_val;
        () in
      let infix tok out =
        let bp = group () in
        let new_val = plus_infix !record tok bp out in
        record := new_val;
        () in
      let postfix tok out =
        let bp = group () in
        let new_val = plus_postfix !record tok bp out in
        record := new_val;
        ()
      in

      let module T = Token in
      let module B = Source.Expr.Binop in
      let module Pre = Source.Expr.PrefixOp in
      let module Post = Source.Expr.PostfixOp in

      next_group ();
      infix T.Double_equal B.Equal;
      infix T.Less_than B.Less_than;
      infix T.Less_equal B.Less_equal;
      infix T.Greater_than B.Greater_than;
      infix T.Greater_equal B.Greater_equal;

      next_group ();
      infix T.Plus B.Plus;
      infix T.Dash B.Minus;

      next_group ();
      infix T.Asterisk B.Times;
      infix T.Slash B.Divide;

      next_group ();
      postfix T.Question Post.Question;

      next_group ();
      prefix T.Plus Pre.Plus;
      prefix T.Dash Pre.Minus;

      next_group ();
      postfix T.Colon Post.Colon;

      next_group ();
      infix T.Dot B.Dot;
      postfix T.Left_paren Post.Left_paren;

      !record

    let prefix tok = TokenTypeMap.find_opt tok mapping.prefix
    let infix tok = TokenTypeMap.find_opt tok mapping.infix
    let postfix tok = TokenTypeMap.find_opt tok mapping.postfix
  end

  let binary_op token_type =
    match Precedence.infix token_type with
    | Some ((l, r), o) -> Some (l, r, o)
    | None -> None

  let prefix_op token_type =
    match Precedence.prefix token_type with
    | Some ((l, r), o) -> Some (l, r, o)
    | None -> None

  let postfix_op token_type =
    match Precedence.postfix token_type with
    | Some ((l, r), o) -> Some (l, r, o)
    | None -> None

  let read_namespace state =
    let open TokenMatchers in
    let module Metadata = Source.Metadata in
    let rec read_names state names =
      if ParserState.starts_with state [name] then
        let (tnode, state) = ParserState.next state in
        if ParserState.starts_with state [dot] then
          let (dot_node, state) = ParserState.next state in
          read_names state (dot_node :: tnode :: names)
        else
          (state, tnode :: names)
      else
        (state, names) in
    let state, rev_names = read_names state [] in
    let names = List.rev rev_names in
    match names with
    | [] -> None
    | first :: rest ->
       let first_md = metadata_of_tnode first in
       let metadata = List.map metadata_of_tnode rest
                      |> List.fold_left Metadata.merge first_md in
       let text = List.map (fun n -> ParserState.substr state n) names
                |> String.concat "" in
       Some (metadata, text, state)

  let read_qualified_name state =
    let module Metadata = Source.Metadata in
    let module Expr = Source.Expr in
    match read_namespace state with
    | Some (metadata, text, state) ->
       if ParserState.starts_with state [TokenMatchers.double_colon] then
         let (double_colon, state) = ParserState.next state in
         let dcmd = metadata_of_tnode double_colon in
         let ns = (metadata, Name.input text) in
         if ParserState.starts_with state [TokenMatchers.name] then
           let (name_tok, state) = ParserState.next state in
           let name_md = metadata_of_tnode name_tok in
           let name_text = ParserState.substr state name_tok in
           let full_md = Metadata.merge (Metadata.merge metadata dcmd) name_md in
           let name = (name_md, (Name.input name_text)) in
           Some ([], (full_md, (Expr.Name (Expr.Qualified_name (ns, name)))), state)
         else
           let full_md = Metadata.merge metadata dcmd in
           let err_span = Span.singular (Span.finish (Metadata.span dcmd)) in
           let error = `Expected_identifier err_span in
           Some ([error], (full_md, (Expr.Name (Expr.Invalid_qualified_name ns))), state)
       else
         None
    | None -> None

  let extract_md =
    let open Source.Expr in
    function Named_arg (md, _, _) -> md | Unnamed_arg (md, _) -> md

  let rec parse_expr_bp state errors min_bp =
    let module Expr = Source.Expr in
    match ParserState.peek state with
    | Some tok ->
       match parse_left_expr state errors tok min_bp with
       | Some (lerrors, lhs, state) -> begin
           match read_postfix state lerrors lhs min_bp with
           | Some (rerrors, result, state) ->
              Some (rerrors, result, state)
           | None -> Some (lerrors, lhs, state)
         end
       | None -> None
    | None -> None

  and read_arg state =
    let open TokenMatchers in
    let module Metadata = Source.Metadata in
    let module Expr = Source.Expr in
    if ParserState.starts_with state [name; equal] then
      let (name_tok, state) = ParserState.next state in
      let (equal, state) = ParserState.next state in
      let name_str = ParserState.substr state name_tok in
      let name = Name.input name_str in
      let name_md = metadata_of_tnode name_tok in
      let prefix_md = Metadata.merge name_md (metadata_of_tnode equal) in
      match parse_expr_bp state [] 0 with
      | Some (errors, (md, expr), state) ->
         let metadata = Metadata.merge prefix_md md in
         Some (errors,
               Expr.Named_arg (metadata, name, (md, expr)),
               state)
      | None ->
         let loc = Span.finish (ParserState.span equal) in
         let span = Span.singular loc in
         let module M = Metadata in
         let metadata = {
             M.span = span;
             M.full_span = span;
             M.tokens = [];
             M.leading_tokens = [];
             M.trailing_tokens = []
           } in
         Some ([`Expected_expression span],
               Expr.Named_arg (prefix_md, name, (metadata, Expr.Invalid)),
               state)
    else
      match parse_expr_bp state [] 0 with
      | Some (errors, expr, state) ->
         Some (errors, Expr.Unnamed_arg expr, state)
      | None -> None

  and read_arg_list state =
    let module Metadata = Source.Metadata in
    let module Expr = Source.Expr in
    let rec read_args state errors args =
      match read_arg state with
      | Some (aerrors, arg, state) ->
         let errors = List.rev_append aerrors errors in
         if ParserState.starts_with state [TokenMatchers.comma] then
           let (comma, state) = ParserState.next state in
           read_args state errors (arg :: args)
         else if ParserState.starts_with state [TokenMatchers.right_paren] then
           (errors, arg :: args, state)
         else
           let span = Span.singular (Span.finish (Metadata.span (extract_md arg))) in
           let errors = (`Expected_comma_or_right_paren_args_list span) :: errors in
           read_args state errors (arg :: args)
      | None -> (errors, args, state)
    in
    let (errors, args, state) = read_args state [] [] in
    (errors, args, state)

  and read_postfix state errors lhs min_bp =
    let module Metadata = Source.Metadata in
    let module Expr = Source.Expr in
    let module P = Expr.PostfixOp in
    match ParserState.peek_type state |> (Option.map postfix_op) |> Option.join with
    | Some (l_bp, _, _) when l_bp < min_bp -> Some (errors, lhs, state)
    | Some (l_bp, (), P.Colon) ->
       let (colon, state) = ParserState.next state in
       begin
         match parse_value_type state with
         | Some (verrors, (vmd, value), state) ->
            let (lmd, _) = lhs in
            let colon_md = metadata_of_tnode colon in
            let metadata = Metadata.merge (Metadata.merge lmd colon_md) vmd in
            let errors = List.rev_append verrors errors in
            let new_lhs = (metadata, Expr.Type_ann (lhs, (vmd, value))) in
            read_postfix state errors new_lhs min_bp
         | None ->
            let module M = Metadata in
            let metadata = metadata_of_tnode colon in
            let span = Metadata.span metadata in
            let md = {
                M.span = span;
                M.full_span = span;
                M.tokens = [colon.token];
                M.leading_tokens = [];
                M.trailing_tokens = []
            } in
            let errors = (`Expected_type_expression span) :: errors in
            let new_lhs = (metadata, Expr.Type_ann (lhs, (md, Invalid))) in
            read_postfix state errors new_lhs min_bp
       end
    | Some (l_bp, (), P.Left_paren) ->
       let (lparen, state) = ParserState.next state in
       begin
         let (lerrors, args, state) = read_arg_list state in
         let args = List.rev args in
         let lmeta = metadata_of_tnode lparen in
         let metadata = List.fold_left Metadata.merge lmeta (List.map extract_md args) in
         let metadata = Metadata.merge lmeta metadata in
         let errors = List.rev_append lerrors errors in
         if ParserState.starts_with state [TokenMatchers.right_paren] then
           let (right_paren, state) = ParserState.next state in
           let metadata = Metadata.merge metadata (metadata_of_tnode right_paren) in
           let call = Expr.Application (lhs, args) in
           Some (errors, (metadata, call), state)
         else
           let span = Metadata.span metadata in
           let call = Expr.Application (lhs, args) in
           let errors = (`Expected_comma_or_right_paren_args_list span) :: errors in
           Some (errors, (metadata, call), state)
       end
    | Some (l_bp, (), postfix) ->
       let (tok, state) = ParserState.next state in
       let (lmd, _) = lhs in
       let metadata = Metadata.merge lmd (metadata_of_tnode tok) in
       let new_lhs = (metadata, Expr.Postfix_op (postfix, lhs)) in
       read_postfix state errors new_lhs min_bp
    | None -> read_infix state errors lhs min_bp

  and read_infix state errors lhs min_bp =
    let module Metadata = Source.Metadata in
    let module Expr = Source.Expr in
    match ParserState.peek_type state |> (Option.map binary_op) |> Option.join with
    | Some (l_bp, _, _) when l_bp < min_bp -> Some (errors, lhs, state)
    | Some (l_bp, r_bp, op) ->
       let (tok, state) = ParserState.next state in
       let (lmd, _) = lhs in
       let middle_md = metadata_of_tnode tok in begin
           match parse_expr_bp state errors r_bp with
           | Some (errors, (rmd, rhs), state) -> begin
               let metadata = Metadata.merge (Metadata.merge lmd middle_md) rmd in
               let new_lhs = (metadata, Expr.Binop (op, lhs, (rmd, rhs))) in
               read_postfix state errors new_lhs min_bp
             end
           | None ->
              let span = Span.singular (Span.finish (ParserState.span tok)) in
              let (lmd, _) = lhs in
              let metadata = Metadata.merge lmd (metadata_of_tnode tok) in
              let module M = Metadata in
              let md = {
                  M.span = span;
                  M.full_span = span;
                  M.tokens = [];
                  M.leading_tokens = [];
                  M.trailing_tokens = []
                } in
                Some ((`Expected_expression span) :: errors,
                      (metadata, Expr.Binop (op, lhs, (md, Expr.Invalid))),
                      state)
         end
    | None -> Some (errors, lhs, state)

  and parse_left_expr state errors tok min_bp =
    let module Expr = Source.Expr in
    match prefix_op (ParserState.token_type tok) with
    | Some ((), r_bp, op) -> begin
        let (_, state) = ParserState.next state in
        match parse_expr_bp state [] r_bp with
        | Some (ferrors, exp, state) -> begin
            let metadata = Source.Metadata.merge (metadata_of_tnode tok) (Expr.metadata exp) in
            Some (List.rev_append ferrors errors, (metadata, Expr.Prefix_op (op, exp)), state)
          end
        | None -> None
      end
    | None -> parse_simple_expr state errors 0

  and parse_simple_expr state errors min_bp =
    let module Expr = Source.Expr in
    let starts_with = ParserState.starts_with state in
    let open TokenMatchers in
    if starts_with [string] then
      let (tnode, state) = ParserState.next state in
      let (serrors, contents) = string_contents state tnode in
      let metadata = metadata_of_tnode tnode in
      Some (List.rev_append serrors errors, (metadata, Expr.String contents), state)
    else if starts_with [raw_string] then
      let (tnode, state) = ParserState.next state in
      let metadata = metadata_of_tnode tnode in
      match tnode.token with
      | (_, Raw_string prefix) ->
         let start, length = Span.slice (ParserState.span tnode) in
         let start_len = prefix + 2 in
         let end_len = prefix + 1 in
         let contents = String.sub (ParserState.text state)
                          (start + start_len)
                          (length - start_len - end_len) in
         Some (errors, (metadata, Expr.String contents), state)
      | _ -> raise (Failure "!!!")
    else if starts_with [number] then
      let (tnode, state) = ParserState.next state in
      let metadata = metadata_of_tnode tnode in
      let text = ParserState.substr state tnode in
      Some (errors, (metadata, Expr.Number text), state)
    else if starts_with [is_true] then
      let (tnode, state) = ParserState.next state in
      let metadata = metadata_of_tnode tnode in
      Some (errors, (metadata, Expr.Boolean true), state)
    else if starts_with [is_false] then
      let (tnode, state) = ParserState.next state in
      let metadata = metadata_of_tnode tnode in
      Some (errors, (metadata, Expr.Boolean false), state)
    else if starts_with [name] then
      match read_qualified_name state with
      | Some (err, qualified, state) -> Some (List.rev_append err errors, qualified, state)
      | None ->
         let (tnode, state) = ParserState.next state in
         let metadata = metadata_of_tnode tnode in
         let text = ParserState.substr state tnode in
         Some (errors, (metadata, Expr.Name (Expr.Simple_name (metadata, Name.input text))), state)
    else if starts_with [left_paren] then
      let (tnode, state) = ParserState.next state in
      match parse_expr_bp state errors 0 with
      | Some (ierrors, expr, state) ->
         let errors = List.rev_append ierrors errors in
         if ParserState.starts_with state [right_paren] then
           let (_, state) = ParserState.next state in
           Some (errors, expr, state)
         else
           let first_span = ParserState.span tnode in
           let (md, exp) = expr in
           let expr_span = Source.Metadata.span md in
           let final_span = Span.singular (Span.finish expr_span) in
           Some ((`Missing_closing_paren (first_span, final_span)) :: errors,
                 expr,
                 state)
      | None -> Some ((`Expected_expression (ParserState.span tnode)) :: errors,
                      (metadata_of_tnode tnode, Expr.Invalid),
                      state)
    else
      None

  let parse_expr state = parse_expr_bp state [] 0

  let parse_val state =
    let starts_with = ParserState.starts_with state in
    let open TokenMatchers in
    let (val_tok, state) = ParserState.next state in
    if starts_with [name] then
      let (name_tok, state) = ParserState.next state in
      ()
      (* capture name *)
    else
      ()
      (* Fail here with an error *)

  (* let parse_namespace state =
   *   let open TokenMatchers in
   *   if ParserState.starts_with state [namespace] then
   *   else *)
      

  let parse_file state =
    (* let open TokenMatchers in
     * if ParserState.starts_with state [] then *)
    parse_expr state

  let%test_unit "unicode_out_of_bounds" =
    let sprintf = Printf.sprintf in
    let filename = "<unknown>" in
    let text = {|"foo bar baz \u{FFFFFF}"|} in
    let parser_state = ParserState.create filename text in
    match parse_expr_bp parser_state [] 0 with
    | Some ([`Unicode_value_out_of_range span],
       (_, Source.Expr.String {|foo bar baz \u{FFFFFF}|}), _) ->
       let expected = Span.from
                        (Loc.create filename 1 13 13)
                        (Loc.create filename 1 23 23) in
       if not (Span.equal span expected) then
         raise (Failure (sprintf "Span mismatch. %s (actual) vs %s (expected)"
                           (Span.to_string span)
                           (Span.to_string expected)))
    | Some (errors, (_, Source.Expr.String contents), _) ->
       raise (Failure (sprintf "Didn't get expected error. Errors: %s; contents: %s"
                         (ParseError.string_of_errors errors)
                         contents))
    | None -> raise (Failure "No expression found.")

end

