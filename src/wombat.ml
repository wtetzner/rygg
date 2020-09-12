
open Compiler

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

module type PATH = sig
  type t
  type name

  val create : name list -> t
  val create_ns : name -> name list -> t
  val namespace : t -> name option
  val plus : t -> name -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val debug_string : t -> string
end

module Make_path
         (Name: NAME): PATH
       with type name = Name.t = struct
  type name = Name.t

  type t = {
      namespace: Name.t option;
      parts: name array
  }

  let has_ns path =
    match path.namespace with
    | Some _ -> true
    | None -> false
                       
  let create names =
    let parts = Array.of_list names in
    { namespace = None; parts }

  let create_ns namespace names =
    let intermediate = create names in
    { intermediate with namespace = Some namespace }

  let namespace path = path.namespace

  let plus path part =
    let { parts } = path in
    { path with parts = Array.append parts [| part |] }

  let ns_equal ns1 ns2 =
    match (ns1, ns2) with
    | (Some n1, Some n2) -> Name.equal n1 n2
    | (None, None) -> true
    | _ -> false

  let ns_compare ns1 ns2 =
    match (ns1, ns2) with
    | (Some n1, Some n2) -> Name.compare n1 n2
    | (None, None) -> 0
    | (Some _, None) -> 1
    | (None, Some _) -> -1

  let equal left right =
    if (ns_equal left.namespace right.namespace)
       && (Array.length left.parts) = (Array.length right.parts) then
      let still_equal = ref true in
      let pos = ref 0 in
      while !still_equal do
        if not (Name.equal (Array.get left.parts !pos) (Array.get right.parts !pos)) then
          still_equal := false
        else ();
        pos := !pos + 1
      done;
      !still_equal
    else
      false

  let compare left right =
    let namespace_cmp = ns_compare left.namespace right.namespace in
    if namespace_cmp = 0 then
      let llen = Array.length left.parts in
      let rlen = Array.length right.parts in
      let pos = ref 0 in
      let cmp_result = ref 0 in
      while !cmp_result = 0 do
        if !pos >= llen && !pos < rlen then
          cmp_result := -1
        else if !pos >= rlen && !pos < llen then
          cmp_result := 1
        else if !pos < llen && !pos < rlen then
          cmp_result := Name.compare (Array.get left.parts !pos) (Array.get right.parts !pos)
        else ();
        pos := !pos + 1
      done;
      !cmp_result
    else
      namespace_cmp

  let to_string path =
    let output = match path.namespace with
      | None -> ref ""
      | Some ns -> ref ((Name.to_string ns) ^ "::") in
    let first = ref true in
    Array.iter
      (fun name ->
        begin
          if !first then
            first := false
          else 
            output := !output ^ ".";
        end;
        output := !output ^ Name.to_string name)
      path.parts;
    !output

  let debug_string path =
    let output = ref "{ namespace = " in
    (match path.namespace with
     | None -> output := !output ^ "None"
     | Some ns -> output := !output ^ ("Some " ^ (Name.debug_string ns)));
    output := !output ^ ", parts = [";
    let first = ref true in
    Array.iter (fun name -> (if !first then
                      first := false
                    else
                      output := !output ^ ", ");
                    output := !output ^ (Name.debug_string name)) path.parts;
    output := !output ^ "]";
    output := !output ^ " }";
    !output
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

module Path = Make_path(Name)

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
    | Greater_than
    | Arrow
    | Back_arrow
    | Colon
    | Double_colon
    | Equal
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
    | Greater_than
    | Arrow
    | Back_arrow
    | Colon
    | Double_colon
    | Equal
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
      | Greater_than -> ">"
      | Arrow -> "->"
      | Back_arrow -> "<-"
      | Colon -> ":"
      | Double_colon -> "::"
      | Equal -> "="
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
    type node = Name of string
    type t = Metadata.t * node

    let to_string value =
      let (_, node) = value in
      match node with
      | Name name -> name
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

      let to_string = function
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
        | Divide -> "/"
        | Dot -> "."
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

      let to_string = function
        | Question -> "?"
        | Colon -> ":"
    end

    type node =
      | Path of Path.t
      | Type_ann of t * value_type
      | Binop of Binop.t * t * t
      | Prefix_op of PrefixOp.t * t
      | Postfix_op of PostfixOp.t * t
      | Application of t * t list
      | Number of string
      | String of string
      | Invalid
    and t = metadata * node

    let is_complex expr =
      let (_, node) = expr in
      match node with
      | Path _ -> false
      | Type_ann _ -> true
      | Binop (_, _, _) -> true
      | Prefix_op (_, _) -> true
      | Postfix_op (_, _) -> true
      | Application (_, _) -> false
      | Number _ | String _ | Invalid -> false

    let rec to_string expr =
      let open Printf in
      let (_, node) = expr in
      match node with
      | Path path -> Path.to_string path
      | Type_ann (e, v) -> sprintf "%s:%s" (wrap e) (ValueType.to_string v)
      | Binop (Binop.Dot, l, r) ->
         sprintf "%s.%s" (wrap l) (wrap r)
      | Binop (op, l, r) ->
         sprintf "%s %s %s" (wrap l) (Binop.to_string op) (wrap r)
      | Prefix_op (op, e) -> sprintf "%s%s" (PrefixOp.to_string op) (wrap e)
      | Postfix_op (op, e) -> sprintf "%s%s" (wrap e) (PostfixOp.to_string op)
      | Application (c, a) ->
         let args = String.concat ", " (List.map to_string a) in
         sprintf "%s(%s)" (wrap c) args
      | Number num -> num
      | String str -> sprintf "\"%s\"" (String.escaped str)
      | Invalid -> "<INVALID>"

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
    | `Path of Path.t
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
  ]

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
      | ',' | '"' | '?' | '!' -> true
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
    | '<' -> if Input.starts_with !input "<-" then
               handle_simple input 2 Back_arrow
             else
               single Less_than
    | '>' -> single Greater_than
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
    | `Invalid_escape_sequence of Span.t
    | `Ascii_value_out_of_range of Span.t
    | `Unicode_value_out_of_range of Span.t
    | `Lone_surrogate_unicode_escape of Span.t
    | `Missing_closing_paren of Span.t * Span.t
    | `Expected_expression of Span.t
    | `Expected_type_expression of Span.t
  ]

  let to_string error =
    let open Printf in
    match error with
    | `Invalid_token span -> sprintf "Invalid_token %s" (Span.to_string span)
    | `Backslash_at_end_of_string span -> sprintf "Backslash_at_end_of_string %s" (Span.to_string span)
    | `Invalid_escape_sequence span -> sprintf "Invalid_escape_sequence %s" (Span.to_string span)
    | `Ascii_value_out_of_range span -> sprintf "Ascii_value_out_of_range %s" (Span.to_string span)
    | `Unicode_value_out_of_range span -> sprintf "Unicode_value_out_of_range %s" (Span.to_string span)
    | `Lone_surrogate_unicode_escape span -> sprintf "Lone_surrogate_unicode_escape %s" (Span.to_string span)
    | `Missing_closing_paren (paren_span, span) -> sprintf "Missing_closing_paren %s" (Span.to_string span)
    | `Expected_expression span -> sprintf "Expected_expression %s" (Span.to_string span)
    | `Expected_type_expression span -> sprintf "Expected_type_expression %s" (Span.to_string span)

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
      let (tnode, state) = (ParserState.next state) in
      match ParserState.token_type tnode with
      | Name ->
         let metadata = metadata_of_tnode tnode in
         let name_text = ParserState.substr state tnode in
         Some ([], (metadata, Source.ValueType.Name name_text), state)
      | _ -> None
    else
      None

  let binary_op token_type =
    let module T = Token in
    let module E = Source.Expr in
    let module B = E.Binop in
    match token_type with
    | T.Plus -> Some (1, 2, B.Plus)
    | T.Dash -> Some (1, 2, B.Minus)

    | T.Asterisk -> Some (3, 4, B.Times)
    | T.Slash -> Some (3, 4, B.Divide)

    | T.Dot -> Some (51, 50, B.Dot)
    | _ -> None

  let prefix_op token_type =
    let module T = Token in
    let module E = Source.Expr in
    let module P = E.PrefixOp in
    match token_type with
    | T.Plus -> Some ((), 6, P.Plus)
    | T.Dash -> Some ((), 6, P.Minus)
    | _ -> None

  let postfix_op token_type =
    let module T = Token in
    let module E = Source.Expr in
    let module P = Source.Expr.PostfixOp in
    match token_type with
    | T.Question -> Some (5, (), P.Question)
    | T.Colon -> Some (5, (), P.Colon)
    | _ -> None

  let rec parse_expr_bp state errors min_bp =
    let module Expr = Source.Expr in
    match ParserState.peek state with
    | Some tok ->
       match parse_left_expr state errors tok min_bp with
       | Some (lerrors, lhs, state) -> begin
           match read_postfix state errors lhs min_bp with
           | Some (rerrors, result, state) ->
              Some (List.rev_append (List.rev_append rerrors lerrors) errors,
                    result,
                    state)
           | None -> Some (List.rev_append lerrors errors, lhs, state)
         end
       | None -> None
    | None -> None

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
            let metadata = metadata_of_tnode colon in
            let span = Metadata.span metadata in
            Some ((`Expected_type_expression span) :: errors,
             (metadata, Invalid),
             state)
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
           | Some (rerrors, (rmd, rhs), state) -> begin
               let metadata = Metadata.merge (Metadata.merge lmd middle_md) rmd in
               let errors = List.rev_append rerrors errors in
               let new_lhs = (metadata, Expr.Binop (op, lhs, (rmd, rhs))) in
               read_postfix state errors new_lhs min_bp
             end
           | None ->
              let span = Span.singular (Span.finish (ParserState.span tok)) in
              let (lmd, _) = lhs in
              let metadata = Metadata.merge lmd (metadata_of_tnode tok) in
              Some ((`Expected_expression span) :: errors,
                    (metadata, Invalid),
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
    else if starts_with [name] then
      let (tnode, state) = ParserState.next state in
      let metadata = metadata_of_tnode tnode in
      let text = ParserState.substr state tnode in
      Some (errors, (metadata, Expr.Path (Path.create [Name.input text])), state)
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

