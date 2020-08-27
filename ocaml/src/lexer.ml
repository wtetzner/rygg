
open Location

type 'a token_constructor = Span.t -> Input.regexp_match_result -> 'a
type 'a matcher =
  | Re of string * 'a token_constructor
  | Lit of string * 'a token_constructor

module type RULES = sig
  type token

  val is_invalid : token -> bool
  val is_whitespace : token -> bool

  val invalid : token token_constructor
  val rules : (token matcher) list
end

module Make(Rules: RULES): sig
  type token = Rules.token

  val is_invalid : token -> bool
  val is_whitespace : token -> bool

  val lex : Input.t -> token Stream.t
end = struct
  type token = Rules.token

  type lexer_error = [
    | `Unexpected_character of Loc.t
  ]

  let is_invalid = Rules.is_invalid
  let is_whitespace = Rules.is_whitespace

  let rule_to_matcher rule =
    let str, constr =
      match rule with
      | Re (str, constr) -> (str, constr)
      | Lit (str, constr) -> (Str.quote str, constr)
    in
    let re = Str.regexp str in
    let lex input = begin
        match Input.match_regexp input re with
        | Some { string; groups } -> begin
            let start = Input.loc input in
            let finish = Input.end_loc input (String.length string) in
            let span = (Span.from start finish) in
            Some (span, constr span { string; groups })
          end
        | None -> None
      end
    in
    lex

  let matchers = List.map rule_to_matcher Rules.rules

  let rec read_token_from input matchers =
    match matchers with
    | [] -> None
    | m :: rest ->
       match m input with
       | Some result -> Some result
       | None -> read_token_from input rest

  let rec skip_invalid input =
    if Input.is_empty input then
      input
    else
      let next_char = Input.advance_by input 1 in
      match read_token_from next_char matchers with
      | Some tok -> next_char
      | None -> skip_invalid next_char

  let read_invalid_token input =
    let new_input = skip_invalid input in
    let span = Span.from (Input.loc input) (Input.loc new_input) in
    let str = Input.substring input span in
    (span, Rules.invalid span { Input.string = str; Input.groups = [] })

  let rec read_token input =
    if Input.is_empty input then
      None
    else begin
        match read_token_from input matchers with
        | Some tok -> Some tok
        | None -> let span, invalid = read_invalid_token input in
                  if Span.is_singular span then
                    None
                  else
                    Some (span, invalid)
      end

  let lex_token input =
    if Input.is_empty !input then
      None
    else
      let tok = read_token !input in
      match tok with
      | Some (span, token) -> begin
          let len = (Loc.offset (Span.finish span)) - (Loc.offset (Span.start span)) in
          input := Input.advance_by !input len;
          Some token
        end
      | None -> None

  let lex input =
    let iref = ref input in
    Stream.from (fun idex -> lex_token iref)

end
