
open Compiler
open Wombat

let () =
  let loc = Loc.create "some-file.cmp" 3 5 15 in
  let span = Span.from loc (Loc.create "some-file.cmp" 3 15 25) in
  let span2 = Span.from loc (Loc.create "foo.cmp" 7 4 25) in
  let span3 = Span.from loc (Loc.create "some-file.cmp" 4 15 25) in
  let name = Name.internal "foo" in
  Printf.printf "%s\n" (Loc.to_string loc);
  Printf.printf "%s\n" (Span.to_string span);
  Printf.printf "%s\n" (Span.to_string span2);
  Printf.printf "%s\n" (Span.to_string span3);
  Printf.printf "%s\n" (Name.to_string name)

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* let () =
 *   let filename = Array.get Sys.argv 1 in
 *   let text = read_whole_file filename in
 *   let input = Input.from_string filename text in
 *   (\* Printf.printf "input: \"%s\"\n" (Input.to_string input); *\)
 *   let tokens = Wombat.SourceLexer.lex input in
 *   (\* Stream.iter (fun tok -> Printf.printf "%s\n" (Wombat.Tokens.to_string tok))
 *    *   tokens *\)
 *   Stream.iter (fun tok -> ()) tokens *)


let () =
  let filename = Array.get Sys.argv 1 in
  let text = read_whole_file filename in
  let input = Input.from_string filename text in
  Printf.printf "\n**fast lexer**\n";
  let substr token = Wombat.Token.substr text token in
  let tokens = Wombat.Lexer.lex input in
  (* Stream.iter (fun tok -> Printf.printf "%s \"%s\"\n" (Wombat.Token.to_string tok) (String.escaped (substr tok)))
   *   tokens; *)
  let parser_state = Wombat.SourceParser.ParserState.create filename text in
  let results = Wombat.SourceParser.parse_expr parser_state in
  let module Expr = Wombat.Source.Expr in
  match results with
  | Some (errors, expr, state) -> Printf.printf "Errors: %s\nExpr: %s\nState: %s\n\n"
                                    (Wombat.ParseError.string_of_errors errors)
                                    (Expr.to_string expr)
                                    (Wombat.SourceParser.ParserState.to_string state)
  | None -> Printf.printf "Could not parse expression\n"
  (* Stream.iter (fun tok -> ()) tokens *)

              
