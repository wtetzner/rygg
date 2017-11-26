
module Span = Compiler.Span
module Location = Span.Location
module Out_channel = Core.Out_channel
module In_channel = Core.In_channel
module Command = Core.Command

let write_bytes_to_file file bytes =
  Out_channel.with_file file ~f:(fun f -> Out_channel.output_string f bytes);
  ()

let load_string filename =
  In_channel.with_file filename ~f:(fun f -> In_channel.input_all f)

let parse_expr str =
  Printf.printf "parsing \"%s\"" str;
  print_newline ();
    let parsed = Vmu.Asm.Parser.Parser.expression (Vmu.Asm.Parser.Lexer.tokens "<unknown>" str) in
    Printf.printf "expression \"%s\" parsed to: %s\n" str (Vmu.Asm.Expression.to_string parsed)

let parse_statement str =
  Printf.printf "parsing statement \"%s\"" str;
  print_newline ();
  let parsed = Vmu.Asm.Parser.Parser.instruction (Vmu.Asm.Parser.Lexer.tokens "<unknown>" str) in
  Printf.printf "statement \"%s\" parsed to: %s\n" str (Vmu.Asm.Statement.to_string parsed)


let assemble input_file output_file =
  Vmu.Asm.Parser.assemble input_file output_file

let vmu_cmd =
  let assemble_cmd =
    Command.basic
      ~summary:"Assembler for Dreamcast VMU"
      Command.Spec.(
      empty
      +> flag ~aliases:["-o"] "-output" (required string) ~doc:"output-file"
      +> anon ("input-file" %: file)
    )
      (fun output filename () ->
        assemble filename output
      ) in
  let compile_cmd =
    Command.basic
      ~summary:"Compiler for the Wombat programming language"
      Command.Spec.(
      empty
      +> flag ~aliases:["-o"] "-output" (required string) ~doc:"output-file"
      +> anon ("input-file" %: file)
    )
      (fun output filename () ->
        raise (Failure "compile is not yet implemented")
      ) in
  Command.group ~summary:"Operations for Dreamcast VMU"
    [ "assemble", assemble_cmd; "compile", compile_cmd ]

let command =
  Command.group ~summary:"Manipulate dates"
    [ "vmu", vmu_cmd ]

let () = Command.run command

