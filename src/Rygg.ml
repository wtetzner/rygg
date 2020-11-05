
module Command = Core.Command
module TextSpan = Text.Span

let vmu_assemble input_file inc_dir output_file =
  Vmu.Asm.Parser.assemble input_file inc_dir output_file

let vmu_disassemble input_file =
  Vmu.Disasm.disassemble input_file

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let vmu_compile input_file output_file =
  let () = Printf.printf "text_span: %s\n" (TextSpan.to_string (TextSpan.from 10 23)) in
  let () = Printf.printf "text_span debug: %s\n" (TextSpan.debug_string (TextSpan.from 10 23)) in
  let text = read_whole_file input_file in
  let parser_state = Compiler.Rygg.SourceParser.ParserState.create input_file text in
  let results = Compiler.Rygg.SourceParser.parse_expr parser_state in
  let module Expr = Compiler.Rygg.Source.Expr in
  match results with
  | Some (errors, expr, state) -> Printf.printf "Errors: %s\nExpr: %s\nState: %s\n\n"
                                    (Compiler.Rygg.ParseError.string_of_errors errors)
                                    (Expr.to_string expr)
                                    (Compiler.Rygg.SourceParser.ParserState.to_string state)
  | None -> Printf.printf "Could not parse expression\n"

let vmu_cmd =
  let disassemble_cmd =
    Command.basic
      ~summary:"Disassembler for Dreamcast VMU"
      ~readme:(fun () -> "Disassembler for Dreamcast VMU")
      Command.Let_syntax.(
      let%map_open filename = anon ("input-file" %: string) in
      fun () -> vmu_disassemble filename
    )
  in
  let assemble_cmd =
    Command.basic
      ~summary:"Assembler for Dreamcast VMU"
      ~readme:(fun () -> "Assembler for Dreamcast VMU")
      Command.Let_syntax.(
      let%map_open output = flag ~aliases:["-o"] "--output" (required string) ~doc:"output Specifies the output filename"
      and inc_dir = flag ~aliases:["-i"] "--include" (optional string) ~doc:"dir Specifies the includes directory"
      and filename = anon ("INPUT_FILE" %: string)
      in
      fun () -> vmu_assemble filename inc_dir output
    )
  in
  let compile_cmd =
    Command.basic
      ~summary:"Compiler for the Rygg programming language"
      ~readme:(fun () -> "Compiler for the Rygg programming language")
      Command.Let_syntax.(
      let%map_open output = flag ~aliases:["-o"] "-output" (required string) ~doc:"Output File"
      and filename = anon ("input-file" %: string)
      in
      fun () -> vmu_compile filename output
    ) in
  Command.group ~summary:"Operations for Dreamcast VMU"
    [ "assemble", assemble_cmd;
      "disassemble", disassemble_cmd;
      "compile", compile_cmd ]

let command =
  Command.group ~summary:"Compiler and build tool for Dreamcast VMU"
    [ "vmu", vmu_cmd ]

let () = Command.run command

