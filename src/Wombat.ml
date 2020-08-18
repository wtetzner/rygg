
module Command = Core.Command

let vmu_assemble input_file inc_dir output_file =
  Vmu.Asm.Parser.assemble input_file inc_dir output_file

let vmu_disassemble input_file =
  Vmu.Disasm.disassemble input_file

let vmu_lir_compile input_file =
  let open Vmu.Lir in
  let functions = ref Env.empty in
  functions := Env.add "_get_keys" (Function.AsmFunc [S.instruction (I.Ld_d9 (E.var "p3"))]) !functions;
  let statements = Compiler.compile !functions Env.empty in
  Stream.iter (fun s -> print_endline (S.to_string s)) statements

let vmu_cmd =
  let lir_cmd =
    Command.basic
      ~summary:"Compiler for Dreamcast VMU Low-level Intermediate Language"
      ~readme:(fun () -> "Compiler for Dreamcast VMU Low-level Intermediate Language")
      Command.Let_syntax.(
      let%map_open filename = anon ("input-file" %: string) in
      fun () -> vmu_lir_compile filename
    )
  in
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
      ~summary:"Compiler for the Wombat programming language"
      ~readme:(fun () -> "Assembler for Dreamcast VMU")
      Command.Let_syntax.(
      let%map_open output = flag ~aliases:["-o"] "-output" (required string) ~doc:"Output File"
      and filename = anon ("input-file" %: string)
      in
      fun () -> raise (Failure (Printf.sprintf "compile is not yet implemented; (output = %s, filename = %s)" output filename))
    ) in
  Command.group ~summary:"Operations for Dreamcast VMU"
    [ "assemble", assemble_cmd;
      "disassemble", disassemble_cmd;
      "compile", compile_cmd;
      "lir", lir_cmd ]

let command =
  Command.group ~summary:"Compiler and build tool for Dreamcast VMU"
    [ "vmu", vmu_cmd ]

let () = Command.run command

