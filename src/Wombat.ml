
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
      (fun filename () ->
        vmu_lir_compile filename
      )
      Command.Spec.(
      empty
      +> anon ("input-file" %: file)
    )
  in
  let disassemble_cmd =
    Command.basic
      ~summary:"Disassembler for Dreamcast VMU"
      Command.Spec.(
      empty
      +> anon ("input-file" %: file)
    )
      (fun filename () ->
        vmu_disassemble filename
      ) in  
  let assemble_cmd =
    Command.basic
      ~summary:"Assembler for Dreamcast VMU"
      Command.Spec.(
      empty
      +> flag ~aliases:["-o"] "-output" (required string) ~doc:"Output File"
      +> flag ~aliases:["-i"] "-include" (optional string) ~doc:"Includes Directory"
      +> anon ("input-file" %: file)
    )
      (fun output inc_dir filename () ->
        vmu_assemble filename inc_dir output
      ) in
  let compile_cmd =
    Command.basic
      ~summary:"Compiler for the Wombat programming language"
      Command.Spec.(
      empty
      +> flag ~aliases:["-o"] "-output" (required string) ~doc:"Output File"
      +> anon ("input-file" %: file)
    )
      (fun output filename () ->
        raise (Failure "compile is not yet implemented")
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

