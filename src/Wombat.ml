
module Command = Core.Command

let vmu_assemble input_file inc_dir output_file =
  Vmu.Asm.Parser.assemble input_file inc_dir output_file

let vmu_disassemble input_file =
  Vmu.Disasm.disassemble input_file

let vmu_cmd =
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
      "compile", compile_cmd ]

let command =
  Command.group ~summary:"Compiler and build tool for Dreamcast VMU"
    [ "vmu", vmu_cmd ]

let () = Command.run command

