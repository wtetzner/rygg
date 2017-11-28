
module Command = Core.Command

let vmu_assemble input_file output_file =
  let inc_dir = Core.Filename.dirname input_file in
  Vmu.Asm.Parser.assemble input_file inc_dir output_file

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
        vmu_assemble filename output
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

