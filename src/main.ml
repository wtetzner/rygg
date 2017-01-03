
open Asm

let () =
  print_endline "Hello, World";
  let env: Asm.Environment.t = Asm.Environment.with_name Asm.Environment.empty "name" 12 in
  let expr = (Times (Number 7, (Plus (Name "name", Number 5)))) in
  Printf.printf "%s = %d\n" (to_string expr) (eval expr env)

