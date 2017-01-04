
open Vmu_Asm
open Vmu_Asm.Expression

let () =
  print_endline "Hello, World";
  let env: Vmu_Asm.Environment.t = Vmu_Asm.Environment.with_name Vmu_Asm.Environment.empty "name" 12 in
  let expr = (Times (Number 7, (Plus (Name "name", Number 5)))) in
  Printf.printf "%s = %d\n" (to_string expr) (eval expr env)

