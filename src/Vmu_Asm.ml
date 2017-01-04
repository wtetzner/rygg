
module Name = struct
    type t = string
    let matches s1 s2 = String.equal s1 s2
end

module Environment = Env.Make (Name) (struct type t = int end)

module Expression = struct
    type t =
      | Name of string
      | Plus of t * t
      | Minus of t * t
      | Times of t * t
      | Divide of t * t
      | Number of int
      | UpperByte of t
      | LowerByte of t

    (*
     * Check to see if the expression is complex,
     * which just means it should be wrapped in parens
     * when printed
     *)
    let complex expr =
      match expr with
      | Name _ -> false
      | Plus (_,_) | Minus (_,_) | Times (_,_) | Divide (_,_)  -> true
      | Number _ -> false
      | UpperByte _ -> true
      | LowerByte _ -> true

    let rec to_string expr =
      match expr with
      | Name name -> name
      | Plus (e1, e2) -> Printf.sprintf "%s + %s" (wrap e1) (wrap e2)
      | Minus (e1, e2) -> Printf.sprintf "%s - %s" (wrap e1) (wrap e2)
      | Times (e1, e2) -> Printf.sprintf "%s * %s" (wrap e1) (wrap e2)
      | Divide (e1, e2) -> Printf.sprintf "%s / %s" (wrap e1) (wrap e2)
      | Number e -> string_of_int e
      | UpperByte e -> Printf.sprintf ">%s" (wrap e)
      | LowerByte e -> Printf.sprintf "<%s" (wrap e)
    and wrap expr =
      if complex expr then
        Printf.sprintf "(%s)" (to_string expr)
      else
        to_string expr

    exception Not_found of string

    (* Evaluate the expression into an int *)
    let rec eval expr env =
      match expr with
      | Name name -> (match Environment.lookup env name with
                      | Some(result) -> result
                      | None -> raise (Not_found (Printf.sprintf "Name '%s' not found" name)))
      | Plus (e1, e2) -> (eval e1 env) + (eval e2 env)
      | Minus (e1, e2) -> (eval e1 env) - (eval e2 env)
      | Times (e1, e2) -> (eval e1 env) * (eval e2 env)
      | Divide (e1, e2) -> (eval e1 env) / (eval e2 env)
      | Number n -> n
      | UpperByte e -> let n = eval e env in (n lsr 8) land 0xFF
      | LowerByte e -> let n = eval e env in n land 0xFF
end

module IndirectionMode = struct
    type t =
      | R0
      | R1
      | R2
      | R3

    let index im =
      match im with
      | R0 -> 0
      | R1 -> 1
      | R2 -> 2
      | R3 -> 3

    let to_string im =
      match im with
      | R0 -> "@R0"
      | R1 -> "@R1"
      | R2 -> "@R2"
      | R3 -> "@R3"
end

module Instruction = struct
    type t =
      | Add_i8 of Expression.t
      | Add_d9 of Expression.t
      | Add_Ri of IndirectionMode.t

      | Addc_i8 of Expression.t
      | Addc_d9 of Expression.t
      | Addc_Ri of IndirectionMode.t

      | Sub_i8 of Expression.t
      | Sub_d9 of Expression.t
      | Sub_Ri of IndirectionMode.t

      | Subc_i8 of Expression.t
      | Subc_d9 of Expression.t
      | Subc_Ri of IndirectionMode.t

      | Inc_d9 of Expression.t
      | Inc_Ri of IndirectionMode.t

      | Dec_d9 of Expression.t
      | Dec_Ri of IndirectionMode.t

      | Mul
      | Div

      | And_i8 of Expression.t
      | And_d9 of Expression.t
      | And_Ri of IndirectionMode.t

      | Or_i8 of Expression.t
      | Or_d9 of Expression.t
      | Or_Ri of IndirectionMode.t

      | Xor_i8 of Expression.t
      | Xor_d9 of Expression.t
      | Xor_Ri of IndirectionMode.t

      | Rol
      | Rolc

      | Ld_d9 of Expression.t
      | Ld_Ri of IndirectionMode.t

      | St_d9 of Expression.t
      | St_Ri of IndirectionMode.t

      | Mov_d9 of Expression.t * Expression.t
      | Mov_Rj of Expression.t * IndirectionMode.t

      | Ldc

      | Push of Expression.t
      | Pop of Expression.t

      | Xch_d9 of Expression.t
      | Xch_Ri of IndirectionMode.t

      | Jmp of Expression.t
      | Jmpf of Expression.t

      | Br of Expression.t
      | Brf of Expression.t
      | Bz of Expression.t
      | Bnz of Expression.t
      | Bp of Expression.t * Expression.t * Expression.t
      | Bpc of Expression.t * Expression.t * Expression.t
      | Bn of Expression.t * Expression.t * Expression.t
      | Dbnz_d9 of Expression.t * Expression.t
      | Dbnz_Ri of IndirectionMode.t * Expression.t
      | Be_i8 of Expression.t * Expression.t
      | Be_d9 of Expression.t * Expression.t
      | Be_Rj of IndirectionMode.t * Expression.t * Expression.t
      | Bne_i8 of Expression.t * Expression.t
      | Bne_d9 of Expression.t * Expression.t
      | Bne_Rj of IndirectionMode.t * Expression.t * Expression.t

      | Call of Expression.t
      | Callf of Expression.t
      | Callr of Expression.t

      | Ret
      | Reti

      | Clr1 of Expression.t * Expression.t
      | Set1 of Expression.t * Expression.t
      | Not1 of Expression.t * Expression.t

      | Nop

    let encode instr pos env =
      let eval expr = Expression.eval expr env in
      let idx ri = IndirectionMode.index ri in
      match instr with
      | Add_i8 i8 -> BITSTRING { 0b10000001 : 8; eval i8 : 8 }
      | Add_d9 d9 -> BITSTRING { 0b1000001 : 7; eval d9 : 9 }
      | Add_Ri ri -> BITSTRING { 0b100001 : 6; idx ri : 2 }

      | Addc_i8 i8 -> BITSTRING { 0b10010001 : 8; eval i8 : 8 }
      | Addc_d9 d9 -> BITSTRING { 0b1001001 : 7; eval d9 : 9 }
      | Addc_Ri ri -> BITSTRING { 0b100101 : 6; idx ri : 2 }

      (* | Sub_i8 i8 ->  *)
      (* | Sub_d9 d9 ->  *)
      (* | Sub_Ri ri ->  *)

      (* | Subc_i8 i8 ->  *)
      (* | Subc_d9 d0 ->  *)
      (* | Subc_Ri ri ->  *)

      (* | Inc_d9 d9 ->  *)
      (* | Inc_Ri ri ->  *)

      (* | Dec_d9 d9 ->  *)
      (* | Dec_Ri ri ->  *)

      (* | Mul ->  *)
      (* | Div ->  *)

      (* | And_i8 i8 ->  *)
      (* | And_d9 d9 ->  *)
      (* | And_Ri ri ->  *)

      (* | Or_i8 i8 ->  *)
      (* | Or_d9 d9 ->  *)
      (* | Or_Ri ri ->  *)

      (* | Xor_i8 i8 ->  *)
      (* | Xor_d9 d9 ->  *)
      (* | Xor_Ri ri ->  *)

      (* | Rol ->  *)
      (* | Rolc ->  *)

      (* | Ld_d9 d9 ->  *)
      (* | Ld_Ri ri ->  *)

      (* | St_d9 d9 ->  *)
      (* | St_Ri ri ->  *)

      (* | Mov_d9 (i8, d9) ->  *)
      (* | Mov_Rj (i8, rj) ->  *)

      (* | Ldc ->  *)

      (* | Push d9 ->  *)
      (* | Pop d9 ->  *)

      (* | Xch_d9 d9 ->  *)
      (* | Xch_Ri ri ->  *)

      (* | Jmp a12 ->  *)
      (* | Jmpf a16 ->  *)

      (* | Br r8 ->  *)
      (* | Brf r16 ->  *)
      (* | Bz r8 ->  *)
      (* | Bnz r8 ->  *)
      (* | Bp (d9, b3, r8) ->  *)
      (* | Bpc (d9, b3, r8) ->  *)
      (* | Bn (d9, b3, r8) ->  *)
      (* | Dbnz_d9 (d9, r8) ->  *)
      (* | Dbnz_Ri (ri, r8) ->  *)
      (* | Be_i8 (i8, r8) ->  *)
      (* | Be_d9 (d9, r8) ->  *)
      (* | Be_Rj (rj, i8, r8) ->  *)
      (* | Bne_i8 (i8, r8) ->  *)
      (* | Bne_d9 (d9, r8) ->  *)
      (* | Bne_Rj (rj, i8, r8) ->  *)

      (* | Call a12 ->  *)
      (* | Callf a16 ->  *)
      (* | Callr r16 ->  *)

      (* | Ret ->  *)
      (* | Reti ->  *)

      (* | Clr1 (d9, b3) ->  *)
      (* | Set1 (d9, b3) ->  *)
      (* | Not1 (d9, b3) ->  *)

      (* | Nop ->  *)
end

