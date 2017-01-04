
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

      | Ror
      | Rorc

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

    exception Invalid_a12 of string

    let encode instr pos env =
      let eval expr = Expression.eval expr env in
      let idx ri = IndirectionMode.index ri in
      let rel expr = (eval expr) - pos in
      match instr with
      | Add_i8 i8 -> BITSTRING { 0b10000001 : 8; eval i8 : 8 }
      | Add_d9 d9 -> BITSTRING { 0b1000001 : 7; eval d9 : 9 }
      | Add_Ri ri -> BITSTRING { 0b100001 : 6; idx ri : 2 }

      | Addc_i8 i8 -> BITSTRING { 0b10010001 : 8; eval i8 : 8 }
      | Addc_d9 d9 -> BITSTRING { 0b1001001 : 7; eval d9 : 9 }
      | Addc_Ri ri -> BITSTRING { 0b100101 : 6; idx ri : 2 }

      | Sub_i8 i8 -> BITSTRING { 0b10100001 : 8; eval i8 : 8 }
      | Sub_d9 d9 -> BITSTRING { 0b1010001 : 7; eval d9 : 9 }
      | Sub_Ri ri -> BITSTRING { 0b101001 : 6; idx ri : 2 }

      | Subc_i8 i8 -> BITSTRING { 0b10110001 : 8; eval i8 : 8 }
      | Subc_d9 d9 -> BITSTRING { 0b1011001 : 7; eval d9 : 9 }
      | Subc_Ri ri -> BITSTRING { 0b101101 : 6; idx ri : 2 }

      | Inc_d9 d9 -> BITSTRING { 0b0110001 : 7; eval d9 : 9 }
      | Inc_Ri ri -> BITSTRING { 0b011001 : 6; idx ri : 2 }

      | Dec_d9 d9 -> BITSTRING { 0b0111001 : 7; eval d9 : 9 }
      | Dec_Ri ri -> BITSTRING { 0b011101 : 6; idx ri : 2 }

      | Mul -> BITSTRING { 0b00110000 : 8 }
      | Div -> BITSTRING { 0b01000000 : 8 }

      | And_i8 i8 -> BITSTRING { 0b11100001 : 8; eval i8 : 8 }
      | And_d9 d9 -> BITSTRING { 0b1110001 : 7; eval d9 : 9 }
      | And_Ri ri -> BITSTRING { 0b111001 : 6; idx ri : 2 }

      | Or_i8 i8 -> BITSTRING { 0b11010001 : 8; eval i8 : 8 }
      | Or_d9 d9 -> BITSTRING { 0b1101001 : 7; eval d9 : 9 }
      | Or_Ri ri -> BITSTRING { 0b110101 : 6; idx ri : 2 }

      | Xor_i8 i8 -> BITSTRING { 0b11110001 : 8; eval i8 : 8 }
      | Xor_d9 d9 -> BITSTRING { 0b1111001 : 7; eval d9 : 9 }
      | Xor_Ri ri -> BITSTRING { 0b111101 : 6; idx ri : 2 }

      | Rol -> BITSTRING { 0b11100000 : 8 }
      | Rolc -> BITSTRING { 0b11110000 : 8 }

      | Ror -> BITSTRING { 0b11000000 : 8 }
      | Rorc -> BITSTRING { 0b11010000 : 8 }

      | Ld_d9 d9 -> BITSTRING { 0b0000001 : 7; eval d9 : 9 }
      | Ld_Ri ri -> BITSTRING { 0b000001 : 6; idx ri : 2 }

      | St_d9 d9 -> BITSTRING { 0b0001001 : 7; eval d9 : 9 }
      | St_Ri ri -> BITSTRING { 0b000101 : 6; idx ri : 2 }

      | Mov_d9 (i8, d9) -> BITSTRING {
                               0b0010001 : 7;
                               eval d9 : 9;
                               eval i8 : 8
                           }
      | Mov_Rj (i8, rj) -> BITSTRING {
                               0b001001 : 6;
                               idx rj : 2;
                               eval i8 : 8
                           }

      | Ldc -> BITSTRING { 0b11000001 : 8 }

      | Push d9 -> BITSTRING { 0b0110000 : 7; eval d9 : 9 }
      | Pop d9 -> BITSTRING { 0b0111000 : 7; eval d9 : 9 }

      | Xch_d9 d9 -> BITSTRING { 0b1100001 : 7; eval d9 : 9 }
      | Xch_Ri ri -> BITSTRING { 0b110001 : 6; idx ri : 2 }

      | Jmp a12 -> (
         let value: int = eval a12 in
         let value_top_bits = value land 0b1111000000000000 in
         let pos_top_bits = pos land 0b1111000000000000 in
         if value_top_bits != pos_top_bits then
           raise (Invalid_a12 (Printf.sprintf "Invalid a12 value %d for pos %d; top 4 bits don't match" value pos))
         else
           (bitmatch (BITSTRING { value : 12}) with
            | { a11 : 1; rest : 11 : bitstring } ->
               BITSTRING {
                   0b001 : 3;
                   a11 : 1;
                   true : 1;
                   rest : 11 : bitstring
               }))
      | Jmpf a16 -> BITSTRING { eval a16 : 16 }

      | Br r8 -> BITSTRING { 0b00000001 : 8; rel r8 : 8 }
      | Brf r16 -> BITSTRING { 0b00010001 : 8; rel r16 : 16 : littleendian }
      | Bz r8 -> BITSTRING { 0b10000000 : 8; rel r8 : 8 }
      | Bnz r8 -> BITSTRING { 0b10010000 : 8; rel r8 : 8 }
      | Bp (d9, b3, r8) ->
         (bitmatch (BITSTRING { eval d9 : 9}) with
          | { d8 : 1; d9rest : 8 : bitstring } ->
             BITSTRING {
                 0b011 : 3;
                 d8 : 1;
                 eval b3 : 3;
                 d9rest : 8 : bitstring;
                 rel r8 : 8
               })
      | Bpc (d9, b3, r8) ->
         (bitmatch (BITSTRING { eval d9 : 9}) with
          | { d8 : 1; d9rest : 8 : bitstring } ->
             BITSTRING {
                 0b010 : 3;
                 d8 : 1;
                 eval b3 : 3;
                 d9rest : 8 : bitstring;
                 rel r8 : 8
               })
      | Bn (d9, b3, r8) ->
         (bitmatch (BITSTRING { eval d9 : 9}) with
          | { d8 : 1; d9rest : 8 : bitstring } ->
             BITSTRING {
                 0b100 : 3;
                 d8 : 1;
                 eval b3 : 3;
                 d9rest : 8 : bitstring;
                 rel r8 : 8
               })
      | Dbnz_d9 (d9, r8) -> BITSTRING {
                                0b0101001 : 7;
                                eval d9 : 9;
                                rel r8 : 8
                            }
      | Dbnz_Ri (ri, r8) -> BITSTRING {
                                0b010101 : 6;
                                idx ri : 2;
                                rel r8 : 8
                            }
      | Be_i8 (i8, r8) -> BITSTRING {
                              0b00110001 : 8;
                              eval i8 : 8;
                              rel r8 : 8
                          }
      | Be_d9 (d9, r8) -> BITSTRING {
                              0b0011001 : 7;
                              eval d9 : 9;
                              rel r8 : 8
                          }
      | Be_Rj (rj, i8, r8) -> BITSTRING {
                                  0b001101 : 6;
                                  idx rj : 2;
                                  eval i8 : 8;
                                  rel r8 : 8
                              }
      | Bne_i8 (i8, r8) -> BITSTRING {
                               0b01000001 : 8;
                               eval i8 : 8;
                               rel r8 : 8
                           }
      | Bne_d9 (d9, r8) -> BITSTRING {
                               0b0100001 : 7;
                               eval d9 : 9;
                               rel r8 : 8
                           }
      | Bne_Rj (rj, i8, r8) -> BITSTRING {
                                   0b010001 : 6;
                                   idx rj : 2;
                                   eval i8 : 8;
                                   rel r8 : 8
                               }

      | Call a12 -> (
        let value: int = eval a12 in
        let value_top_bits = value land 0b1111000000000000 in
        let pos_top_bits = pos land 0b1111000000000000 in
        if value_top_bits != pos_top_bits then
          raise (Invalid_a12 (Printf.sprintf "Invalid a12 value %d for pos %d; top 4 bits don't match" value pos))
        else
          (bitmatch (BITSTRING { value : 12}) with
           | { a11 : 1; rest : 11 : bitstring } ->
              BITSTRING {
                  0b000 : 3;
                  a11 : 1;
                  true : 1;
                  rest : 11 : bitstring
                }))
      | Callf a16 -> BITSTRING {
                         0b00100000 : 8;
                         eval a16 : 16 : bigendian
                     }
      | Callr r16 -> BITSTRING {
                         0b00010000 : 8;
                         rel r16 : 16 : littleendian
                     }

      | Ret -> BITSTRING { 0b10100000 : 8 }
      | Reti -> BITSTRING { 0b10110000 : 8 }

      | Clr1 (d9, b3) ->
         (bitmatch (BITSTRING { eval d9 : 9}) with
          | { d8 : 1; d9rest : 8 : bitstring } ->
             BITSTRING {
                 0b110 : 3;
                 d8 : 1;
                 true : 1;
                 eval b3 : 3;
                 d9rest : 8 : bitstring
               })
      | Set1 (d9, b3) ->
         (bitmatch (BITSTRING { eval d9 : 9}) with
          | { d8 : 1; d9rest : 8 : bitstring } ->
             BITSTRING {
                 0b111 : 3;
                 d8 : 1;
                 true : 1;
                 eval b3 : 3;
                 d9rest : 8 : bitstring
               })
      | Not1 (d9, b3) ->
         (bitmatch (BITSTRING { eval d9 : 9}) with
          | { d8 : 1; d9rest : 8 : bitstring } ->
             BITSTRING {
                 0b101 : 3;
                 d8 : 1;
                 true : 1;
                 eval b3 : 3;
                 d9rest : 8 : bitstring
               })

      | Nop -> BITSTRING { 0b00000000 : 8 }

    let size instr =
      match instr with
      | Add_i8 i8 -> 2
      | Add_d9 d9 -> 2
      | Add_Ri ri -> 1

      | Addc_i8 i8 -> 2
      | Addc_d9 d9 -> 2
      | Addc_Ri ri -> 1

      | Sub_i8 i8 -> 2
      | Sub_d9 d9 -> 2
      | Sub_Ri ri -> 1

      | Subc_i8 i8 -> 2
      | Subc_d9 d9 -> 2
      | Subc_Ri ri -> 1

      | Inc_d9 d9 -> 2
      | Inc_Ri ri -> 1

      | Dec_d9 d9 -> 2
      | Dec_Ri ri -> 1

      | Mul -> 1
      | Div -> 1

      | And_i8 i8 -> 2
      | And_d9 d9 -> 2
      | And_Ri ri -> 1

      | Or_i8 i8 -> 2
      | Or_d9 d9 -> 2
      | Or_Ri ri -> 1

      | Xor_i8 i8 -> 2
      | Xor_d9 d9 -> 2
      | Xor_Ri ri -> 1

      | Rol -> 1
      | Rolc -> 1

      | Ror -> 1
      | Rorc -> 1

      | Ld_d9 d9 -> 2
      | Ld_Ri ri -> 1

      | St_d9 d9 -> 2
      | St_Ri ri -> 1

      | Mov_d9 (i8, d9) -> 3
      | Mov_Rj (i8, rj) -> 2

      | Ldc -> 1

      | Push d9 -> 2
      | Pop d9 -> 2

      | Xch_d9 d9 -> 2
      | Xch_Ri ri -> 1

      | Jmp a12 -> 2
      | Jmpf a16 -> 3

      | Br r8 -> 2
      | Brf r16 -> 3
      | Bz r8 -> 2
      | Bnz r8 -> 2
      | Bp (d9, b3, r8) -> 3
      | Bpc (d9, b3, r8) -> 3
      | Bn (d9, b3, r8) -> 3
      | Dbnz_d9 (d9, r8) -> 3
      | Dbnz_Ri (ri, r8) -> 2
      | Be_i8 (i8, r8) -> 3
      | Be_d9 (d9, r8) -> 3
      | Be_Rj (rj, i8, r8) -> 3
      | Bne_i8 (i8, r8) -> 3
      | Bne_d9 (d9, r8) -> 3
      | Bne_Rj (rj, i8, r8) -> 3

      | Call a12 -> 2
      | Callf a16 -> 3
      | Callr r16 -> 3

      | Ret -> 1
      | Reti -> 1

      | Clr1 (d9, b3) -> 2
      | Set1 (d9, b3) -> 2
      | Not1 (d9, b3) -> 2

      | Nop -> 1
end

