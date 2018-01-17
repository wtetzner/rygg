
module Expression = Asm.Expression
module Instruction = Asm.Instruction
module IndirectionMode = Asm.IndirectionMode

let chunk str pos =
  let len = String.length str in
  let chunk_len = min (len - pos) 3 in
  String.sub str pos chunk_len

let disasm_instr bytes pos =
  let module I = Instruction in
  let module E = Expression in
  let module IM = IndirectionMode in
  match%bitstring (Bitstring.bitstring_of_string (chunk bytes pos)) with
  | {| 0b10000001 : 8; i8 : 8 |} -> Some (I.Add_i8 (E.num i8))
  | {| 0b1000001 : 7; d9 : 9 |} -> Some (I.Add_d9 (E.num d9))
  | {| 0b100001 : 6; 0 : 2 |} -> Some (I.Add_Ri IM.R0)
  | {| 0b100001 : 6; 1 : 2 |} -> Some (I.Add_Ri IM.R1)
  | {| 0b100001 : 6; 2 : 2 |} -> Some (I.Add_Ri IM.R2)
  | {| 0b100001 : 6; 3 : 2 |} -> Some (I.Add_Ri IM.R3)

  | {| 0b10010001 : 8; i8 : 8 |} -> Some (I.Addc_i8 (E.num i8))
  | {| 0b1001001 : 7; d9 : 9 |} -> Some (I.Addc_d9 (E.num d9))
  | {| 0b100101 : 6; 0 : 2 |} -> Some (I.Addc_Ri IM.R0)
  | {| 0b100101 : 6; 1 : 2 |} -> Some (I.Addc_Ri IM.R1)
  | {| 0b100101 : 6; 2 : 2 |} -> Some (I.Addc_Ri IM.R2)
  | {| 0b100101 : 6; 3 : 2 |} -> Some (I.Addc_Ri IM.R3)

  | {| 0b10100001 : 8; i8 : 8 |} -> Some (I.Sub_i8 (E.num i8))
  | {| 0b1010001 : 7; d9 : 9 |} -> Some (I.Sub_d9 (E.num d9))
  | {| 0b101001 : 6; 0 : 2 |} -> Some (I.Sub_Ri IM.R0)
  | {| 0b101001 : 6; 1 : 2 |} -> Some (I.Sub_Ri IM.R1)
  | {| 0b101001 : 6; 2 : 2 |} -> Some (I.Sub_Ri IM.R2)
  | {| 0b101001 : 6; 3 : 2 |} -> Some (I.Sub_Ri IM.R3)

  | {| 0b10110001 : 8; i8 : 8 |} -> Some (I.Subc_i8 (E.num i8))
  | {| 0b1011001 : 7; d9 : 9 |} -> Some (I.Subc_d9 (E.num d9))
  | {| 0b101101 : 6; 0 : 2 |} -> Some (I.Subc_Ri IM.R0)
  | {| 0b101101 : 6; 1 : 2 |} -> Some (I.Subc_Ri IM.R1)
  | {| 0b101101 : 6; 2 : 2 |} -> Some (I.Subc_Ri IM.R2)
  | {| 0b101101 : 6; 3 : 2 |} -> Some (I.Subc_Ri IM.R3)

  | {| 0b0110001 : 7; d9 : 9 |} -> Some (I.Inc_d9 (E.num d9))
  | {| 0b011001 : 6; 0 : 2 |} -> Some (I.Inc_Ri IM.R0)
  | {| 0b011001 : 6; 1 : 2 |} -> Some (I.Inc_Ri IM.R1)
  | {| 0b011001 : 6; 2 : 2 |} -> Some (I.Inc_Ri IM.R2)
  | {| 0b011001 : 6; 3 : 2 |} -> Some (I.Inc_Ri IM.R3)

  | {| 0b0111001 : 7; d9 : 9 |} -> Some (I.Dec_d9 (E.num d9))
  | {| 0b011101 : 6; 0 : 2 |} -> Some (I.Dec_Ri IM.R0)
  | {| 0b011101 : 6; 1 : 2 |} -> Some (I.Dec_Ri IM.R1)
  | {| 0b011101 : 6; 2 : 2 |} -> Some (I.Dec_Ri IM.R2)
  | {| 0b011101 : 6; 3 : 2 |} -> Some (I.Dec_Ri IM.R3)

  | {| 0b00110000 : 8 |} -> Some I.Mul
  | {| 0b01000000 : 8 |} -> Some I.Div

  | {| 0b11100001 : 8; i8 : 8 |} -> Some (I.And_i8 (E.num i8))
  | {| 0b1110001 : 7; d9 : 9 |} -> Some (I.And_d9 (E.num d9))
  | {| 0b111001 : 6; 0 : 2 |} -> Some (I.And_Ri IM.R0)
  | {| 0b111001 : 6; 1 : 2 |} -> Some (I.And_Ri IM.R1)
  | {| 0b111001 : 6; 2 : 2 |} -> Some (I.And_Ri IM.R2)
  | {| 0b111001 : 6; 3 : 2 |} -> Some (I.And_Ri IM.R3)

  | {| 0b11010001 : 8; i8 : 8 |} -> Some (I.Or_i8 (E.num i8))
  | {| 0b1101001 : 7; d9 : 9 |} -> Some (I.Or_d9 (E.num d9))
  | {| 0b110101 : 6; 0 : 2 |} -> Some (I.Or_Ri IM.R0)
  | {| 0b110101 : 6; 1 : 2 |} -> Some (I.Or_Ri IM.R1)
  | {| 0b110101 : 6; 2 : 2 |} -> Some (I.Or_Ri IM.R2)
  | {| 0b110101 : 6; 3 : 2 |} -> Some (I.Or_Ri IM.R3)

  | {| 0b11110001 : 8; i8 : 8 |} -> Some (I.Xor_i8 (E.num i8))
  | {| 0b1111001 : 7; d9 : 9 |} -> Some (I.Xor_d9 (E.num d9))
  | {| 0b111101 : 6; 0 : 2 |} -> Some (I.Xor_Ri IM.R0)
  | {| 0b111101 : 6; 1 : 2 |} -> Some (I.Xor_Ri IM.R1)
  | {| 0b111101 : 6; 2 : 2 |} -> Some (I.Xor_Ri IM.R2)
  | {| 0b111101 : 6; 3 : 2 |} -> Some (I.Xor_Ri IM.R3)

  | {| 0b11100000 : 8 |} -> Some I.Rol
  | {| 0b11110000 : 8 |} -> Some I.Rolc

  | {| 0b11000000 : 8 |} -> Some I.Ror
  | {| 0b11010000 : 8 |} -> Some I.Rorc

  | {| 0b0000001 : 7; d9 : 9 |} -> Some (I.Ld_d9 (E.num d9))
  | {| 0b000001 : 6; 0 : 2 |} -> Some (I.Ld_Ri IM.R0)
  | {| 0b000001 : 6; 1 : 2 |} -> Some (I.Ld_Ri IM.R1)
  | {| 0b000001 : 6; 2 : 2 |} -> Some (I.Ld_Ri IM.R2)
  | {| 0b000001 : 6; 3 : 2 |} -> Some (I.Ld_Ri IM.R3)

  | {| 0b0001001 : 7; d9 : 9 |} -> Some (I.St_d9 (E.num d9))
  | {| 0b000101 : 6; 0 : 2 |} -> Some (I.St_Ri IM.R0)
  | {| 0b000101 : 6; 1 : 2 |} -> Some (I.St_Ri IM.R1)
  | {| 0b000101 : 6; 2 : 2 |} -> Some (I.St_Ri IM.R2)
  | {| 0b000101 : 6; 3 : 2 |} -> Some (I.St_Ri IM.R3)

  | {| 0b0010001 : 7; d9 : 9; i8 : 8 |} -> Some (I.Mov_d9 (E.num i8, E.num d9))

  | {| 0b001001 : 6; rj : 2; i8 : 8 |} -> Some (I.Mov_Rj (E.num i8, IM.from_index rj))

  | {| 0b11000001 : 8 |} -> Some I.Ldc
  | {| 0b0110000 : 7; d9 : 9 |} -> Some (I.Push (E.num d9))
  | {| 0b0111000 : 7; d9 : 9 |} -> Some (I.Pop (E.num d9))

  | {| 0b1100001 : 7; d9 : 9 |} -> Some (I.Xch_d9 (E.num d9))
  | {| 0b110001 : 6; 0 : 2 |} -> Some (I.Xch_Ri IM.R0)
  | {| 0b110001 : 6; 1 : 2 |} -> Some (I.Xch_Ri IM.R1)
  | {| 0b110001 : 6; 2 : 2 |} -> Some (I.Xch_Ri IM.R2)
  | {| 0b110001 : 6; 3 : 2 |} -> Some (I.Xch_Ri IM.R3)

  | {| 0b001 : 3; a11 : 1; true : 1; rest : 11 : bitstring |} ->
     (let num_bits = [%bitstring {| a11 : 1; rest : 11 : bitstring |}] in
      let num = (match%bitstring num_bits with
                 | {| value : 12 |} -> value) in
      let next_pos = pos + 3 in
      let top_bits = next_pos land 0b1111000000000000 in
      let value = top_bits lor num in
      Some (I.Jmp (E.num value)))

  | {| 0b00100001 : 8; a16 : 16 |} -> Some (I.Jmpf (E.num a16))

  | {| 0b00000001 : 8; r8 : 8 : signed |} ->
     let next_pos = pos + 2 in
     Some (I.Br (E.num (r8 + next_pos)))
  | {| 0b00010001 : 8; r16 : 16 : littleendian, signed |} ->
     let next_pos = pos + 3 in
     Some (I.Brf (E.num (r16 + next_pos)))     

  | {| 0b10000000 : 8; r8 : 8 : signed |} ->
     let next_pos = pos + 2 in
     Some (I.Bz (E.num (r8 + next_pos)))
  | {| 0b10010000 : 8; r8 : 8 : signed |} ->
     let next_pos = pos + 2 in
     Some (I.Bnz (E.num (r8 + next_pos)))
  | {| 0b011 : 3; d8 : 1; true : 1; b3 : 3; d9rest : 8 : bitstring; r8 : 8 : signed |} ->
     (let d9_bits = [%bitstring {| d8 : 1; d9rest : 8 : bitstring |}] in
      let d9 = (match%bitstring d9_bits with
                | {| value : 9 |} -> value) in
      let next_pos = pos + 3 in
      Some (I.Bp (E.num d9, E.num b3, E.num (r8 + next_pos))))
  | {| 0b010 : 3; d8 : 1; true : 1; b3 : 3; d9rest : 8 : bitstring; r8 : 8 : signed |} ->
     (let d9_bits = [%bitstring {| d8 : 1; d9rest : 8 : bitstring |}] in
      let d9 = (match%bitstring d9_bits with
                | {| value : 9 |} -> value) in
      let next_pos = pos + 3 in
      Some (I.Bpc (E.num d9, E.num b3, E.num (r8 + next_pos))))
  | {| 0b100 : 3; d8 : 1; true : 1; b3 : 3; d9rest : 8 : bitstring; r8 : 8 : signed |} ->
     (let d9_bits = [%bitstring {| d8 : 1; d9rest : 8 : bitstring |}] in
      let d9 = (match%bitstring d9_bits with
                | {| value : 9 |} -> value) in
      let next_pos = pos + 3 in
      Some (I.Bn (E.num d9, E.num b3, E.num (r8 + next_pos))))
  | {| 0b0101001 : 7; d9 : 9; r8 : 8 : signed |} ->
     (let next_pos = pos + 3 in
      Some (I.Dbnz_d9 (E.num d9, E.num (r8 + next_pos))))
  | {| 0b010101 : 6; ri : 2; r8 : 8 : signed |} ->
     (let next_pos = pos + 2 in
      Some (I.Dbnz_Ri (IM.from_index ri, E.num (r8 + next_pos))))
  | {| 0b00110001 : 8; i8 : 8; r8 : 8 : signed |} ->
     (let next_pos = pos + 3 in
      Some (I.Be_i8 (E.num i8, E.num (r8 + next_pos))))
  | {| 0b0011001 : 7; d9 : 9; r8 : 8 : signed |} ->
     (let next_pos = pos + 3 in
      Some (I.Be_d9 (E.num d9, E.num (r8 + next_pos))))
  | {| 0b001101 : 6; rj : 2; i8 : 8; r8 : 8 : signed |} ->
     (let next_pos = pos + 3 in
      Some (I.Be_Rj (IM.from_index rj, E.num i8, E.num (r8 + next_pos))))
  | {| 0b01000001 : 8; i8 : 8; r8 : 8 : signed |} ->
     (let next_pos = pos + 3 in
      Some (I.Bne_i8 (E.num i8, E.num (r8 + next_pos))))
  | {| 0b0100001 : 7; d9 : 9; r8 : 8 : signed |} ->
     (let next_pos = pos + 3 in
      Some (I.Bne_d9 (E.num d9, E.num (r8 + next_pos))))
  | {| 0b010001 : 6; rj : 2; i8 : 8; r8 : 8 : signed |} ->
     (let next_pos = pos + 3 in
      Some (I.Bne_Rj (IM.from_index rj, E.num i8, E.num (r8 + next_pos))))

  | {| 0b000 : 3; a11 : 1; true : 1; rest : 11 : bitstring |} ->
     (let num_bits = [%bitstring {| a11 : 1; rest : 11 : bitstring |}] in
      let num = (match%bitstring num_bits with
                 | {| value : 12 |} -> value) in
      let next_pos = pos + 3 in
      let top_bits = next_pos land 0b1111000000000000 in
      let value = top_bits lor num in
      Some (I.Call (E.num value)))
  | {| 0b00100000 : 8; a16 : 16 : bigendian |} -> Some (I.Callf (E.num a16))
  (* | {| 0b00010000 : 8; r16 : 16 : littleendian |} ->
   *    (let next_pos = pos + 3 in
   *     Some (I.Callr (E.num (r16 + next_pos)))) *)

  | {| _ |} -> None

let print_all bytes =
  let module I = Instruction in
  let len = String.length bytes in
  let pos = ref 0 in
  while !pos < len do
    let instr = disasm_instr bytes !pos in
    match instr with
    | Some ins ->
       (let size = I.size ins in
        Printf.printf "%s ; %08X\n" (I.to_string ins) !pos;
        pos := !pos + size)
    | None ->
       (Printf.printf ".byte $%02X ; %08X\n" (Char.code (String.get bytes !pos)) !pos;
        pos := !pos + 1)
  done

let load_string filename =
  Core.In_channel.with_file
    filename ~f:(fun f -> Core.In_channel.input_all f)

let disassemble filename =
  let bytes = load_string filename in
  print_all bytes;
  ()
