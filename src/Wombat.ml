
open Vmu_Asm
open Vmu_Asm.Expression
module Vmu_Asm_Parser = Vmu_Asm_Parser
module Out_channel = Core.Out_channel

let write_bytes_to_file file bytes =
  Out_channel.with_file file ~f:(fun f -> Out_channel.output_string f bytes);
  ()

let () =
  (* let env: Vmu_Asm.Environment.t = Vmu_Asm.Environment.with_name Vmu_Asm.Environment.empty "name" 12 in *)
  (* let expr = (Times (Number 7, (Plus (Name "name", Number 5)))) in
   * Printf.printf "%s = %d\n" (to_string expr) (eval expr env); *)
  let module S = Vmu_Asm.Statement in
  let module D = Vmu_Asm.Directive in
  let module I = Vmu_Asm.Instruction in
  let module E = Vmu_Asm.Expression in
  let statements = [
      S.Comment "Special Function Register addresses";
      S.Alias ("ACC", E.num 0x100);
      S.Alias ("PSW", E.num 0x101);
      S.Alias ("B", E.num 0x102);
      S.Alias ("C", E.num 0x103);
      S.Alias ("TRL", E.num 0x104);
      S.Alias ("TRH", E.num 0x105);
      S.Alias ("SP", E.num 0x106);
      S.Alias ("PCON", E.num 0x107);
      S.Alias ("IE", E.num 0x108);
      S.Alias ("IP", E.num 0x109);
      S.Alias ("EXT", E.num 0x10D);
      S.Alias ("OCR", E.num 0x10E);
      S.Alias ("T0CON", E.num 0x110);
      S.Alias ("T0PRR", E.num 0x111);
      S.Alias ("T0L", E.num 0x112);
      S.Alias ("T0LR", E.num 0x113);
      S.Alias ("T0H", E.num 0x114);
      S.Alias ("T0HR", E.num 0x115);
      S.Alias ("T1CNT", E.num 0x118);
      S.Alias ("T1LC", E.num 0x11A);
      S.Alias ("T1L", E.num 0x11B);
      S.Alias ("T1LR", E.num 0x11B);
      S.Alias ("T1HC", E.num 0x11C);
      S.Alias ("T1H", E.num 0x11D);
      S.Alias ("T1HR", E.num 0x11D);
      S.Alias ("MCR", E.num 0x120);
      S.Alias ("STAD", E.num 0x122);
      S.Alias ("CNR", E.num 0x123);
      S.Alias ("TDR", E.num 0x124);
      S.Alias ("XBNK", E.num 0x125);
      S.Alias ("VCCR", E.num 0x127);
      S.Alias ("SCON0", E.num 0x130);
      S.Alias ("SBUF0", E.num 0x131);
      S.Alias ("SBR", E.num 0x132);
      S.Alias ("SCON1", E.num 0x134);
      S.Alias ("SBUF1", E.num 0x135);
      S.Alias ("P1", E.num 0x144);
      S.Alias ("P1DDR", E.num 0x145);
      S.Alias ("P1FCR", E.num 0x146);
      S.Alias ("P3", E.num 0x14C);
      S.Alias ("P3DDR", E.num 0x14D);
      S.Alias ("P3INT", E.num 0x14E);
      S.Alias ("P7", E.num 0x15C);
      S.Alias ("I01CR", E.num 0x15D);
      S.Alias ("I23CR", E.num 0x15E);
      S.Alias ("ISL", E.num 0x15F);
      S.Alias ("VSEL", E.num 0x163);
      S.Alias ("VRMAD1", E.num 0x164);
      S.Alias ("VRMAD2", E.num 0x165);
      S.Alias ("VTRBF", E.num 0x166);
      S.Alias ("VLREG", E.num 0x167);
      S.Alias ("BTCR", E.num 0x17F);
      S.Alias ("XRAM", E.num 0x180);

      S.Comment "\nPSW bits";
      S.Alias ("CY", E.num 7);
      S.Alias ("AC", E.num 6);
      S.Alias ("IRBK1", E.num 4);
      S.Alias ("IRBK0", E.num 3);
      S.Alias ("OV", E.num 2);
      S.Alias ("RAMBK0", E.num 1);
      S.Alias ("P", E.num 0);

      S.Comment "\nGame variables";
      S.Variable ("piece_x", E.num 0x30);
      S.Variable ("piece_y", E.num 0x31);
      S.Variable ("piece_n", E.num 0x32);
      S.Variable ("piece_r", E.num 0x33);
      S.Variable ("piece_i0", E.num 0x34);
      S.Variable ("piece_i1", E.num 0x35);
      S.Variable ("gotkeys", E.num 0x36);
      S.Variable ("time", E.num 0x37);
      S.Variable ("speed", E.num 0x38);
      S.Variable ("scorelo", E.num 0x39);
      S.Variable ("scorehi", E.num 0x3a);
      S.Variable ("keyinhib", E.num 0x3b);
      S.Variable ("seed", E.num 0x3c);

      S.Variable ("hitmap", E.num 0x3e);

      S.Comment "\nReset and interrupt vectors";
      S.Directive (D.Org 0);
      S.Instruction (I.Jmpf (E.var "start"));

      S.Directive (D.Org 0x3);
      S.Instruction (I.Jmp (E.var "nop_irq"));

      S.Directive (D.Org 0xb);
      S.Instruction (I.Jmp (E.var "nop_irq"));

      S.Directive (D.Org 0x13);
      S.Instruction (I.Jmp (E.var "nop_irq"));

      S.Directive (D.Org 0x1b);
      S.Instruction (I.Jmp (E.var "t1int"));

      S.Directive (D.Org 0x23);
      S.Instruction (I.Jmp (E.var "nop_irq"));

      S.Directive (D.Org 0x2b);
      S.Instruction (I.Jmp (E.var "nop_irq"));

      S.Directive (D.Org 0x33);
      S.Instruction (I.Jmp (E.var "nop_irq"));

      S.Directive (D.Org 0x3b);
      S.Instruction (I.Jmp (E.var "nop_irq"));

      S.Directive (D.Org 0x43);
      S.Instruction (I.Jmp (E.var "nop_irq"));

      S.Directive (D.Org 0x4b);
      S.Instruction (I.Jmp (E.var "nop_irq"));

      S.Instruction (I.Clr1 (E.var "p3int", E.num 0));
      S.Instruction (I.Clr1 (E.var "p3int", E.num 1));

      S.Label "nop_irq";
      S.Instruction I.Reti;

      S.Directive (D.Org 0x130);
      S.Label "t1int";
      S.Instruction (I.Push (E.var "ie"));
      S.Instruction (I.Clr1 (E.var "ie", E.num 7));
      S.Instruction (I.Not1 (E.var "ext", E.num 0));
      S.Instruction (I.Jmpf (E.var "t1int"));
      S.Instruction (I.Pop (E.var "ie"));
      S.Instruction I.Reti;

      S.Directive (D.Org 0x1f0);
      S.Label "goodbye";
      S.Instruction (I.Not1 (E.var "ext", E.num 0));
      S.Instruction (I.Jmpf (E.var "goodbye"));

      S.Comment "\nHeader";
      S.Directive (D.Org 0x200);
      S.Directive (D.ByteString (Bitstring.bitstring_of_string "Tiny Tetris     "));
      S.Directive (D.ByteString (Bitstring.bitstring_of_string "Mini VMU Tetris by marcus       "));

      S.Comment "\nIconHeader";
      S.Directive (D.Org 0x240);
      S.Comment "Two Frames";
      S.Directive (D.Word [E.num 2; E.num 10]);

      S.Comment "\nIcon palette";
      S.Directive (D.Org 0x260);

      S.Directive (D.Word [E.num 0x0000; E.num 0xFCFC;
                           E.num 0xF0A0; E.num 0xF0F0;
                           E.num 0xFCCF; E.num 0xF00A;
                           E.num 0xF00F; E.num 0xFFFF]);
      S.Directive (D.Word [E.num 0xFFFF; E.num 0xFFFF;
                           E.num 0xFFFF; E.num 0xFFFF;
                           E.num 0xFFFF; E.num 0xFFFF;
                           E.num 0xFFFF; E.num 0xFFFF]);

      S.Comment "\nIcon palette";
      S.Directive (D.Org 0x260);

      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x11; E.num 0x11;
                           E.num 0x11; E.num 0x11;
                           E.num 0x11; E.num 0x10;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x22; E.num 0x22;
                           E.num 0x31; E.num 0x22;
                           E.num 0x22; E.num 0x30;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x22; E.num 0x22;
                           E.num 0x31; E.num 0x22;
                           E.num 0x22; E.num 0x30;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x22; E.num 0x22;
                           E.num 0x31; E.num 0x22;
                           E.num 0x22; E.num 0x30;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x22; E.num 0x22;
                           E.num 0x31; E.num 0x22;
                           E.num 0x22; E.num 0x30;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x33; E.num 0x33;
                           E.num 0x31; E.num 0x33;
                           E.num 0x33; E.num 0x30;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x01; E.num 0x11;
                           E.num 0x11; E.num 0x11;
                           E.num 0x11; E.num 0x11;
                           E.num 0x10; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x01; E.num 0x22;
                           E.num 0x22; E.num 0x31;
                           E.num 0x22; E.num 0x22;
                           E.num 0x30; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x01; E.num 0x22;
                           E.num 0x22; E.num 0x31;
                           E.num 0x22; E.num 0x22;
                           E.num 0x30; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x01; E.num 0x22;
                           E.num 0x22; E.num 0x31;
                           E.num 0x22; E.num 0x22;
                           E.num 0x30; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x01; E.num 0x22;
                           E.num 0x22; E.num 0x31;
                           E.num 0x22; E.num 0x22;
                           E.num 0x30; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x01; E.num 0x33;
                           E.num 0x33; E.num 0x31;
                           E.num 0x33; E.num 0x33;
                           E.num 0x30; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x44; E.num 0x44;
                           E.num 0x40; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x66; E.num 0x66;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x44; E.num 0x44;
                           E.num 0x44; E.num 0x44;
                           E.num 0x44; E.num 0x44;
                           E.num 0x44; E.num 0x44;
                           E.num 0x40; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x64; E.num 0x55;
                           E.num 0x55; E.num 0x64;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x64; E.num 0x55;
                           E.num 0x55; E.num 0x64;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x64; E.num 0x55;
                           E.num 0x55; E.num 0x64;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x64; E.num 0x55;
                           E.num 0x55; E.num 0x64;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x66; E.num 0x66;
                           E.num 0x64; E.num 0x66;
                           E.num 0x66; E.num 0x64;
                           E.num 0x66; E.num 0x66;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);

      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x11; E.num 0x11;
                           E.num 0x11; E.num 0x11;
                           E.num 0x11; E.num 0x10;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x22; E.num 0x22;
                           E.num 0x31; E.num 0x22;
                           E.num 0x22; E.num 0x30;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x22; E.num 0x22;
                           E.num 0x31; E.num 0x22;
                           E.num 0x22; E.num 0x30;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x22; E.num 0x22;
                           E.num 0x31; E.num 0x22;
                           E.num 0x22; E.num 0x30;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x22; E.num 0x22;
                           E.num 0x31; E.num 0x22;
                           E.num 0x22; E.num 0x30;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x01;
                           E.num 0x33; E.num 0x33;
                           E.num 0x31; E.num 0x33;
                           E.num 0x33; E.num 0x30;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x44; E.num 0x44;
                           E.num 0x41; E.num 0x11;
                           E.num 0x11; E.num 0x11;
                           E.num 0x11; E.num 0x11;
                           E.num 0x10; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x61; E.num 0x22;
                           E.num 0x22; E.num 0x31;
                           E.num 0x22; E.num 0x22;
                           E.num 0x30; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x61; E.num 0x22;
                           E.num 0x22; E.num 0x31;
                           E.num 0x22; E.num 0x22;
                           E.num 0x30; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x61; E.num 0x22;
                           E.num 0x22; E.num 0x31;
                           E.num 0x22; E.num 0x22;
                           E.num 0x30; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x61; E.num 0x22;
                           E.num 0x22; E.num 0x31;
                           E.num 0x22; E.num 0x22;
                           E.num 0x30; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x66; E.num 0x66;
                           E.num 0x61; E.num 0x33;
                           E.num 0x33; E.num 0x31;
                           E.num 0x33; E.num 0x33;
                           E.num 0x30; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x44; E.num 0x44;
                           E.num 0x44; E.num 0x44;
                           E.num 0x44; E.num 0x44;
                           E.num 0x44; E.num 0x44;
                           E.num 0x40; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x64; E.num 0x55;
                           E.num 0x55; E.num 0x64;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x64; E.num 0x55;
                           E.num 0x55; E.num 0x64;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x64; E.num 0x55;
                           E.num 0x55; E.num 0x64;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x55; E.num 0x55;
                           E.num 0x64; E.num 0x55;
                           E.num 0x55; E.num 0x64;
                           E.num 0x55; E.num 0x55;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x04;
                           E.num 0x66; E.num 0x66;
                           E.num 0x64; E.num 0x66;
                           E.num 0x66; E.num 0x64;
                           E.num 0x66; E.num 0x66;
                           E.num 0x60; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);
      S.Directive (D.Byte [E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00;
                           E.num 0x00; E.num 0x00]);

      S.Comment "\nMain program";
      S.Directive (D.Org 0x680);
      S.Label "start";
      S.Instruction (I.Clr1 (E.var "ie", E.num 7));
      S.Instruction (I.Mov_d9 (E.num 0xA1, E.var "ocr"));
      S.Instruction (I.Mov_d9 (E.num 0x09, E.var "mcr"));
      S.Instruction (I.Mov_d9 (E.num 0x80, E.var "vccr"));
      S.Instruction (I.Clr1 (E.var "p3int", E.num 0));
      S.Instruction (I.Clr1 (E.var "p1", E.num 7));
      S.Instruction (I.Mov_d9 (E.num 0xFF, E.var "p3"));

      S.Instruction (I.Clr1 (E.var "psw", E.num 1));
      S.Instruction (I.Ld_d9 (E.num 0x1C));
      S.Instruction (I.Mov_d9 (E.num 0xFF, { pos = Vmu_Asm.Position.Span (Span.make Location.empty Location.empty); expr = E.Name "BooBob" }));
    ] in
  List.iter (fun s -> print_endline (Statement.to_string s)) statements;
  try
    let bytes = Vmu_Asm.assemble statements in
    ()
  with
  | Vmu_Asm.Asm_failure (_,_) as e -> print_endline ("[Error] " ^ (Printexc.to_string e))
                                               (* write_bytes_to_file "/Users/walter/temp/test-output.vms" bytes *)


