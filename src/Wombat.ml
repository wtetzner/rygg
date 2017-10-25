
open Vmu_Asm
open Vmu_Asm.Expression
module Out_channel = Core.Std.Out_channel

let write_bytes_to_file file bytes =
  Out_channel.with_file file ~f:(fun f -> Out_channel.output_string f bytes);
  ()

let () =
  let env: Vmu_Asm.Environment.t = Vmu_Asm.Environment.with_name Vmu_Asm.Environment.empty "name" 12 in
  let expr = (Times (Number 7, (Plus (Name "name", Number 5)))) in
  Printf.printf "%s = %d\n" (to_string expr) (eval expr env);
  let module S = Vmu_Asm.Statement in
  let module D = Vmu_Asm.Directive in
  let module I = Vmu_Asm.Instruction in
  let module E = Vmu_Asm.Expression in
  let statements = [
      S.Comment "Special Function Register addresses";
      S.Alias ("ACC", E.Number 0x100);
      S.Alias ("PSW", E.Number 0x101);
      S.Alias ("B", E.Number 0x102);
      S.Alias ("C", E.Number 0x103);
      S.Alias ("TRL", E.Number 0x104);
      S.Alias ("TRH", E.Number 0x105);
      S.Alias ("SP", E.Number 0x106);
      S.Alias ("PCON", E.Number 0x107);
      S.Alias ("IE", E.Number 0x108);
      S.Alias ("IP", E.Number 0x109);
      S.Alias ("EXT", E.Number 0x10D);
      S.Alias ("OCR", E.Number 0x10E);
      S.Alias ("T0CON", E.Number 0x110);
      S.Alias ("T0PRR", E.Number 0x111);
      S.Alias ("T0L", E.Number 0x112);
      S.Alias ("T0LR", E.Number 0x113);
      S.Alias ("T0H", E.Number 0x114);
      S.Alias ("T0HR", E.Number 0x115);
      S.Alias ("T1CNT", E.Number 0x118);
      S.Alias ("T1LC", E.Number 0x11A);
      S.Alias ("T1L", E.Number 0x11B);
      S.Alias ("T1LR", E.Number 0x11B);
      S.Alias ("T1HC", E.Number 0x11C);
      S.Alias ("T1H", E.Number 0x11D);
      S.Alias ("T1HR", E.Number 0x11D);
      S.Alias ("MCR", E.Number 0x120);
      S.Alias ("STAD", E.Number 0x122);
      S.Alias ("CNR", E.Number 0x123);
      S.Alias ("TDR", E.Number 0x124);
      S.Alias ("XBNK", E.Number 0x125);
      S.Alias ("VCCR", E.Number 0x127);
      S.Alias ("SCON0", E.Number 0x130);
      S.Alias ("SBUF0", E.Number 0x131);
      S.Alias ("SBR", E.Number 0x132);
      S.Alias ("SCON1", E.Number 0x134);
      S.Alias ("SBUF1", E.Number 0x135);
      S.Alias ("P1", E.Number 0x144);
      S.Alias ("P1DDR", E.Number 0x145);
      S.Alias ("P1FCR", E.Number 0x146);
      S.Alias ("P3", E.Number 0x14C);
      S.Alias ("P3DDR", E.Number 0x14D);
      S.Alias ("P3INT", E.Number 0x14E);
      S.Alias ("P7", E.Number 0x15C);
      S.Alias ("I01CR", E.Number 0x15D);
      S.Alias ("I23CR", E.Number 0x15E);
      S.Alias ("ISL", E.Number 0x15F);
      S.Alias ("VSEL", E.Number 0x163);
      S.Alias ("VRMAD1", E.Number 0x164);
      S.Alias ("VRMAD2", E.Number 0x165);
      S.Alias ("VTRBF", E.Number 0x166);
      S.Alias ("VLREG", E.Number 0x167);
      S.Alias ("BTCR", E.Number 0x17F);
      S.Alias ("XRAM", E.Number 0x180);

      S.Comment "\nPSW bits";
      S.Alias ("CY", E.Number 7);
      S.Alias ("AC", E.Number 6);
      S.Alias ("IRBK1", E.Number 4);
      S.Alias ("IRBK0", E.Number 3);
      S.Alias ("OV", E.Number 2);
      S.Alias ("RAMBK0", E.Number 1);
      S.Alias ("P", E.Number 0);

      S.Comment "\nGame variables";
      S.Variable ("piece_x", E.Number 0x30);
      S.Variable ("piece_y", E.Number 0x31);
      S.Variable ("piece_n", E.Number 0x32);
      S.Variable ("piece_r", E.Number 0x33);
      S.Variable ("piece_i0", E.Number 0x34);
      S.Variable ("piece_i1", E.Number 0x35);
      S.Variable ("gotkeys", E.Number 0x36);
      S.Variable ("time", E.Number 0x37);
      S.Variable ("speed", E.Number 0x38);
      S.Variable ("scorelo", E.Number 0x39);
      S.Variable ("scorehi", E.Number 0x3a);
      S.Variable ("keyinhib", E.Number 0x3b);
      S.Variable ("seed", E.Number 0x3c);

      S.Variable ("hitmap", E.Number 0x3e);

      S.Comment "\nReset and interrupt vectors";
      S.Directive (D.Org 0);
      S.Instruction (I.Jmpf (E.Name "start"));

      S.Directive (D.Org 0x3);
      S.Instruction (I.Jmp (E.Name "nop_irq"));

      S.Directive (D.Org 0xb);
      S.Instruction (I.Jmp (E.Name "nop_irq"));

      S.Directive (D.Org 0x13);
      S.Instruction (I.Jmp (E.Name "nop_irq"));

      S.Directive (D.Org 0x1b);
      S.Instruction (I.Jmp (E.Name "t1int"));

      S.Directive (D.Org 0x23);
      S.Instruction (I.Jmp (E.Name "nop_irq"));

      S.Directive (D.Org 0x2b);
      S.Instruction (I.Jmp (E.Name "nop_irq"));

      S.Directive (D.Org 0x33);
      S.Instruction (I.Jmp (E.Name "nop_irq"));

      S.Directive (D.Org 0x3b);
      S.Instruction (I.Jmp (E.Name "nop_irq"));

      S.Directive (D.Org 0x43);
      S.Instruction (I.Jmp (E.Name "nop_irq"));

      S.Directive (D.Org 0x4b);
      S.Instruction (I.Jmp (E.Name "nop_irq"));

      S.Instruction (I.Clr1 (E.Name "p3int", E.Number 0));
      S.Instruction (I.Clr1 (E.Name "p3int", E.Number 1));

      S.Label "nop_irq";
      S.Instruction I.Reti;

      S.Directive (D.Org 0x130);
      S.Label "t1int";
      S.Instruction (I.Push (E.Name "ie"));
      S.Instruction (I.Clr1 (E.Name "ie", E.Number 7));
      S.Instruction (I.Not1 (E.Name "ext", E.Number 0));
      S.Instruction (I.Jmpf (E.Name "t1int"));
      S.Instruction (I.Pop (E.Name "ie"));
      S.Instruction I.Reti;

      S.Directive (D.Org 0x1f0);
      S.Label "goodbye";
      S.Instruction (I.Not1 (E.Name "ext", E.Number 0));
      S.Instruction (I.Jmpf (E.Name "goodbye"));

      S.Comment "\nHeader";
      S.Directive (D.Org 0x200);
      S.Directive (D.ByteString (Bitstring.bitstring_of_string "Tiny Tetris     "));
      S.Directive (D.ByteString (Bitstring.bitstring_of_string "Mini VMU Tetris by marcus       "));

      S.Comment "\nIconHeader";
      S.Directive (D.Org 0x240);
      S.Comment "Two Frames";
      S.Directive (D.Word [E.Number 2; E.Number 10]);

      S.Comment "\nIcon palette";
      S.Directive (D.Org 0x260)
    ] in
  List.iter (fun s -> print_endline (Statement.to_string s)) statements;
  let bytes = Vmu_Asm.assemble statements in
  write_bytes_to_file "/Users/walter/temp/test-output.vms" bytes


