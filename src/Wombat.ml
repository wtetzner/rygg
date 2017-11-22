
module Span = Compiler.Span
module Location = Span.Location
module Out_channel = Core.Out_channel
module In_channel = Core.In_channel
module Command = Core.Command

let write_bytes_to_file file bytes =
  Out_channel.with_file file ~f:(fun f -> Out_channel.output_string f bytes);
  ()

let load_string filename =
  In_channel.with_file filename ~f:(fun f -> In_channel.input_all f)

let parse_expr str =
  Printf.printf "parsing \"%s\"" str;
  print_newline ();
    let parsed = Vmu.Asm.Parser.Parser.expression (Vmu.Asm.Parser.Lexer.tokens str) in
    Printf.printf "expression \"%s\" parsed to: %s\n" str (Vmu.Asm.Expression.to_string parsed)

let parse_statement str =
  Printf.printf "parsing statement \"%s\"" str;
  print_newline ();
  let parsed = Vmu.Asm.Parser.Parser.instruction (Vmu.Asm.Parser.Lexer.tokens str) in
  Printf.printf "statement \"%s\" parsed to: %s\n" str (Vmu.Asm.Statement.to_string parsed)


let assemble input_file output_file =
  try
    ANSITerminal.(print_string [red] "cool\n");

    parse_expr ">>2 + name * ((4)) + 4";
    parse_expr "2 + 3";
    parse_expr "2 + 3 * 7";
    parse_expr "2 + 3 - 7 + 5";
    parse_expr "2 + 3 * 7 + 4";
    parse_expr "2 + 3 + (4 - 5)";

    parse_statement "add #45 * 3";
    parse_statement "add 7 * 6 + 4";
    parse_statement "bp 34, 45, >32";
    parse_statement "mov #34, $75";
    parse_statement "mOv #34, @r2";
    parse_statement "add @r3";

    let input_text = load_string input_file in
    let tokens = Vmu.Asm.Parser.Lexer.tokens input_text in
    Stream.iter (fun t -> Printf.printf "%s\n" (Vmu.Asm.Parser.Token.to_string t)) tokens;

    let module S = Vmu.Asm.Statement in
    let module D = Vmu.Asm.Directive in
    let module I = Vmu.Asm.Instruction in
    let module E = Vmu.Asm.Expression in
    let statements = [
        S.comment "Special Function Register addresses";
        S.alias ("ACC", E.num 0x100);
        S.alias ("PSW", E.num 0x101);
        S.alias ("B", E.num 0x102);
        S.alias ("C", E.num 0x103);
        S.alias ("TRL", E.num 0x104);
        S.alias ("TRH", E.num 0x105);
        S.alias ("SP", E.num 0x106);
        S.alias ("PCON", E.num 0x107);
        S.alias ("IE", E.num 0x108);
        S.alias ("IP", E.num 0x109);
        S.alias ("EXT", E.num 0x10D);
        S.alias ("OCR", E.num 0x10E);
        S.alias ("T0CON", E.num 0x110);
        S.alias ("T0PRR", E.num 0x111);
        S.alias ("T0L", E.num 0x112);
        S.alias ("T0LR", E.num 0x113);
        S.alias ("T0H", E.num 0x114);
        S.alias ("T0HR", E.num 0x115);
        S.alias ("T1CNT", E.num 0x118);
        S.alias ("T1LC", E.num 0x11A);
        S.alias ("T1L", E.num 0x11B);
        S.alias ("T1LR", E.num 0x11B);
        S.alias ("T1HC", E.num 0x11C);
        S.alias ("T1H", E.num 0x11D);
        S.alias ("T1HR", E.num 0x11D);
        S.alias ("MCR", E.num 0x120);
        S.alias ("STAD", E.num 0x122);
        S.alias ("CNR", E.num 0x123);
        S.alias ("TDR", E.num 0x124);
        S.alias ("XBNK", E.num 0x125);
        S.alias ("VCCR", E.num 0x127);
        S.alias ("SCON0", E.num 0x130);
        S.alias ("SBUF0", E.num 0x131);
        S.alias ("SBR", E.num 0x132);
        S.alias ("SCON1", E.num 0x134);
        S.alias ("SBUF1", E.num 0x135);
        S.alias ("P1", E.num 0x144);
        S.alias ("P1DDR", E.num 0x145);
        S.alias ("P1FCR", E.num 0x146);
        S.alias ("P3", E.num 0x14C);
        S.alias ("P3DDR", E.num 0x14D);
        S.alias ("P3INT", E.num 0x14E);
        S.alias ("P7", E.num 0x15C);
        S.alias ("I01CR", E.num 0x15D);
        S.alias ("I23CR", E.num 0x15E);
        S.alias ("ISL", E.num 0x15F);
        S.alias ("VSEL", E.num 0x163);
        S.alias ("VRMAD1", E.num 0x164);
        S.alias ("VRMAD2", E.num 0x165);
        S.alias ("VTRBF", E.num 0x166);
        S.alias ("VLREG", E.num 0x167);
        S.alias ("BTCR", E.num 0x17F);
        S.alias ("XRAM", E.num 0x180);

        S.comment "\nPSW bits";
        S.alias ("CY", E.num 7);
        S.alias ("AC", E.num 6);
        S.alias ("IRBK1", E.num 4);
        S.alias ("IRBK0", E.num 3);
        S.alias ("OV", E.num 2);
        S.alias ("RAMBK0", E.num 1);
        S.alias ("P", E.num 0);

        S.comment "\nGame variables";
        S.variable ("piece_x", E.num 0x30);
        S.variable ("piece_y", E.num 0x31);
        S.variable ("piece_n", E.num 0x32);
        S.variable ("piece_r", E.num 0x33);
        S.variable ("piece_i0", E.num 0x34);
        S.variable ("piece_i1", E.num 0x35);
        S.variable ("gotkeys", E.num 0x36);
        S.variable ("time", E.num 0x37);
        S.variable ("speed", E.num 0x38);
        S.variable ("scorelo", E.num 0x39);
        S.variable ("scorehi", E.num 0x3a);
        S.variable ("keyinhib", E.num 0x3b);
        S.variable ("seed", E.num 0x3c);

        S.variable ("hitmap", E.num 0x3e);

        S.comment "\nReset and interrupt vectors";
        S.directive (D.Org 0);
        S.instruction (I.Jmpf (E.var "start"));

        S.directive (D.Org 0x3);
        S.instruction (I.Jmp (E.var "nop_irq"));

        S.directive (D.Org 0xb);
        S.instruction (I.Jmp (E.var "nop_irq"));

        S.directive (D.Org 0x13);
        S.instruction (I.Jmp (E.var "nop_irq"));

        S.directive (D.Org 0x1b);
        S.instruction (I.Jmp (E.var "t1int"));

        S.directive (D.Org 0x23);
        S.instruction (I.Jmp (E.var "nop_irq"));

        S.directive (D.Org 0x2b);
        S.instruction (I.Jmp (E.var "nop_irq"));

        S.directive (D.Org 0x33);
        S.instruction (I.Jmp (E.var "nop_irq"));

        S.directive (D.Org 0x3b);
        S.instruction (I.Jmp (E.var "nop_irq"));

        S.directive (D.Org 0x43);
        S.instruction (I.Jmp (E.var "nop_irq"));

        S.directive (D.Org 0x4b);
        S.instruction (I.Jmp (E.var "nop_irq"));

        S.instruction (I.Clr1 (E.var "p3int", E.num 0));
        S.instruction (I.Clr1 (E.var "p3int", E.num 1));

        S.label "nop_irq";
        S.instruction I.Reti;

        S.directive (D.Org 0x130);
        S.label "t1int";
        S.instruction (I.Push (E.var "ie"));
        S.instruction (I.Clr1 (E.var "ie", E.num 7));
        S.instruction (I.Not1 (E.var "ext", E.num 0));
        S.instruction (I.Jmpf (E.var "t1int"));
        S.instruction (I.Pop (E.var "ie"));
        S.instruction I.Reti;

        S.directive (D.Org 0x1f0);
        S.label "goodbye";
        S.instruction (I.Not1 (E.var "ext", E.num 0));
        S.instruction (I.Jmpf (E.var "goodbye"));

        S.comment "\nHeader";
        S.directive (D.Org 0x200);
        S.directive (D.ByteString (Bitstring.bitstring_of_string "Tiny Tetris     "));
        S.directive (D.ByteString (Bitstring.bitstring_of_string "Mini VMU Tetris by marcus       "));

        S.comment "\nIconHeader";
        S.directive (D.Org 0x240);
        S.comment "Two Frames";
        S.directive (D.Word [E.num 2; E.num 10]);

        S.comment "\nIcon palette";
        S.directive (D.Org 0x260);

        S.directive (D.Word [E.num 0x0000; E.num 0xFCFC;
                             E.num 0xF0A0; E.num 0xF0F0;
                             E.num 0xFCCF; E.num 0xF00A;
                             E.num 0xF00F; E.num 0xFFFF]);
        S.directive (D.Word [E.num 0xFFFF; E.num 0xFFFF;
                             E.num 0xFFFF; E.num 0xFFFF;
                             E.num 0xFFFF; E.num 0xFFFF;
                             E.num 0xFFFF; E.num 0xFFFF]);

        S.comment "\nIcon palette";
        S.directive (D.Org 0x260);

        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x11; E.num 0x11;
                             E.num 0x11; E.num 0x11;
                             E.num 0x11; E.num 0x10;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x22; E.num 0x22;
                             E.num 0x31; E.num 0x22;
                             E.num 0x22; E.num 0x30;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x22; E.num 0x22;
                             E.num 0x31; E.num 0x22;
                             E.num 0x22; E.num 0x30;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x22; E.num 0x22;
                             E.num 0x31; E.num 0x22;
                             E.num 0x22; E.num 0x30;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x22; E.num 0x22;
                             E.num 0x31; E.num 0x22;
                             E.num 0x22; E.num 0x30;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x33; E.num 0x33;
                             E.num 0x31; E.num 0x33;
                             E.num 0x33; E.num 0x30;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x01; E.num 0x11;
                             E.num 0x11; E.num 0x11;
                             E.num 0x11; E.num 0x11;
                             E.num 0x10; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x01; E.num 0x22;
                             E.num 0x22; E.num 0x31;
                             E.num 0x22; E.num 0x22;
                             E.num 0x30; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x01; E.num 0x22;
                             E.num 0x22; E.num 0x31;
                             E.num 0x22; E.num 0x22;
                             E.num 0x30; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x01; E.num 0x22;
                             E.num 0x22; E.num 0x31;
                             E.num 0x22; E.num 0x22;
                             E.num 0x30; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x01; E.num 0x22;
                             E.num 0x22; E.num 0x31;
                             E.num 0x22; E.num 0x22;
                             E.num 0x30; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x01; E.num 0x33;
                             E.num 0x33; E.num 0x31;
                             E.num 0x33; E.num 0x33;
                             E.num 0x30; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x44; E.num 0x44;
                             E.num 0x40; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x66; E.num 0x66;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x44; E.num 0x44;
                             E.num 0x44; E.num 0x44;
                             E.num 0x44; E.num 0x44;
                             E.num 0x44; E.num 0x44;
                             E.num 0x40; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x64; E.num 0x55;
                             E.num 0x55; E.num 0x64;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x64; E.num 0x55;
                             E.num 0x55; E.num 0x64;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x64; E.num 0x55;
                             E.num 0x55; E.num 0x64;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x64; E.num 0x55;
                             E.num 0x55; E.num 0x64;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x66; E.num 0x66;
                             E.num 0x64; E.num 0x66;
                             E.num 0x66; E.num 0x64;
                             E.num 0x66; E.num 0x66;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);

        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x11; E.num 0x11;
                             E.num 0x11; E.num 0x11;
                             E.num 0x11; E.num 0x10;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x22; E.num 0x22;
                             E.num 0x31; E.num 0x22;
                             E.num 0x22; E.num 0x30;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x22; E.num 0x22;
                             E.num 0x31; E.num 0x22;
                             E.num 0x22; E.num 0x30;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x22; E.num 0x22;
                             E.num 0x31; E.num 0x22;
                             E.num 0x22; E.num 0x30;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x22; E.num 0x22;
                             E.num 0x31; E.num 0x22;
                             E.num 0x22; E.num 0x30;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x01;
                             E.num 0x33; E.num 0x33;
                             E.num 0x31; E.num 0x33;
                             E.num 0x33; E.num 0x30;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x44; E.num 0x44;
                             E.num 0x41; E.num 0x11;
                             E.num 0x11; E.num 0x11;
                             E.num 0x11; E.num 0x11;
                             E.num 0x10; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x61; E.num 0x22;
                             E.num 0x22; E.num 0x31;
                             E.num 0x22; E.num 0x22;
                             E.num 0x30; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x61; E.num 0x22;
                             E.num 0x22; E.num 0x31;
                             E.num 0x22; E.num 0x22;
                             E.num 0x30; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x61; E.num 0x22;
                             E.num 0x22; E.num 0x31;
                             E.num 0x22; E.num 0x22;
                             E.num 0x30; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x61; E.num 0x22;
                             E.num 0x22; E.num 0x31;
                             E.num 0x22; E.num 0x22;
                             E.num 0x30; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x66; E.num 0x66;
                             E.num 0x61; E.num 0x33;
                             E.num 0x33; E.num 0x31;
                             E.num 0x33; E.num 0x33;
                             E.num 0x30; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x44; E.num 0x44;
                             E.num 0x44; E.num 0x44;
                             E.num 0x44; E.num 0x44;
                             E.num 0x44; E.num 0x44;
                             E.num 0x40; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x64; E.num 0x55;
                             E.num 0x55; E.num 0x64;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x64; E.num 0x55;
                             E.num 0x55; E.num 0x64;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x64; E.num 0x55;
                             E.num 0x55; E.num 0x64;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x55; E.num 0x55;
                             E.num 0x64; E.num 0x55;
                             E.num 0x55; E.num 0x64;
                             E.num 0x55; E.num 0x55;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x04;
                             E.num 0x66; E.num 0x66;
                             E.num 0x64; E.num 0x66;
                             E.num 0x66; E.num 0x64;
                             E.num 0x66; E.num 0x66;
                             E.num 0x60; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);
        S.directive (D.Byte [E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00;
                             E.num 0x00; E.num 0x00]);

        S.comment "\nMain program";
        S.directive (D.Org 0x680);
        S.label "start";
        S.instruction (I.Clr1 (E.var "ie", E.num 7));
        S.instruction (I.Mov_d9 (E.num 0xA1, E.var "ocr"));
        S.instruction (I.Mov_d9 (E.num 0x09, E.var "mcr"));
        S.instruction (I.Mov_d9 (E.num 0x80, E.var "vccr"));
        S.instruction (I.Clr1 (E.var "p3int", E.num 0));
        S.instruction (I.Clr1 (E.var "p1", E.num 7));
        S.instruction (I.Mov_d9 (E.num 0xFF, E.var "p3"));

        S.instruction (I.Clr1 (E.var "psw", E.num 1));
        S.instruction (I.Ld_d9 (E.num 0x1C));
        S.instruction (I.Mov_d9 (E.num 0xFF, E.var "BooBob"));
      ] in
    (* List.iter (fun s -> print_endline (Statement.to_string s)) statements; *)
    ANSITerminal.(print_string [red] "cool\n");
    let bytes = Vmu.Asm.assemble statements in
    write_bytes_to_file "/Users/walter/temp/test-output.vms" bytes;
    print_endline "hmm";
    ()
  with
  | Vmu.Asm.Asm_failure (pos,msg) -> Compiler.Message.(print_msgln Error pos msg)
  | Vmu.Asm.Parser.Lexer_failure (loc, msg) -> Compiler.Message.(print_msgln Error (Compiler.Position.Location loc) msg)
  | Vmu.Asm.Parser.Parse_failure (span, msg) -> Compiler.Message.(print_msgln Error (Compiler.Position.Span span) msg)

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
        assemble filename output
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

