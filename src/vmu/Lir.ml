
module E = Asm.Expression
module S = Asm.Statement
module D = Asm.Directive
module I = Asm.Instruction

type name = string
module EnvMap = Map.Make(String)

let extra_var_location idx =
  match idx with
  | 0  -> 0x10A
  | 1  -> 0x10B
  | 2  -> 0x10C
  | 3  -> 0x10F
  | 4  -> 0x116
  | 5  -> 0x117
  | 6  -> 0x119
  | 7  -> 0x11E
  | 8  -> 0x11F
  | 9  -> 0x121
  | 10 -> 0x126
  | 11 -> 0x128
  | 12 -> 0x129
  | 13 -> 0x12A
  | 14 -> 0x12B
  | 15 -> 0x12C
  | 16 -> 0x12D
  | 17 -> 0x12E
  | 18 -> 0x12F
  | 19 -> 0x133
  | 20 -> 0x136
  | 21 -> 0x136
  | 22 -> 0x137
  | 23 -> 0x138
  | 24 -> 0x139
  | 25 -> 0x13A
  | 26 -> 0x13B
  | 27 -> 0x13C
  | 28 -> 0x13D
  | 29 -> 0x13E
  | 30 -> 0x13F
  | 31 -> 0x140
  | 32 -> 0x141
  | 33 -> 0x142
  | 34 -> 0x143
  | 35 -> 0x147
  | 36 -> 0x148
  | 37 -> 0x149
  | 38 -> 0x14A
  | 39 -> 0x14B
  | 40 -> 0x14F
  | 41 -> 0x150
  | 42 -> 0x151
  | 43 -> 0x152
  | 44 -> 0x153
  | 45 -> 0x154
  | 46 -> 0x155
  | 47 -> 0x156
  | 48 -> 0x157
  | 49 -> 0x158
  | 50 -> 0x159
  | 51 -> 0x15A
  | 52 -> 0x15B
  | 53 -> 0x160
  | 54 -> 0x161
  | 55 -> 0x162
  | 56 -> 0x168
  | 57 -> 0x169
  | 58 -> 0x16A
  | 59 -> 0x16B
  | 60 -> 0x16C
  | 61 -> 0x16D
  | 62 -> 0x16E
  | 63 -> 0x16F
  | 64 -> 0x170
  | 65 -> 0x171
  | 66 -> 0x172
  | 67 -> 0x173
  | 68 -> 0x174
  | 69 -> 0x175
  | 70 -> 0x176
  | 71 -> 0x177
  | 72 -> 0x178
  | 73 -> 0x179
  | 74 -> 0x17A
  | 75 -> 0x17B
  | 76 -> 0x17C
  | 77 -> 0x17D
  | 78 -> 0x17E
  | 79 -> 0x1FC
  | 80 -> 0x1FD
  | 81 -> 0x1FE
  | 82 -> 0x1FF

module Type = struct
  type t =
    | I8

  let size typ =
    match typ with
    | I8 -> 1
end

module Variable = struct
  type t = {
      location: int;
      vartype: Type.t
    }
end

let stream_map f stream =
  let rec next i =
    try Some (f (Stream.next stream))
    with Stream.Failure -> None in
  Stream.from next

let stream_concat streams =
  let current_stream = ref None in
  let rec next i =
    try
      let stream =
        match !current_stream with
        | Some stream -> stream
        | None ->
           let stream = Stream.next streams in
           current_stream := Some stream;
           stream in
      try Some (Stream.next stream)
      with Stream.Failure -> (current_stream := None; next i)
    with Stream.Failure -> None in
  Stream.from next

module Statements : sig
  type t

  val make : unit -> t
  val singleton : S.t -> t
  val append : t -> S.t -> unit
  val append_all : t -> t -> unit
  val to_stream : t -> S.t Stream.t
  val concat : t list -> t
end = struct
  type t = (S.t list) ref

  let make () = ref []

  let append statements statement = statements := statement :: !statements

  let singleton statement =
    let statements = make () in
    append statements statement;
    statements

  let to_stream statements =
    Stream.of_list (List.rev !statements)

  let append_all statements new_statements =
    Stream.iter (fun st -> append statements st)
      (to_stream new_statements)

  let concat args =
    let statements = ref [] in
    List.iter (fun s ->
        Stream.iter (fun st -> append statements st) (to_stream s)) args;
    statements
end

module Expression : sig
  type t = Type.t * expr
  and expr =
    | Call of name * (t list)
    | TailCall of name * (t list)
    | Let of name * t * t
    | RamVar of name
    | Const of name
    | I8 of int
end = struct
  type t = Type.t * expr
  and expr =
    | Call of name * (t list)
    | TailCall of name * (t list)
    | Let of name * t * t
    | RamVar of name
    | Const of name
    | I8 of int

  let get_type expr =
    match expr with
    | (ty, _) -> ty

  let rec compile variables expr =
    match expr with
    | (ty, Call (name, args)) ->
       let instrs = Statements.make () in
       List.iter
         (fun arg ->
           Statements.append instrs (S.instruction (I.Push (E.var "acc")))) args;
       Statements.append instrs (S.instruction (I.Callf (E.var name)));
       instrs
    | (ty, RamVar name) ->
       Statements.singleton (S.instruction (I.Ld_d9 (E.var name)))
end

module Function = struct
  type arg = Type.t * name

  type t =
    | Normal of (arg list) * Expression.t * Type.t
    | AsmFunc of S.t list

  let compile_one statements func variables =
    (match func with
     | Normal (args, body, typ) ->
        ()
     | AsmFunc stmts ->
        List.iter
          (fun stmt -> Statements.append statements stmt) stmts);
    Statements.append statements (S.instruction I.Ret)

  let compile statements functions variables =
    Env.iter (fun name func ->
        Statements.append statements (S.comment (Printf.sprintf "\nThe %s function" name));
        Statements.append statements (S.label name);
        compile_one statements func variables
      ) functions
end

module Compiler = struct
  let compile functions variables =
    let statements = Statements.make () in
    Statements.append statements (S.comment "Special Function Registers");
    Statements.append statements (S.alias ("acc", E.num 0x100));
    Statements.append statements (S.alias ("psw", E.num 0x101));
    Statements.append statements (S.alias ("b", E.num 0x102));
    Statements.append statements (S.alias ("c", E.num 0x103));
    Statements.append statements (S.alias ("trl", E.num 0x104));
    Statements.append statements (S.alias ("trh", E.num 0x105));
    Statements.append statements (S.alias ("sp", E.num 0x106));
    Statements.append statements (S.alias ("pcon", E.num 0x107));
    Statements.append statements (S.alias ("ie", E.num 0x108));
    Statements.append statements (S.alias ("ip", E.num 0x109));
    Statements.append statements (S.alias ("ext", E.num 0x10D));
    Statements.append statements (S.alias ("ocr", E.num 0x10E));
    Statements.append statements (S.alias ("t0con", E.num 0x110));
    Statements.append statements (S.alias ("t0prr", E.num 0x111));
    Statements.append statements (S.alias ("t0l", E.num 0x112));
    Statements.append statements (S.alias ("t0lr", E.num 0x113));
    Statements.append statements (S.alias ("t0h", E.num 0x114));
    Statements.append statements (S.alias ("t0hr", E.num 0x115));
    Statements.append statements (S.alias ("t1cnt", E.num 0x118));
    Statements.append statements (S.alias ("t1lc", E.num 0x11A));
    Statements.append statements (S.alias ("t1l", E.num 0x11B));
    Statements.append statements (S.alias ("t1lr", E.num 0x11B));
    Statements.append statements (S.alias ("t1hc", E.num 0x11C));
    Statements.append statements (S.alias ("t1h", E.num 0x11D));
    Statements.append statements (S.alias ("t1hr", E.num 0x11D));
    Statements.append statements (S.alias ("mcr", E.num 0x120));
    Statements.append statements (S.alias ("stad", E.num 0x122));
    Statements.append statements (S.alias ("cnr", E.num 0x123));
    Statements.append statements (S.alias ("tdr", E.num 0x124));
    Statements.append statements (S.alias ("xbnk", E.num 0x125));
    Statements.append statements (S.alias ("vccr", E.num 0x127));
    Statements.append statements (S.alias ("scon0", E.num 0x130));
    Statements.append statements (S.alias ("sbuf0", E.num 0x131));
    Statements.append statements (S.alias ("sbr", E.num 0x132));
    Statements.append statements (S.alias ("scon1", E.num 0x134));
    Statements.append statements (S.alias ("sbuf1", E.num 0x135));
    Statements.append statements (S.alias ("p1", E.num 0x144));
    Statements.append statements (S.alias ("p1ddr", E.num 0x145));
    Statements.append statements (S.alias ("p1fcr", E.num 0x146));
    Statements.append statements (S.alias ("p3", E.num 0x14C));
    Statements.append statements (S.alias ("p3ddr", E.num 0x14D));
    Statements.append statements (S.alias ("p3int", E.num 0x14E));
    Statements.append statements (S.alias ("p7", E.num 0x15C));
    Statements.append statements (S.alias ("i01cr", E.num 0x15D));
    Statements.append statements (S.alias ("i23cr", E.num 0x15E));
    Statements.append statements (S.alias ("isl", E.num 0x15F));
    Statements.append statements (S.alias ("vsel", E.num 0x163));
    Statements.append statements (S.alias ("vrmad1", E.num 0x164));
    Statements.append statements (S.alias ("vrmad2", E.num 0x165));
    Statements.append statements (S.alias ("vtrbf", E.num 0x166));
    Statements.append statements (S.alias ("vlreg", E.num 0x167));
    Statements.append statements (S.alias ("btcr", E.num 0x17F));
    Statements.append statements (S.alias ("xram", E.num 0x180));

    Statements.append statements (S.comment "\nPSW bits");
    Statements.append statements (S.alias ("cy", E.num 7));
    Statements.append statements (S.alias ("ac", E.num 6));
    Statements.append statements (S.alias ("irbk1", E.num 4));
    Statements.append statements (S.alias ("irbk0", E.num 3));
    Statements.append statements (S.alias ("ov", E.num 2));
    Statements.append statements (S.alias ("rambk0", E.num 1));
    Statements.append statements (S.alias ("p", E.num 0));
    Function.compile statements functions variables;
    Statements.to_stream statements
end
