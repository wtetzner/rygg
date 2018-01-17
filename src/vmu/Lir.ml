
module E = Asm.Expression
module S = Asm.Statement
module D = Asm.Directive
module I = Asm.Instruction

type name = string
module EnvMap = Map.Make(String)

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

module Env = struct
  type t = {
      global: Variable.t EnvMap.t;
      local: Variable.t EnvMap.t;
      max_local: int (* The largest local variable location *)
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
    | RomVar of name
    | I8 of int
end = struct
  type t = Type.t * expr
  and expr =
    | Call of name * (t list)
    | TailCall of name * (t list)
    | Let of name * t * t
    | RamVar of name
    | RomVar of name
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
     | Normal (args, body) ->
        
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
