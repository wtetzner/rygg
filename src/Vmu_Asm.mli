
module Location = Span.Location

module Position: sig
  type t =
    | Location of Location.t
    | Span of Span.t
    | No_position

  val merge : t -> t -> t
end

exception Asm_failure of Position.t * string

module Environment : Env.Environment
       with type name = string
       with type value = int

module Expression : sig
  type t = { pos: Position.t; expr: expr_type }
  and expr_type =
    | Name of string
    | Plus of t * t
    | Minus of t * t
    | Times of t * t
    | Divide of t * t
    | Number of int
    | UpperByte of t
    | LowerByte of t

  val (+): t -> t -> t
  val (-): t -> t -> t
  val ( * ): t -> t -> t
  val (/): t -> t -> t
  val num : int -> t
  val var: string -> t
  val upper: t -> t
  val lower: t -> t

  val eval : t -> Environment.t -> int

  val to_string : t -> string
end

module IndirectionMode : sig
  type t =
    | R0
    | R1
    | R2
    | R3

  val index : t -> int

  val to_string : t -> string
end

module Instruction : sig
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

  val encode : t -> int -> Environment.t -> Bitstring.t

  val size : t -> int

  val to_string : t -> string
end

module Directive : sig
  type t =
    | Byte of Expression.t list
    | ByteString of Bitstring.t
    | Org of int
    | Word of Expression.t list
    | Cnop of Expression.t * Expression.t

  val to_string : t -> string
end

module Statement : sig
  type t =
    | Directive of Directive.t
    | Label of string
    | Instruction of Instruction.t
    | Variable of string * Expression.t
    | Alias of string * Expression.t
    | Comment of string

  val to_string : t -> string
end

val assemble : Statement.t list -> Bytes.t
