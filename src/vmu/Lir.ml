
module Expression = Asm.Expression

module Operand = struct
  type t =
    | IndirectReference of Expression.t
    | Reference of Expression.t
    | Immediate of Expression.t
end

module Instruction = struct
  type t =
    | Mul
    | Div
    | Add of Operand.t
end

