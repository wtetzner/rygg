
module Label : sig
  type t
  type allocator

  val name : t -> string
  val subscript : t -> int

  val allocator : unit -> allocator
  val allocate : allocator -> string -> t
end = struct
  type t = {
      name: string;
      subscript: int
    }

  let name label = label.name
  let subscript label = label.subscript

  module StringMap = Map.Make(String)

  type allocator = (int StringMap.t) ref

  let allocator () = ref StringMap.empty
  let allocate allocator name =
    let sub = match StringMap.find_opt name !allocator with
      | None -> 0
      | Some old_sub -> old_sub + 1
    in
    begin
      let label = { name = name; subscript = sub } in
      allocator := StringMap.add name sub !allocator;
      label
    end
end

module Type : sig
  type t =
    | U8
end = struct
  type t =
    | U8
end

(* module Expr : sig
 *   type node =
 *     | Plus of t * t
 *     | Minus of t * t
 *     | Times of t * t
 *     | Divide of t * t
 * 
 *   type t = Type.t * node
 * end = struct
 * end
 * 
 * module Var = struct
 *   type t = Label.t * Type.t
 * end
 * 
 * module Statement : sig
 *   type t =
 *     | Assign of Var.t * Expr.t
 *     | Phi of Label.t list
 * end = struct
 * end
 * 
 * module BasicBlock : sig
 * end = struct
 *   type t = {
 *       stmts: Statement.t list
 *     }
 * end *)
