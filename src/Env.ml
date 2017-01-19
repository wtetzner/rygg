
module type Name = sig
  type t
  val matches : t -> t -> bool
end

module type Environment = sig
 type t
 type name
 type value

 val with_name : t -> name -> value -> t
 val lookup : t -> name -> value option
 val contains : t -> name -> bool
 val empty : t
end

module type Value = sig
  type t
end

module Make(N: Name)(V: Value) : Environment
       with type name = N.t
       with type value = V.t =
struct
  type t =
      Empty
    | Env of N.t * V.t * t

  type name = N.t
  type value = V.t

  let with_name env name value =
    Env (name, value, env)

  let empty = Empty

  let rec lookup env name =
    match env with
        Empty -> None
      | Env (ename, value, parent) ->
          if N.matches name ename then
            Some value
          else
            lookup parent name

  let contains env name =
    match lookup env name with
    | Some _ -> true
    | None -> false
end

