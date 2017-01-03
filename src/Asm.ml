
module Name = struct
    type t = string
    let matches s1 s2 = String.equal s1 s2
end

module Environment = Env.Make (Name) (struct type t = int end)

type expression =
  | Name of string
  | Plus of expression * expression
  | Minus of expression * expression
  | Times of expression * expression
  | Divide of expression * expression
  | Number of int
  | UpperByte of expression
  | LowerByte of expression

let complex expr =
  match expr with
  | Name _ -> false
  | Plus (_,_) | Minus (_,_) | Times (_,_) | Divide (_,_)  -> true
  | Number _ -> false
  | UpperByte _ -> true
  | LowerByte _ -> true

let rec to_string expr =
  match expr with
  | Name name -> name
  | Plus (e1, e2) -> Printf.sprintf "%s + %s" (wrap e1) (wrap e2)
  | Minus (e1, e2) -> Printf.sprintf "%s - %s" (wrap e1) (wrap e2)
  | Times (e1, e2) -> Printf.sprintf "%s * %s" (wrap e1) (wrap e2)
  | Divide (e1, e2) -> Printf.sprintf "%s / %s" (wrap e1) (wrap e2)
  | Number e -> string_of_int e
  | UpperByte e -> Printf.sprintf ">%s" (wrap e)
  | LowerByte e -> Printf.sprintf "<%s" (wrap e)
and wrap expr =
  if complex expr then
    Printf.sprintf "(%s)" (to_string expr)
  else
    to_string expr

exception Not_found of string

let rec eval expr env =
  match expr with
  | Name name -> (match Environment.lookup env name with
                  | Some(result) -> result
                  | None -> raise (Not_found (Printf.sprintf "Name '%s' not found" name)))
  | Plus (e1, e2) -> (eval e1 env) + (eval e2 env)
  | Minus (e1, e2) -> (eval e1 env) - (eval e2 env)
  | Times (e1, e2) -> (eval e1 env) * (eval e2 env)
  | Divide (e1, e2) -> (eval e1 env) / (eval e2 env)
  | Number n -> n
  | UpperByte e -> let n = eval e env in (n lsr 8) land 0xFF
  | LowerByte e -> let n = eval e env in n land 0xFF

