
module Lang(Loc: Module.LOC)(Name: Module.NAME)(SourceId: Module.NAME) = struct
  module Name = Name
  module SourceId = SourceId
  module Loc = Loc
  module Ident = Module.Make_ident(Name)(Loc)
  module Path = Module.Make_path(Ident)(Name)(Loc)

  type num_bits = int
  type signedness = Signed | Unsigned

  type ctype =
    | Void of Loc.t option
    | Int of Loc.t option * signedness * num_bits
    | Float of Loc.t option * num_bits
    | Pointer of Loc.t option * ctype
    | Function of Loc.t option * ctype list * ctype
    | Typename of Path.t
  type 'typ expr =
    | Intconst of Loc.t option * int * num_bits option
    | Floatconst of Loc.t option * float * num_bits option
    | Stringconst of Loc.t option * string
    | Variable of Path.t * 'typ
    | Apply of Loc.t option * 'typ expr * ('typ expr) list * 'typ
    | Assign of Loc.t option * 'typ expr * 'typ expr * 'typ
    | Unary_op of Loc.t option * string * 'typ expr * 'typ
    | Binary_op of Loc.t option * string * 'typ expr * 'typ
    | Post_op of Loc.t option * 'typ expr * string * 'typ
    | Cast of Loc.t option * 'typ expr * ctype
  type 'typ statement =
    | Expr of Loc.t option * 'typ expr
    | If of Loc.t option * 'typ expr * 'typ statement
    | If_then of Loc.t option * 'typ expr * 'typ statement * 'typ statement
    | Return of Loc.t option * 'typ expr
    | Block of Loc.t option * ('typ block_entry) list
  and 'typ block_entry =
    | Statement of 'typ statement
    | Local_decl of Ident.t * ctype
    | Local_decl_assign of 'typ expr * 'typ expr

  type 'typ cterm =
    | Var_decl of Loc.t option * ctype
    | Fun_def of Loc.t option * (Ident.t * ctype) list * ctype * 'typ statement
  type value_type = ctype
  type typ = ctype
  type kind = unit
  type term = (ctype option) cterm
  type typed_term = ctype cterm

  type value_type_alias = value_type
  type typ_alias = typ
  type kind_alias = kind
  type term_alias = term
  type typed_term_alias = typed_term

  module TypeChecker
           (Module_syntax: Module.MODULE_SYNTAX
            with type value_type = value_type
            with type typ = typ
            with type kind = kind
            with type term = term
            with module Name = Name
            with module Loc = Loc
            with module Ident = Ident
            with module Path = Path)
           (Env: Module.ENV
            with type ident = Ident.t
            with type path = Path.t
            with type name = Name.t
            with type type_decl = Module_syntax.type_decl
            with type module_type = Module_syntax.module_type
            with type value_type = value_type) = struct
    type env = Env.t
    type ident = Ident.t
    type name = Name.t
    type value_type = value_type_alias
    type typ = typ_alias
    type kind = kind_alias
    type term = term_alias
    type type_decl = Module_syntax.type_decl
    type module_type = Module_syntax.module_type
    type type_error = [
        | `Unbound_type of Path.t
        | `Unbound_module_type of Path.t
        | `Unbound_value_type of Path.t
    ]

    (* Typing functions *)
    (* let type_term = Env.t -> term -> (value_type, type_error) result
     * let kind_deftype = Env.t -> typ -> (kind, type_error) result *)
    let rec check_valtype env value_type =
      match value_type with
      | Typename path -> begin
         match Env.find_type env path with
         | Some typ -> Ok value_type
         | None -> Error [`Unbound_type path]
        end
      | Pointer (_, typ) -> check_valtype env typ
      | Function(_, args, res) -> begin
          let check_results = List.map (check_valtype env) args in
          let return_result = check_valtype env res in
          let result_list = List.append check_results [return_result] in
          let errors = List.filter_map (function
                           | Ok _ -> None
                           | Error err -> Some err)
                         result_list in
          if List.length errors > 0 then
            Error (List.concat errors)
          else
            Ok value_type
        end
      | _ -> Ok value_type
    let kind_deftype env typ =
      match check_valtype env typ with
      | Ok result -> Ok ()
      | Error e -> Error e

    let deftype_of_path path kind = Typename path

    let kind_match env k1 k2 = true

    let get_type env path =
      match Env.find_type env path with
      | Some typ -> typ
      | None -> raise (Failure ("Unbound type: " ^ (Path.to_string path)))

    let rec valtype_match env ty1 ty2 =
      let open Module_syntax in
      match (ty1, ty2) with
      | (Void _, Void _) -> true
      | (Int(_, Signed, bits1), Int(_, Signed, bits2)) -> bits1 = bits2
      | (Int(_, Unsigned, bits1), Int(_, Unsigned, bits2)) -> bits1 = bits2
      | (Int(_, _, _), Int(_, _, _)) -> false
      | (Float(_, bits1), Float(_, bits2)) -> bits1 = bits2
      | (Function(loc1, args1, res1), Function(loc2, args2, res2)) ->
         List.length args1 = List.length args2
         && List.for_all2 (valtype_match env) args1 args2
         && valtype_match env res1 res2
      | (Typename path1, Typename path2) ->
         Path.equal path1 path2 || begin
            match (get_type env path1, get_type env path2) with
            | ({ concrete = Some def }, _) -> valtype_match env def ty2
            | (_, { concrete = Some def }) -> valtype_match env ty1 def
            | ({ concrete = None }, { concrete = None }) -> false
          end
      | (Typename path1, _) -> begin
           match get_type env path1 with
           | { concrete = Some def } -> valtype_match env def ty2
           | { concrete = None } -> false
         end
      | (_, Typename path2) -> begin
           match get_type env path2 with
           | { concrete = Some def } -> valtype_match env ty1 def
           | { concrete = None } -> false
         end
      | (_, _) -> false

    let rec expand_type env typ =
      match typ with
      | Typename path -> begin
          match get_type env path with
          | { Module_syntax.concrete = Some ty } -> expand_type env ty
          | { Module_syntax.concrete = None } -> Typename path
        end
      | ty -> ty

    (* Type matching functions *)
    (* let valtype_match : Env.t -> value_type -> value_type -> bool
     * let deftype_equiv : Env.t -> kind -> typ -> typ -> bool
     * let kind_match : Env.t -> kind -> kind -> bool
     * let deftype_of_path : Path.t -> kind -> typ *)
  end
end

(* module ModularizedLang = Module.Modularize(Lang) *)
