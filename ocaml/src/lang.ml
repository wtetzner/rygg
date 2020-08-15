
module Lang(Loc: LOC)(Name: NAME)(SourceId: NAME) = struct
  module Name = Name
  module SourceId = SourceId
  module Loc = Loc
  module Ident = Module.Make_ident(Name)(Loc)
  module Path = Module.Make_path(Ident)

  type signedness = Signed | Unsigned

  type int_type = { bits: int; signedness: signedness }

  type float_bits = int

  type literal = Int_lit of Big_int * int_type option
               | Float_lit of Big_int * Big_int * float_bits option
               | String_lit of string

  type kind = int

  type type_var = string

  type rec simple_type =
    | Typename of Path.t
    | Int of Big_int * int_type
    | Float of Big_int * Big_int * float_bits
    | Array of typ * int (* Contained type plus length *)
  and typ = {
    
  }

  type rec 'typ expr =
    | Let of let_expr * 'typ
    | Literal of literal * 'typ
    | Binary_op of Ident.t * 'typ expr * 'typ expr * 'typ
    | Unary_op of Ident.t * 'typ expr * 'typ
    | If of 'typ expr * 'typ expr
    | If_else of 'typ expr * 'typ expr * 'typ expr * 'typ
    | Apply of 'typ expr * ('typ expr) list * 'typ
  and 'typ let_expr = {
      name: Ident.t;
      var_type: 'typ;
      value: 'typ expr;
      body: 'typ expr
  }

  type 'typ lang_term =
    | Function_def of Ident.t * (Ident.t list) * ('typ expr) * typ option

  type value_type = typ

  type typ_alias = typ
  type value_type_alias = value_type
  type kind_alias = kind
  type term = (typ option) lang_term
  type typed_term = typ lang_term

  module TypeChecker
           (Module_syntax: MODULE_SYNTAX
            with type value_type = value_type
            with type typ = typ
            with type kind = kind
            with type term = term
            with module Name = Name
            with module Loc = Loc
            with module Ident = Ident
            with module Path = Path)
           (Env: ENV
            with type ident = Ident.t
            with type path = Path.t
            with type name = Name.t
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
    type type_error = unit

    (* Typing functions *)
    let type_term = Env.t -> term -> (value_type, type_error) result
    let kind_deftype = Env.t -> typ -> (kind, type_error) result
    let check_valtype = Env.t -> value_type -> (value_type, type_error) result

    (* Type matching functions *)
    let valtype_match : Env.t -> value_type -> value_type -> bool
    let deftype_equiv : Env.t -> kind -> typ -> typ -> bool
    let kind_match : Env.t -> kind -> kind -> bool
    let deftype_of_path : Path.t -> kind -> typ
  end
end

module ModularizedLang = Module.Modularize(Lang)
