
open Location

module type IDENT = sig
  type t
  type name
  type loc

  val create : name -> loc -> t
  val loc : t -> loc
  val name : t -> name
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
  val debug_string : t -> string
end

module type LOC = sig
  type t
  type source

  val merge : t -> t -> t
  val sources : t -> source list
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val debug_string : t -> string
end

module type NAME = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val debug_string : t -> string
end

module type PATH = sig
  type t
  type loc
  type ident
  type name

  val from_ident : ident -> t
  val create : ident -> name list -> t
  val create_ns : name -> ident -> name list -> t
  val namespace : t -> name option
  val ident : t -> ident
  val loc : t -> loc option
  val with_loc : t -> loc -> t
  val without_loc : t -> t
  val with_local : t -> t -> t
  val without_local : t -> t
  val plus : t -> name -> t
  val subst : t -> (ident * t) -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val debug_string : t -> string
end

module Make_ident(Name: NAME)(Loc: LOC): IDENT
       with type name = Name.t
       with type loc = Loc.t = struct
  let curr_timestamp = ref 0
  type name = Name.t
  type loc = Loc.t
  type t = { timestamp: int; name: name; loc: loc }

  let create name loc =
    let timestamp = !curr_timestamp in
    curr_timestamp := !curr_timestamp + 1;
    { timestamp; name; loc }

  let loc ident = ident.loc
  let name ident = ident.name
  let compare id1 id2 = Int.compare id1.timestamp id2.timestamp
  let equal id1 id2 = id1.timestamp = id2.timestamp
  let to_string ident = Name.to_string ident.name
  let debug_string ident =
    Printf.sprintf "{ timestamp = %d; name = %s; loc = %s }"
      ident.timestamp
      (Name.debug_string ident.name)
      (Loc.debug_string ident.loc)
end

module Make_path(Ident: IDENT)
         (Name: NAME with type t = Ident.name)
         (Loc: LOC with type t = Ident.loc): PATH
       with type name = Name.t
       with type name = Ident.name
       with type ident = Ident.t = struct
  type ident = Ident.t
  type name = Name.t
  type loc = Ident.loc

  type t = {
      namespace: Name.t option;
      ident: Ident.t;
      parts: name array;
      loc: loc option;
      local: t option
  }

  let has_ns path =
    match path.namespace with
    | Some _ -> true
    | None -> false

  let has_loc path =
    match path.loc with
    | Some _ -> true
    | None -> false

  let from_ident ident = {
      namespace = None;
      ident = ident;
      parts = [||];
      loc = Some (Ident.loc ident);
      local = None
  }
                       
  let create ident names =
    let parts = Array.of_list names in
    { namespace = None; ident = ident; loc = None; parts; local = None }

  let create_ns namespace ident names =
    let intermediate = create ident names in
    { intermediate with namespace = Some namespace }
    
  let loc path = path.loc

  let ident path = path.ident

  let namespace path = path.namespace

  let plus path part =
    let { parts } = path in
    { path with parts = Array.append parts [| part |];
                loc = None}

  let with_loc path loc =
    { path with loc = Some loc }

  let without_loc path =
    { path with loc = None }

  let with_local path local =
    { path with local = Some local }

  let without_local path =
    { path with local = None }

  let subst path s =
    if has_ns path then
      path
    else
      let (ident, p) = s in
      let new_parts = Array.append p.parts path.parts in
      let new_path = { path with ident = ident; parts = new_parts } in
      let new_path = match path.loc with
        | Some l -> with_loc new_path l
        | None -> without_loc new_path in
      with_local new_path path

      let ns_equal ns1 ns2 =
        match (ns1, ns2) with
        | (Some n1, Some n2) -> Name.equal n1 n2
        | (None, None) -> true
        | _ -> false

  let ns_compare ns1 ns2 =
    match (ns1, ns2) with
    | (Some n1, Some n2) -> Name.compare n1 n2
    | (None, None) -> 0
    | (Some _, None) -> 1
    | (None, Some _) -> -1

  let equal left right =
    if (ns_equal left.namespace right.namespace)
       && Ident.equal left.ident right.ident
       && (Array.length left.parts) = (Array.length right.parts) then
      let still_equal = ref true in
      let pos = ref 0 in
      while !still_equal do
        if not (Name.equal (Array.get left.parts !pos) (Array.get right.parts !pos)) then
          still_equal := false
        else ();
        pos := !pos + 1
      done;
      !still_equal
    else
      false

  let compare left right =
    let namespace_cmp = ns_compare left.namespace right.namespace in
    if namespace_cmp = 0 then
      let ident_cmp = Ident.compare left.ident right.ident in
      if ident_cmp = 0 then
        let llen = Array.length left.parts in
        let rlen = Array.length right.parts in
        let pos = ref 0 in
        let cmp_result = ref 0 in
        while !cmp_result = 0 do
          if !pos >= llen && !pos < rlen then
            cmp_result := -1
          else if !pos >= rlen && !pos < llen then
            cmp_result := 1
          else if !pos < llen && !pos < rlen then
            cmp_result := Name.compare (Array.get left.parts !pos) (Array.get right.parts !pos)
          else ();
          pos := !pos + 1
        done;
        !cmp_result
      else
        ident_cmp
    else
      namespace_cmp

  let to_string path =
    let output = match path.namespace with
      | None -> ref ""
      | Some ns -> ref ((Name.to_string ns) ^ "/") in
    output := !output ^ (Name.to_string (Ident.name path.ident));
    Array.iter
      (fun name -> output := !output ^ "." ^ Name.to_string name)
      path.parts;
    !output

  let debug_string path =
    let output = ref "{ namespace = " in
    (match path.namespace with
     | None -> output := !output ^ "None"
     | Some ns -> output := !output ^ ("Some " ^ (Name.debug_string ns)));
    output := !output ^ ", ident = " ^ (Ident.debug_string path.ident);
    output := !output ^ ", parts = [";
    let first = ref true in
    Array.iter (fun name -> (if !first then
                      first := false
                    else
                      output := !output ^ ", ");
                    output := !output ^ (Name.debug_string name)) path.parts;
    output := !output ^ "]";
    (match path.loc with
     | Some loc -> output := !output ^ ", loc = " ^ (Loc.debug_string loc)
     | None -> ());
    output := !output ^ " }";
    !output
end

module Name: sig
  type t

  val input : string -> t
  val internal : string -> t

  val is_input : t -> bool
  val is_internal : t -> bool

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val debug_string : t -> string
end = struct
  type t = { tag: int; name: string }
  type tag = Input | Internal

  let input_tag = 0
  let current_internal_tag = ref (input_tag + 1)

  let input name = { tag = input_tag; name }
  let internal name =
    let tag = !current_internal_tag in
    current_internal_tag := !current_internal_tag + 1;
    { tag; name }

  let is_input name = name.tag = input_tag
  let is_internal name = name.tag != input_tag

  let equal n1 n2 =
    n1.tag = n2.tag
    && String.equal n1.name n2.name

  let compare n1 n2 =
    let tag_cmp = Int.compare n1.tag n2.tag in
    if tag_cmp = 0 then
      String.compare n1.name n2.name
    else
      tag_cmp

  let to_string name =
    if name.tag = input_tag then
      name.name
    else
      Printf.sprintf "%s%%%d" name.name name.tag

  let debug_string name = Printf.sprintf "{ tag = %d; name = %s }" name.tag name.name
end

module Ident = Make_ident(Name)(Span)
module Path = Make_path(Ident)(Name)(Span)

module type SUBST = sig
  type t
  type ident
  type path

  val identity : t
  val plus : t -> ident -> path -> t
  val path : t -> path -> path
  val to_string : t -> string
  val debug_string : t -> string
end

module Make_subst(Ident: IDENT)
         (Path: PATH
          with type ident = Ident.t
          with type name = Ident.name): SUBST
       with type ident = Ident.t
       with type path = Path.t = struct
  module IdentMap = Map.Make(Ident)

  type ident = Ident.t
  type path = Path.t
  type t = Path.t IdentMap.t

  let identity = IdentMap.empty
  let plus map ident path = IdentMap.add ident path map
  let path map path =
    let ident = Path.ident path in
    match IdentMap.find_opt ident map with
    | Some p -> Path.subst path (ident, p)
    | None -> path

  let to_string subst =
    let output = ref "{ " in
    let first = ref true in
    Seq.iter
      (fun entry ->
        (if !first then
           first := false
         else
           output := !output ^ ", ");
        let (ident, path) = entry in
        output := !output ^ (Ident.to_string ident) ^ " -> " ^ (Path.to_string path))
      (IdentMap.to_seq subst);
    !output ^ " }"

  let debug_string subst =
    let output = ref "{ " in
    let first = ref true in
    Seq.iter
      (fun entry ->
        (if !first then
           first := false
         else
           output := !output ^ ", ");
        let (ident, path) = entry in
        output := !output ^ (Ident.debug_string ident) ^ " -> " ^ (Path.debug_string path))
      (IdentMap.to_seq subst);
    !output ^ " }"
end

module Subst = Make_subst(Ident)(Path)

module Source = struct
  type type_decl = unit
  type type_def = unit
  type value_type = unit
  type module_decl = unit
  type term = unit

  type import_entry =
    | Name of Loc.t * Name.t
    | Rename of (Loc.t * Name.t) * (Loc.t * Name.t)

  type import_line =
    | Type_import of Loc.t * import_entry list
    | Value_import of Loc.t * import_entry list
    | Module_type_import of Loc.t * import_entry list
    | Module_import of Loc.t * import_entry list

  type open_mask = import_line list

  type signature_entry = [
    | `Type_decl of Ident.t * type_decl
    | `Value_type of Ident.t * value_type
    | `Module_type of Ident.t * module_type
    | `Module_decl of Ident.t * module_decl
    | `Module_open of module_term * open_mask option
  ]
  and signature = signature_entry list
  and module_type =
    | Signature of Loc.t * signature
    | Functor_type of Loc.t * Ident.t * module_type * module_type
  and module_entry = [
    | `Type_def of Ident.t * type_def
    | `Term of Ident.t * term
    | `Module_type of Ident.t * module_type
    | `Module_term of Ident.t * module_term
    | `Module_open of module_term * open_mask option
  ]
  and module_term = [
    | `Path of Path.t
    | `Structure of Loc.t * structure
    | `Functor of Ident.t * module_type * module_term
    | `Apply of module_term * module_term
    | `Constraint of module_term * module_term
  ]
  and structure = module_entry list
end

module type SIGNATURE = sig
  type t
  type name
  type ident
  type type_decl
  type value_type
  type module_type
  type module_decl
  type module_term

  val lookup_type : t -> ident -> type_decl option
  val lookup_type_name : t -> name -> type_decl option
  val lookup_value_type : t -> ident -> value_type option
  val lookup_value_type_name : t -> name -> value_type option
  val lookup_module_type : t -> ident -> module_type option
  val lookup_module_type_name : t -> name -> module_type option
  val lookup_module_decl : t -> ident -> module_decl option
  val lookup_module_decl_name : t -> name -> module_decl option

  val type_seq : t -> (ident * type_decl) Seq.t
  val value_type_seq : t -> (ident * value_type) Seq.t
  val module_type_seq : t -> (ident * module_type) Seq.t
  val module_decl_seq : t -> (ident * module_decl) Seq.t
end

module type STRUCTURE = sig
  type 'term t
  type name
  type ident
  type type_def
  type 'term module_term
  type module_type
     
  val empty : 'term t
  val plus_type : 'term t -> ident -> type_def -> ('term t, ident) result
  val plus_term : 'term t -> ident -> 'term -> ('term t, ident) result
  val plus_module : 'term t -> ident -> 'term module_term -> ('term t, ident) result
  val plus_module_type : 'term t -> ident -> module_type -> ('term t, ident) result

  val lookup_type : 'term t -> ident -> type_def option
  val lookup_type_name : 'term t -> name -> type_def option
  val lookup_term : 'term t -> ident -> 'term option
  val lookup_term_name : 'term t -> name -> 'term option
  val lookup_module : 'term t -> ident -> ('term module_term) option
  val lookup_module_name : 'term t -> name -> ('term module_term) option
  val lookup_module_type : 'term t -> ident -> module_type option
  val lookup_module_type_name : 'term t -> name -> module_type option

  val type_seq : 'term t -> (ident * type_def) Seq.t
  val term_seq : 'term t -> (ident * 'term) Seq.t
  val module_seq : 'term t -> (ident * ('term module_term)) Seq.t
  val module_type_seq : 'term t -> (ident * module_type) Seq.t
end

module type NAMESPACE_DECLARATION = sig
  type t
  type loc
  type name
  type ident
  type module_type
  type module_term

  type export_signifier =
    | Module of loc * name
    | Signature of loc * name

  type export_header =
    | Export of export_signifier list
    | Hide of export_signifier list
    | Export_all
    | Hide_all

  val empty : ident -> export_header -> t

  val ident : t -> ident
  val name : t -> name

  val export_header : t -> export_header

  val plus_sig : t -> ident -> module_type -> (t, ident) result
  val plus_term : t -> ident -> module_term -> (t, ident) result

  val lookup_sig : t -> ident -> module_type option
  val lookup_sig_name : t -> name -> module_type option
  val lookup_term : t -> ident -> module_term option
  val lookup_term_name : t -> name -> module_term option

  val sig_seq : t -> (ident * module_type) Seq.t
  val term_seq : t -> (ident * module_term) Seq.t
end

module type NAMESPACE = sig
  type 'term t
  type name
  type ident
  type term
  type module_type
  type 'term module_term
  type namespace_declaration
  type error

  val name : 'term t -> name
  val idents : 'term t -> ident Seq.t

  val create : name -> namespace_declaration Seq.t -> (term t, error list) result

  val sig_exports : 'term t -> name Seq.t
  val term_exports : 'term t -> name Seq.t

  val lookup_sig : 'term t -> ident -> module_type option
  val lookup_sig_name : 'term t -> name -> module_type option
  val lookup_term : 'term t -> ident -> ('term module_term) option
  val lookup_term_name : 'term t -> name -> ('term module_term) option

  val sig_seq : 'term t -> (ident * module_type) Seq.t
  val term_seq : 'term t -> (ident * ('term module_term)) Seq.t
end

module type ENV = sig
  type t
  type ident
  type path
  type name
  type type_decl
  type value_type
  type module_type

  (* val current_path : t -> path option
   * val with_current_path : t -> path option -> t
   * val plus_current_path : t -> name -> t *)

  val find_type : t -> path -> type_decl option
  val find_value_type : t -> path -> value_type option
  val find_module_type : t -> path -> module_type option

  (* val find_type_def : t -> path -> type_def option
   * val find_term : t -> path -> term option
   * val find_module_term : t -> path -> module_term option *)
end
                                  
module type NAMESPACES = sig
  type 'term t
  type name
  type ident
  type term
  type typed_term
  type value_type
  type type_decl
  type 'term namespace
  type module_type
  type 'term module_term
  type env

  module Env : ENV
         with type t = env
         with type ident = ident
         with type name = name
         with type type_decl = type_decl
         with type value_type = value_type
         with type module_type = module_type
         
  val create : ('term namespace) list -> 'term t

  val lookup_namespace : 'term t -> name -> ('term namespace) option

  val to_seq : 'term t -> ('term namespace) Seq.t

  val env : 'term t -> env
end

module type SOURCES = sig
  type 'term t
  type source_id
  type 'term source
  type 'term namespaces

  val empty : 'term t

  val plus : 'term t -> 'term source -> 'term t

  val lookup_source : 'term t -> source_id -> ('term source) option
  val namespaces : 'term t -> ('term namespaces)

  val to_seq : 'term t -> ('term source) Seq.t
end

module type SOURCE_PARSER = sig
  type source_id
  type source_input
  type term
  type 'term source
  type parse_error

  val parse : source_id -> source_input -> (term source, parse_error) result
end

module type SOURCES_PARSER = sig
  type t
  type term
  type source_id
  type source_input
  type 'term source
  type parse_error
     
  val parse : source_id list -> (t, parse_error) result

  (*           sources           changed sources   result *)
  val reload : source_id list -> source_id list -> (t, parse_error) result
end

(*
module type MODULE_SYNTAX = sig
  module Name : NAME
  module Loc : LOC
  module Ident : IDENT
         with type name = Name.t
         with type loc = Loc.t
  module Path : PATH
         with type ident = Ident.t
         with type name = Ident.name
         with type name = Name.t
  module SourceId : NAME

  type value_type
  type typ
  type kind
  type term
  type typed_term

  type type_decl = {
      kind: kind;
      concrete: typ option
  }

  type type_def = {
      def_kind: kind;
      concrete_def: typ
  }

  type module_decl

  type signature
  type module_type = Signature of signature
                   | Functor_type of Ident.t * module_type * module_type

  type 'term structure
  type 'term module_term = Path of Path.t
                         | Structure of ('term structure)
                         | Functor of Ident.t * module_type * ('term module_term)
                         | Apply of ('term module_term) * ('term module_term)
                         | Constraint of ('term module_term) * module_type

  module Signature : SIGNATURE
         with type t = signature
         with type name = Name.t
         with type ident = Ident.t
         with type type_decl = type_decl
         with type value_type = value_type
         with type module_type = module_type
         with type module_decl = module_decl

  module Structure : STRUCTURE
         with type 'term t = 'term structure
         with type name = Name.t
         with type ident = Ident.t
         with type type_def = type_def
         with type 'term module_term = 'term module_term
         with type module_type = module_type

  module NamespaceDeclaration : NAMESPACE_DECLARATION
         with type loc = Loc.t
         with type name = Name.t
         with type ident = Ident.t
         with type module_type = module_type
         with type module_term = term module_term

  module Source : SOURCE
         with type source_id = SourceId.t
         with type name = Name.t
         with type ident = Ident.t
         with type term = term
         with type module_type = module_type
         with type module_term = term module_term
         with type namespace_declaration = NamespaceDeclaration.t

  module Namespace : NAMESPACE
         with type name = Name.t
         with type ident = Ident.t
         with type module_type = module_type
         with type 'term module_term = 'term module_term
         with type namespace_declaration = NamespaceDeclaration.t

  (* module Env : ENV
   *        with type ident = Ident.t
   *        with type type_decl = type_decl
   *        with type value_type = value_type
   *        with type module_type = module_type
   *        with type type_def = type_def
   *        with type term = term
   *        with type module_term = module_term
   * 
   * module Namespaces : NAMESPACES
   *        with type name = Name.t
   *        with type ident = Ident.t
   *        with type namespace = Namespace.t
   *        with type module_type = module_type
   *        with type module_term = module_term
   *        with type env = Env.t *)
end

module type LANG = sig
  module Name : NAME
  module SourceId : NAME
  module Loc : LOC with type source = SourceId.t
  module Ident : IDENT
         with type name = Name.t
         with type loc = Loc.t
  module Path : PATH
         with type ident = Ident.t
         with type name = Ident.name
         with type name = Name.t
  module Subst : SUBST
         with type ident = Ident.t
         with type path = Path.t

  type value_type
  type typ
  type kind
  type term
  type typed_term

  val subst_value_type: value_type -> Subst.t -> value_type
  val subst_typ: typ -> Subst.t -> typ
  val subst_kind: kind -> Subst.t -> kind

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
            with type type_decl = Module_syntax.type_decl
            with type module_type = Module_syntax.module_type
            with type value_type = value_type): sig
    type env
    type ident
    type name
    type value_type
    type typ
    type kind
    type term
    type type_decl
    type module_type
    type type_error

    (* Typing functions *)
    val type_term : Env.t -> term -> (value_type, type_error list) result
    val kind_deftype : Env.t -> typ -> (kind, type_error list) result
    val check_valtype : Env.t -> value_type -> (value_type, type_error list) result

    (* Type matching functions *)
    val valtype_match : Env.t -> value_type -> value_type -> bool
    val deftype_equiv : Env.t -> kind -> typ -> typ -> bool
    val kind_match : Env.t -> kind -> kind -> bool
    val deftype_of_path : Path.t -> kind -> typ
  end
         with type env = Env.t
         with type ident = Ident.t
         with type name = Name.t
         with type value_type = value_type
         with type typ = typ
         with type kind = kind
         with type term = term
end

module Modularize(Lang: LANG): MODULE_SYNTAX
       with type value_type = Lang.value_type
       with type typ = Lang.typ
       with type kind = Lang.kind
       with type term = Lang.term
       with type typed_term = Lang.typed_term
  = struct

  type value_type = Lang.value_type
  type typ = Lang.typ
  type kind = Lang.kind
  type term = Lang.term
  type typed_term = Lang.typed_term

  module Loc = Lang.Loc
  module Name = Lang.Name
  module Ident = Lang.Ident
  module Path = Lang.Path
  module SourceId = Lang.SourceId

  type type_decl = {
      kind: kind;
      concrete: typ option
  }

  type type_def = {
      def_kind: kind;
      concrete_def: typ
  }

  type type_decl_alias = type_decl
  type type_def_alias = type_def

  module IdentMap : sig
    type 'a t
    type ident = Ident.t
    type name = Name.t

    val empty : 'a t
    val contains_ident : 'a t -> ident -> bool
    val contains_name : 'a t -> name -> bool

    val lookup_ident : 'a t -> ident -> 'a option
    val lookup_name : 'a t -> name -> 'a option

    val plus : 'a t -> ident -> 'a -> ('a t, ident) result

    val ident_seq : 'a t -> (Ident.t * 'a) Seq.t
    val name_seq : 'a t -> (Name.t * 'a) Seq.t
  end = struct
    type ident = Ident.t
    type name = Name.t

    module IdMap = Map.Make(Ident)
    module NameMap = Map.Make(Name)
    type 'a t = {
        ident_map: 'a IdMap.t;
        name_map: 'a NameMap.t;
        idents: Ident.t NameMap.t
    }

    let empty = {
        ident_map = IdMap.empty;
        name_map = NameMap.empty;
        idents = NameMap.empty
    }

    let contains_ident map ident = IdMap.mem ident map.ident_map
    let contains_name map name = NameMap.mem name map.name_map

    let lookup_ident map ident = IdMap.find_opt ident map.ident_map
    let lookup_name map name = NameMap.find_opt name map.name_map

    let plus map ident value =
      match NameMap.find_opt (Ident.name ident) map.idents with
      | Some existing -> Error(existing)
      | None -> Ok({
                      ident_map = IdMap.add ident value map.ident_map;
                      name_map = NameMap.add (Ident.name ident) value map.name_map;
                      idents = NameMap.add (Ident.name ident) ident map.idents
                  })

    let ident_seq map = IdMap.to_seq map.ident_map
    let name_seq map = NameMap.to_seq map.name_map
  end

  type signature = {
      type_decls: type_decl IdentMap.t;
      values: value_type IdentMap.t;
      sig_module_types: module_type IdentMap.t;
      module_decls: module_decl IdentMap.t
  }
  and module_type = Signature of signature
                  | Functor_type of Ident.t * module_type * module_type
  and module_decl = module_type

  type 'term structure = {
        types: type_def IdentMap.t;
        terms: 'term IdentMap.t;
        modules: ('term module_term) IdentMap.t;
        module_types: module_type IdentMap.t
  }
  and 'term module_term = Path of Path.t
              | Structure of 'term structure
              | Functor of Ident.t * module_type * ('term module_term)
              | Apply of ('term module_term) * ('term module_term)
              | Constraint of ('term module_term) * module_type

  type module_type_alias = module_type
  type 'term module_term_alias = 'term module_term
  type module_decl_alias = module_decl

  module Signature : SIGNATURE
         with type t = signature
         with type name = Name.t
         with type ident = Ident.t
         with type type_decl = type_decl
         with type value_type = value_type
         with type module_type = module_type
         with type module_decl = module_decl = struct
    type name = Name.t
    type ident = Ident.t
    type type_decl = type_decl_alias
    type value_type = Lang.value_type
    type module_type = module_type_alias
    type module_decl = module_decl_alias
    type t = signature

    let empty = {
        type_decls = IdentMap.empty;
        values = IdentMap.empty;
        sig_module_types = IdentMap.empty;
        module_decls = IdentMap.empty
    }

    let plus_type signature ident typ =
      match IdentMap.plus signature.type_decls ident typ with
      | Ok new_tbl -> Ok { signature with type_decls = new_tbl }
      | Error old_ident -> Error old_ident

    let plus_value_type signature ident value =
      match IdentMap.plus signature.values ident value with
      | Ok new_tbl -> Ok { signature with values = new_tbl }
      | Error old_ident -> Error old_ident

    let plus_module_type signature ident module_type =
      match IdentMap.plus signature.sig_module_types ident module_type with
      | Ok new_tbl -> Ok { signature with sig_module_types = new_tbl }
      | Error old_ident -> Error old_ident

    let plus_module_decl signature ident module_decl =
      match IdentMap.plus signature.module_decls ident module_decl with
      | Ok new_tbl -> Ok { signature with module_decls = new_tbl }
      | Error old_ident -> Error old_ident

    let lookup_type signature ident = IdentMap.lookup_ident signature.type_decls ident
    let lookup_type_name signature name = IdentMap.lookup_name signature.type_decls name
    let lookup_value_type signature ident = IdentMap.lookup_ident signature.values ident
    let lookup_value_type_name signature name = IdentMap.lookup_name signature.values name
    let lookup_module_type signature ident = IdentMap.lookup_ident signature.sig_module_types ident
    let lookup_module_type_name signature name = IdentMap.lookup_name signature.sig_module_types name
    let lookup_module_decl signature ident = IdentMap.lookup_ident signature.module_decls ident
    let lookup_module_decl_name signature name = IdentMap.lookup_name signature.module_decls name

    let type_seq signature = IdentMap.ident_seq signature.type_decls
    let value_type_seq signature = IdentMap.ident_seq signature.values
    let module_type_seq signature = IdentMap.ident_seq signature.sig_module_types
    let module_decl_seq signature = IdentMap.ident_seq signature.module_decls
  end
 
  module Structure: STRUCTURE
         with type 'term t = 'term structure
         with type name = Name.t
         with type ident = Ident.t
         with type type_def = type_def
         with type 'term module_term = 'term module_term
         with type module_type = module_type = struct
    type name = Name.t
    type ident = Ident.t
    type type_def = type_def_alias
    type 'term module_term = 'term module_term_alias
    type module_type = module_type_alias

    type 'term t = 'term structure

    let empty = {
        types = IdentMap.empty;
        terms = IdentMap.empty;
        modules = IdentMap.empty;
        module_types = IdentMap.empty
    }

    let plus_type structure ident type_def =
      match IdentMap.plus structure.types ident type_def with
      | Ok new_tbl -> Ok { structure with types = new_tbl }
      | Error old_ident -> Error old_ident

    let plus_term structure ident term =
      match IdentMap.plus structure.terms ident term with
      | Ok new_tbl -> Ok { structure with terms = new_tbl }
      | Error old_ident -> Error old_ident

    let plus_module structure ident module_term =
      match IdentMap.plus structure.modules ident module_term with
      | Ok new_tbl -> Ok { structure with modules = new_tbl }
      | Error old_ident -> Error old_ident

    let plus_module_type structure ident module_type =
      match IdentMap.plus structure.module_types ident module_type with
      | Ok new_tbl -> Ok { structure with module_types = new_tbl }
      | Error old_ident -> Error old_ident

    let lookup_type structure ident = IdentMap.lookup_ident structure.types ident
    let lookup_type_name structure name = IdentMap.lookup_name structure.types name
    let lookup_term structure ident = IdentMap.lookup_ident structure.terms ident
    let lookup_term_name structure name = IdentMap.lookup_name structure.terms name
    let lookup_module structure ident = IdentMap.lookup_ident structure.modules ident
    let lookup_module_name structure name = IdentMap.lookup_name structure.modules name
    let lookup_module_type structure ident = IdentMap.lookup_ident structure.module_types ident
    let lookup_module_type_name structure name = IdentMap.lookup_name structure.module_types name

    let type_seq structure = IdentMap.ident_seq structure.types
    let term_seq structure = IdentMap.ident_seq structure.terms
    let module_seq structure = IdentMap.ident_seq structure.modules
    let module_type_seq structure = IdentMap.ident_seq structure.module_types
  end

  module NamespaceDeclaration : NAMESPACE_DECLARATION
         with type loc = Loc.t
         with type name = Name.t
         with type ident = Ident.t
         with type module_type = module_type
         with type module_term = term module_term = struct
    type loc = Loc.t
    type name = Name.t
    type ident = Ident.t
    type module_type = module_type_alias
    type module_term = Lang.term module_term_alias

    type export_signifier =
      | Module of loc * Name.t
      | Signature of loc * Name.t

    type export_header =
      | Export of export_signifier list
      | Hide of export_signifier list
      | Export_all
      | Hide_all

    module NameSet = Set.Make(Name)

    type t = {
        ident: Ident.t;
        export_header: export_header;
        types: module_type IdentMap.t;
        terms: module_term IdentMap.t
    }

    let empty ident export_header = {
        ident = ident;
        export_header: export_header;
        types = IdentMap.empty;
        terms = IdentMap.empty
    }

    let ident ns = ns.ident
    let name ns = Ident.name ns.ident

    let export_header ns = ns.export_header

    let plus_sig ns ident module_type =
      match IdentMap.plus ns.types ident module_type with
      | Ok new_tbl -> Ok { ns with types = new_tbl }
      | Error old_ident -> Error old_ident

    let plus_term ns ident module_term =
      match IdentMap.plus ns.terms ident module_term with
      | Ok new_tbl -> Ok { ns with terms = new_tbl }
      | Error old_ident -> Error old_ident

    let lookup_sig ns ident = IdentMap.lookup_ident ns.types ident
    let lookup_sig_name ns name = IdentMap.lookup_name ns.types name
    let lookup_term ns ident = IdentMap.lookup_ident ns.terms ident
    let lookup_term_name ns name = IdentMap.lookup_name ns.terms name

    let sig_seq ns = IdentMap.ident_seq ns.types
    let term_seq ns = IdentMap.ident_seq ns.terms
  end

  type ns_merge_error = [
    | `Duplicate_signature of Ident.t * Ident.t
    | `Duplicate_module of Ident.t * Ident.t
  ]

  module Namespace : NAMESPACE
         with type name = Name.t
         with type ident = Ident.t
         with type term = term
         with type module_type = module_type
         with type 'term module_term = 'term module_term
         with type namespace_declaration = NamespaceDeclaration.t
         with type error = ns_merge_error = struct
    type name = Name.t
    type ident = Ident.t
    type term = Lang.term
    type module_type = module_type_alias
    type 'term module_term = 'term module_term_alias
    type namespace_declaration = NamespaceDeclaration.t
    type error = ns_merge_error

    module NameSet = Set.Make(Name)
    module IdentSet = Set.Make(Ident)
    module NsDecl = NamespaceDeclaration

    type 'term t = {
        name: Name.t;
        idents: IdentSet.t;
        type_exports: NameSet.t;
        module_exports: NameSet.t;
        types: module_type IdentMap.t;
        terms: ('term module_term) IdentMap.t
    }

    let name ns = ns.name
    let idents ns = IdentSet.to_seq ns.idents

    let plus_decl types terms type_exports mod_exports errors decl =
      let type_names = ref NameSet.empty in
      let mod_names = ref NameSet.empty in
      let plus_type mod_type =
        let (ident, mt) = mod_type in
        match IdentMap.plus !types ident mt with
        | Ok new_tbl ->
           types := new_tbl;
           type_names := NameSet.add (Ident.name ident) !type_names
        | Error old_ident -> errors := (`Duplicate_signature (old_ident, ident)) :: !errors
      in
      let plus_term mod_term =
        let (ident, mt) = mod_term in
        match IdentMap.plus !terms ident mt with
        | Ok new_tbl ->
           terms := new_tbl;
           mod_names := NameSet.add (Ident.name ident) !mod_names
        | Error old_ident -> errors :=  (`Duplicate_module (old_ident, ident)) :: !errors
      in
      let plus_export_header type_names mod_names export_header =
        match export_header with
        | NsDecl.Export exports ->
           List.iter (function
               | NsDecl.Module (loc, name) -> mod_exports := NameSet.add name !mod_exports
               | NsDecl.Signature (loc, name) -> type_exports := NameSet.add name !type_exports)
             exports
        | NsDecl.Hide hidden ->
           let hidden_types = ref NameSet.empty in
           let hidden_mods = ref NameSet.empty in
           List.iter (function
               | NsDecl.Module (loc, name) -> hidden_mods := NameSet.add name !hidden_mods
               | NsDecl.Signature (loc, name) -> hidden_types := NameSet.add name !hidden_types)
             hidden;
           let types = NameSet.diff type_names !hidden_types in
           let mods = NameSet.diff mod_names !hidden_mods in
           type_exports := types;
           mod_exports := mods
        | NsDecl.Export_all ->
           mod_exports := mod_names;
           type_exports := type_names
        | NsDecl.Hide_all ->
           mod_exports := NameSet.empty;
           type_exports := NameSet.empty
      in
      Seq.iter plus_type @@ NsDecl.sig_seq decl;
      Seq.iter plus_term @@ NsDecl.term_seq decl;
      plus_export_header !type_names !mod_names (NsDecl.export_header decl)

    let create name decls =
      let errors = ref [] in
      let idents = ref IdentSet.empty in
      let types = ref IdentMap.empty in
      let terms = ref IdentMap.empty in
      let type_exports = ref NameSet.empty in
      let module_exports = ref NameSet.empty in
      let update decl =
        let ident = NamespaceDeclaration.ident decl in
        idents := IdentSet.add ident !idents;
        plus_decl types terms type_exports module_exports errors decl in
      Seq.iter update decls;
      match !errors with
      | [] -> Ok {
          name = name;
          idents = !idents;
          type_exports = !type_exports;
          module_exports = !module_exports;
          types = !types;
          terms = !terms
        }
      | _ -> Error !errors

    let sig_exports ns = NameSet.to_seq ns.type_exports
    let term_exports ns = NameSet.to_seq ns.module_exports

    let lookup_sig ns ident = IdentMap.lookup_ident ns.types ident
    let lookup_sig_name ns name = IdentMap.lookup_name ns.types name
    let lookup_term ns ident = IdentMap.lookup_ident ns.terms ident
    let lookup_term_name ns name = IdentMap.lookup_name ns.terms name

    let sig_seq ns = IdentMap.ident_seq ns.types
    let term_seq ns = IdentMap.ident_seq ns.terms
  end

  module NameMap = Map.Make(Name)

  module Env = struct

    type t = {
        current_path: Path.t option;
        namespace_remappings: Name.t NameMap.t;
        namespaces: Name.t NameMap.t;
        value_types: value_type NameMap.t;
        type_decls: type_decl NameMap.t;
        module_types: module_type NameMap.t;
        type_defs: type_def NameMap.t;
        terms: term NameMap.t;
        module_terms: (Lang.term module_term) NameMap.t;
        ident_paths: Path.t IdentMap.t
    }

    (* let plus_value_type env name value =
     *   { env with value_types = NameMap.add name value env.value_types }
     * 
     * let plus_type_decl env name value =
     *   { env with type_decls = NameMap.add name value env.type_decls }
     * 
     * let plus_module_type env name value =
     *   { env with module_types = NameMap.add name value env.module_types }
     * 
     * let plus_module_term env name value =
     *   { env with module_terms = NameMap.add name value env.module_terms }
     * 
     * let plus_type_def env name value =
     *   { env with type_defs = NameMap.add name value env.type_defs }
     * 
     * let lookup_value_type env name = NameMap.find_opt name env.value_types
     * let lookup_type_decl env name = NameMap.find_opt name env.type_decls
     * let lookup_module_type env name = NameMap.find_opt name env.module_types
     * let lookup_module_term env name = NameMap.find_opt name env.module_terms
     * 
     * let curr env name =
     *   match env.current_path with
     *   | Some p -> Path.plus p name
     *   | None -> Path.create [name]
     * 
     * let plus env_ref cont seqfn add =
     *   Seq.iter (fun v -> begin
     *                 let (ident, value) = v in
     *                 let name = Ident.name ident in
     *                 env_ref := add !env_ref name value
     *               end)
     *     (seqfn cont)
     * 
     * let plus_module env module_term =
     *   let structure = match module_term with
     *     | Structure structure -> structure in
     *   let env_ref = ref env in
     *   plus env_ref structure Structure.type_seq plus_type_def;
     *   plus env_ref structure Structure.value_type_seq plus_value_type;
     *   plus env_ref structure Structure.module_type_seq plus_module_type;
     *   plus env_ref structure Structure.module_decl_seq plus_module_decl;
     *   !env_ref
     * 
     * let rec find_module_nested env names =
     *   match names with
     *   | [] -> raise (Failure "find_module_nested called with empty list")
     *   | [name] -> begin
     *       match NameMap.find_opt name env.module_terms with
     *       | Some module_term -> Ok module_term
     *       | None -> Error (`Unbound_module (curr env name))
     *     end
     *   | name :: rest -> begin
     *       match find_module_nested env [name] with
     *       | Ok found_module ->
     *          let new_env = plus_module env found_module in
     *          find_module_nested new_env rest
     *       | err -> err
     *     end *)

    (* let find_type_decl_nested env names =
     *   match names with
     *   | [] -> raise (Failure "find_type_decl_nested called with empty list")
     *   | [name] -> begin
     *       match NameMap.find_opt name env.type_decls with
     *       | Some type_decl -> Ok type_decl
     *       | None -> Error (`Unbound_type_decl (curr env name))
     *     end
     *   | name :: rest -> begin
     *       match
     *     end *)
  end

  module Source : SOURCE
         with type source_id = SourceId.t
         with type name = Name.t
         with type ident = Ident.t
         with type term = Lang.term
         with type module_type = module_type
         with type module_term = Lang.term module_term
         with type namespace_declaration = NamespaceDeclaration.t = struct
    type source_id = SourceId.t
    type name = Name.t
    type ident = Ident.t
    type term = Lang.term
    type module_type = module_type_alias
    type module_term = Lang.term module_term_alias
    type namespace_declaration = NamespaceDeclaration.t

    module IdMap = Map.Make(Ident)

    type t = {
        name : source_id;
        declarations : namespace_declaration IdMap.t
    }

    let empty source_id = {
        name = source_id;
        declarations = IdMap.empty
    }

    let name source = source.name
    let namespaces source =
      let names = List.map (fun b -> let (i, v) = b in Ident.name i)
                    (IdMap.bindings source.declarations) in
      List.sort_uniq Name.compare names

    let plus source decl =
      let ident = (NamespaceDeclaration.ident decl) in
      { source with declarations = IdMap.add ident decl source.declarations } 
      
    let plus_all source decls =
      let add map decl = IdMap.add (NamespaceDeclaration.ident decl) decl map in
      let new_decls = List.fold_left add source.declarations decls in
      { source with declarations = new_decls }

    let lookup_namespace_declaration source ident =
      IdMap.find_opt ident source.declarations

    let lookup_namespace_declarations source name =
      let matches_name pair =
        let (id, value) = pair in
        Name.equal (Ident.name id) name in
      let second pair =
        let (first, second) = pair in
        second in
      Seq.map second @@ Seq.filter matches_name @@ IdMap.to_seq source.declarations

    let namespace_declaration_seq source = IdMap.to_seq source.declarations
  end

  (* module Namespaces : NAMESPACES
   *        with type name = Name.t
   *        with type ident = Ident.t
   *        with type namespace = Namespace.t
   *        with type module_type = module_type_alias
   *        with type module_term = module_term_alias
   *        with type env = Env.t = struct
   *   type name = Name.t
   *   type ident = Ident.t
   *   type namespace = Namespace.t
   *   type module_type = module_type_alias
   *   type module_term = module_term_alias
   * 
   *   module NameMap = Map.Make(Name)
   *   module IdentMap = Map.Make(Ident)
   * 
   *   type t = namespace NameMap.t
   * 
   *   type env = {
   *       namespaces: t;
   *       current_path: Path.t option;
   *       namespace_remappings: Name.t NameMap.t;
   *       value_types: Path.t NameMap.t;
   *       type_decls: Path.t NameMap.t;
   *       module_types: Path.t NameMap.t;
   *       type_defs: Path.t NameMap.t;
   *       terms: Path.t NameMap.t;
   *       module_terms: Path.t NameMap.t;
   *       ident_paths: Path.t IdentMap.t
   *   }
   * 
   *   module Env : ENV
   *          with type t = env
   *          with type ident = Ident.t
   *          with type path = Path.t
   *          with type name = Name.t
   *          with type type_decl = type_decl
   *          with type value_type = value_type
   *          with type module_type = module_type_alias
   *          with type type_def = type_def
   *          with type term = term
   *          with type module_term = module_term_alias = struct
   *     type t = env
   *     type path = Path.t
   *     type ident = Ident.t
   *     type name = Name.t
   *     type type_decl = type_decl_alias
   *     type value_type = Lang.value_type
   *     type module_type = module_type_alias
   *     type type_def = type_def_alias
   *     type term = Lang.term
   *     type module_term = module_term_alias
   * 
   *     let current_path env = env.current_path
   * 
   *     let with_current_path env path =
   *       { env with current_path = path }
   * 
   *     let plus_current_path env name =
   *       match current_path env with
   *       | Some path -> with_current_path env (Some (Path.plus path name))
   *       | None -> env
   * 
   *     let find_value_type env path =
   *       match NameMap.find_opt  env.value_types with
   *         
   * 
   *     let find_type_decl env path = ()
   *     let find_module_type env path = ()
   * 
   *     let find_type_def env path = ()
   *     let find_term env path = ()
   *     let find_module_term env path = ()
   * 
   *   end
   * 
   * end *)
end
 *)
