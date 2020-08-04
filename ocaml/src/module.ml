
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
  type ident
  type name

  val from_ident : ident -> t
  val create : ident -> name list -> t
  val create_ns : ident -> ident -> name list -> t
  val namespace : t -> ident option
  val plus : t -> name -> t
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

module Make_path(Ident: IDENT)(Name: NAME with type t = Ident.name): (
  PATH with type name = Name.t
  with type name = Ident.name
  with type ident = Ident.t
) = struct
  type ident = Ident.t
  type name = Name.t

  type t = {
      namespace: Ident.t option;
      ident: Ident.t;
      parts: name array
  }

  let from_ident ident = { namespace = None; ident = ident; parts = [||] }

  let create ident names =
    let parts = Array.of_list names in
    { namespace = None; ident; parts }

  let create_ns namespace ident names =
    let intermediate = create ident names in
    { intermediate with namespace = Some namespace }
    
  let namespace path = path.namespace

  let plus path part =
    let { namespace; ident; parts } = path in
    { namespace; ident; parts = Array.append parts [| part |] }

  let equal left right =
    if Ident.equal left.ident right.ident
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

  let to_string path =
    let output = match path.namespace with
      | None -> ref ""
      | Some ns -> ref ((Ident.to_string ns) ^ "/") in 
    output := !output ^ (Ident.to_string path.ident);
    Array.iter (fun name -> output := !output ^ "." ^ Name.to_string name) path.parts;
    !output

  let debug_string path =
    let output = ref "{ namespace = " in
    (match path.namespace with
     | None -> output := !output ^ "None"
     | Some ns -> output := !output ^ ("Some " ^ (Ident.debug_string ns)));
    output := !output ^ (", ident = " ^ (Ident.debug_string path.ident));
    output := !output ^ ", parts = [";
    Array.iter (fun name -> output := !output ^ ", " ^ Name.debug_string name) path.parts;
    output := !output ^ "] }";
    !output
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

  type value_type
  type typ
  type kind
  type term

  val kind : typ -> kind
  val kind_match : typ -> kind -> bool
  val kind_equal : kind -> kind -> bool
end

module type SIGNATURE = sig
  type ('t, 'v, 'm) t
  type name
  type ident
  val empty : ('t, 'v, 'm) t
  val plus_type : ('t, 'v, 'm) t -> ident -> 't -> (('t, 'v, 'm) t, ident) result
  val plus_value_type : ('t, 'v, 'm) t -> ident -> 'v -> (('t, 'v, 'm) t, ident) result
  val plus_module_type : ('t, 'v, 'm) t -> ident -> 'm -> (('t, 'v, 'm) t, ident) result

  val lookup_type : ('t, 'v, 'm) t -> ident -> 't option
  val lookup_type_name : ('t, 'v, 'm) t -> name -> 't option
  val lookup_value_type : ('t, 'v, 'm) t -> ident -> 'v option
  val lookup_value_type_name : ('t, 'v, 'm) t -> name -> 'v option
  val lookup_module_type : ('t, 'v, 'm) t -> ident -> 'm option
  val lookup_module_type_name : ('t, 'v, 'm) t -> name -> 'm option

  val type_seq : ('t, 'v, 'm) t -> (ident * 't) Seq.t
  val value_type_seq : ('t, 'v, 'm) t -> (ident * 'v) Seq.t
  val module_type_seq : ('t, 'v, 'm) t -> (ident * 'm) Seq.t
end

module type STRUCTURE = sig
  type ('type_def, 'term, 'module_term) t
  type name
  type ident
     
  val empty : ('d, 't, 'm) t
  val plus_type : ('d, 't, 'm) t -> ident -> 'd -> (('d, 't, 'm) t, ident) result
  val plus_term : ('d, 't, 'm) t -> ident -> 't -> (('d, 't, 'm) t, ident) result
  val plus_module : ('d, 't, 'm) t -> ident -> 'm -> (('d, 't, 'm) t, ident) result

  val lookup_type : ('d, 't, 'm) t -> ident -> 'd option
  val lookup_type_name : ('d, 't, 'm) t -> name -> 'd option
  val lookup_term : ('d, 't, 'm) t -> ident -> 't option
  val lookup_term_name : ('d, 't, 'm) t -> name -> 't option
  val lookup_module : ('d, 't, 'm) t -> ident -> 'm option
  val lookup_module_name : ('d, 't, 'm) t -> name -> 'm option

  val type_seq : ('d, 't, 'm) t -> (ident * 'd) Seq.t
  val term_seq : ('d, 't, 'm) t -> (ident * 't) Seq.t
  val module_seq : ('d, 't, 'm) t -> (ident * 'm) Seq.t
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

module type SOURCE = sig
  type t
  type source_id
  type name
  type ident
  type module_type
  type module_term
  type namespace_declaration

  val empty : source_id -> t

  val name: t -> source_id
  val namespaces : t -> name list

  val plus : t -> namespace_declaration -> t
  val plus_all : t -> namespace_declaration list -> t

  val lookup_namespace_declaration : t -> ident -> namespace_declaration option
  val lookup_namespace_declarations : t -> name -> namespace_declaration Seq.t

  val namespace_declaration_seq : t -> (ident * namespace_declaration) Seq.t
end

module type NAMESPACE = sig
  type t
  type name
  type ident
  type module_type
  type module_term
  type namespace_declaration
  type error

  val name : t -> name
  val idents : t -> ident Seq.t

  val create : name -> namespace_declaration Seq.t -> (t, error list) result

  val sig_exports : t -> name Seq.t
  val term_exports : t -> name Seq.t

  val lookup_sig : t -> ident -> module_type option
  val lookup_sig_name : t -> name -> module_type option
  val lookup_term : t -> ident -> module_term option
  val lookup_term_name : t -> name -> module_term option

  val sig_seq : t -> (ident * module_type) Seq.t
  val term_seq : t -> (ident * module_term) Seq.t
end

module type ENV = sig
  type t
  type ident
  type path
  type type_decl
  type value_type
  type module_type
  type type_def
  type term
  type module_term

  val empty: t
  val plus_value_type : t -> path -> value_type -> t
  val plus_type_decl : t -> path -> type_decl -> t 
  val plus_module_type : t -> path -> module_type -> t

  val plus_type_def : t -> path -> type_def -> t
  val plus_term : t -> path -> term -> t
  val plus_module_term : t -> path -> module_term -> t

  val find_value_type : t -> path -> value_type option
  val find_type_decl : t -> path -> type_decl option
  val find_module_type : t -> path -> module_type option

  val find_type_def : t -> path -> type_def option
  val find_term : t -> path -> term option
  val find_module_term : t -> path -> module_term option
end
                                  
module type NAMESPACES = sig
  type t
  type name
  type ident
  type source
  type namespace
  type module_type
  type module_term
  type env

  val create : namespace list -> t

  val lookup_namespace : t -> name -> namespace option

  val to_seq : t -> namespace Seq.t

  val env : t -> env
end

module type SOURCES = sig
  type t
  type source_id
  type source
  type namespaces

  val empty : t

  val plus : t -> source -> t

  val lookup_source : t -> source_id -> source option
  val namespaces : t -> namespaces

  val to_seq : t -> source Seq.t
end

module type SOURCE_PARSER = sig
  type source_id
  type source_input
  type source
  type parse_error

  val parse : source_id -> source_input -> (source, parse_error) result
end

module type SOURCES_PARSER = sig
  type t
  type source_id
  type source_input
  type source
  type parse_error
     
  val parse : source_id list -> (t, parse_error) result

  (*           sources           changed sources   result *)
  val reload : source_id list -> source_id list -> (t, parse_error) result
end

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

  type type_decl = {
      kind: kind;
      concrete: typ option
  }

  type type_def = {
      kind: kind;
      concrete: typ
  }

  module Signature : SIGNATURE
         with type name = Name.t
         with type ident = Ident.t

  module Structure : STRUCTURE
         with type name = Name.t
         with type ident = Ident.t

  type signature = (type_decl, value_type, module_type) Signature.t
  and module_type = Signature of signature
                  | Functor_type of module_type * module_type

  type structure = (type_def, term, module_term) Structure.t
  and module_term = Path of Path.t
              | Structure of structure
              | Functor of Ident.t * module_type * module_term
              | Apply of module_term * module_term
              | Constraint of module_term * module_type

  module NamespaceDeclaration : NAMESPACE_DECLARATION
         with type loc = Loc.t
         with type name = Name.t
         with type ident = Ident.t
         with type module_type = module_type
         with type module_term = module_term

  module Source : SOURCE
         with type source_id = SourceId.t
         with type name = Name.t
         with type ident = Ident.t
         with type module_type = module_type
         with type module_term = module_term
         with type namespace_declaration = NamespaceDeclaration.t

  module Namespace : NAMESPACE
         with type name = Name.t
         with type ident = Ident.t
         with type module_type = module_type
         with type module_term = module_term
         with type namespace_declaration = NamespaceDeclaration.t
end

module Modularize(Lang: LANG): MODULE_SYNTAX
       with type value_type = Lang.value_type
       with type typ = Lang.typ
       with type kind = Lang.kind
       with type term = Lang.term
  = struct

  type value_type = Lang.value_type
  type typ = Lang.typ
  type kind = Lang.kind
  type term = Lang.term

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
      kind: kind;
      concrete: typ
  }

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

  module Signature : SIGNATURE
         with type name = Name.t
         with type ident = Ident.t = struct
    type ('t, 'v, 'm) t = {
        types: 't IdentMap.t;
        values: 'v IdentMap.t;
        module_types: 'm IdentMap.t
      }
    type name = Name.t
    type ident = Ident.t

    let empty = {
        types = IdentMap.empty;
        values = IdentMap.empty;
        module_types = IdentMap.empty
    }

    let plus_type signature ident typ =
      match IdentMap.plus signature.types ident typ with
      | Ok new_tbl -> Ok { signature with types = new_tbl }
      | Error old_ident -> Error old_ident

    let plus_value_type signature ident value =
      match IdentMap.plus signature.values ident value with
      | Ok new_tbl -> Ok { signature with values = new_tbl }
      | Error old_ident -> Error old_ident

    let plus_module_type signature ident module_type =
      match IdentMap.plus signature.module_types ident module_type with
      | Ok new_tbl -> Ok { signature with module_types = new_tbl }
      | Error old_ident -> Error old_ident

    let lookup_type signature ident = IdentMap.lookup_ident signature.types ident
    let lookup_type_name signature name = IdentMap.lookup_name signature.types name
    let lookup_value_type signature ident = IdentMap.lookup_ident signature.values ident
    let lookup_value_type_name signature name = IdentMap.lookup_name signature.values name
    let lookup_module_type signature ident = IdentMap.lookup_ident signature.module_types ident
    let lookup_module_type_name signature name = IdentMap.lookup_name signature.module_types name

    let type_seq signature = IdentMap.ident_seq signature.types
    let value_type_seq signature = IdentMap.ident_seq signature.values
    let module_type_seq signature = IdentMap.ident_seq signature.module_types
  end
 
  module Structure: STRUCTURE
         with type name = Name.t
         with type ident = Ident.t = struct
    type ('d, 't, 'm) t = {
        types: 'd IdentMap.t;
        terms: 't IdentMap.t;
        modules: 'm IdentMap.t
    }
    type name = Name.t
    type ident = Ident.t

    let empty = {
        types = IdentMap.empty;
        terms = IdentMap.empty;
        modules = IdentMap.empty
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

    let lookup_type structure ident = IdentMap.lookup_ident structure.types ident
    let lookup_type_name structure name = IdentMap.lookup_name structure.types name
    let lookup_term structure ident = IdentMap.lookup_ident structure.terms ident
    let lookup_term_name structure name = IdentMap.lookup_name structure.terms name
    let lookup_module structure ident = IdentMap.lookup_ident structure.modules ident
    let lookup_module_name structure name = IdentMap.lookup_name structure.modules name

    let type_seq structure = IdentMap.ident_seq structure.types
    let term_seq structure = IdentMap.ident_seq structure.terms
    let module_seq structure = IdentMap.ident_seq structure.modules
  end

  type signature = (type_decl, value_type, module_type) Signature.t
  and module_type = Signature of signature
                  | Functor_type of module_type * module_type

  type structure = (type_def, term, module_term) Structure.t
  and module_term = Path of Path.t
              | Structure of structure
              | Functor of Ident.t * module_type * module_term
              | Apply of module_term * module_term
              | Constraint of module_term * module_type

  type module_type_alias = module_type
  type module_term_alias = module_term

  module NamespaceDeclaration : NAMESPACE_DECLARATION
         with type loc = Loc.t
         with type name = Name.t
         with type ident = Ident.t
         with type module_type = module_type
         with type module_term = module_term = struct
    type loc = Loc.t
    type name = Name.t
    type ident = Ident.t
    type module_type = module_type_alias
    type module_term = module_term_alias

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

  module Source : SOURCE
         with type source_id = SourceId.t
         with type name = Name.t
         with type ident = Ident.t
         with type module_type = module_type
         with type module_term = module_term
         with type namespace_declaration = NamespaceDeclaration.t = struct
    type source_id = SourceId.t
    type name = Name.t
    type ident = Ident.t
    type module_type = module_type_alias
    type module_term = module_term_alias
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

  type ns_merge_error = [
    | `Duplicate_signature of Ident.t * Ident.t
    | `Duplicate_module of Ident.t * Ident.t
  ]

  module Namespace : NAMESPACE
         with type name = Name.t
         with type ident = Ident.t
         with type module_type = module_type
         with type module_term = module_term
         with type namespace_declaration = NamespaceDeclaration.t
         with type error = ns_merge_error = struct
    type name = Name.t
    type ident = Ident.t
    type module_type = module_type_alias
    type module_term = module_term_alias
    type namespace_declaration = NamespaceDeclaration.t
    type error = ns_merge_error

    module NameSet = Set.Make(Name)
    module IdentSet = Set.Make(Ident)
    module NsDecl = NamespaceDeclaration

    type t = {
        name: Name.t;
        idents: IdentSet.t;
        type_exports: NameSet.t;
        module_exports: NameSet.t;
        types: module_type IdentMap.t;
        terms: module_term IdentMap.t
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
end
