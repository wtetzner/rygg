
open Location

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

module Ident = Module.Make_ident(Name)(Span)
module Path = Module.Make_path(Ident)(Name)(Span)
   
let () =
  let loc = Loc.create "some-file.cmp" 3 5 15 in
  let span = Span.from loc (Loc.create "some-file.cmp" 3 15 25) in
  let span2 = Span.from loc (Loc.create "foo.cmp" 7 4 25) in
  let span3 = Span.from loc (Loc.create "some-file.cmp" 4 15 25) in
  let name = Name.internal "foo" in
  let path = Path.create_ns (Name.input "org.bovinegenius") (List.map Name.input ["Foo"; "abc"; "xyz"; "baz"]) in
  Printf.printf "%s\n" (Loc.to_string loc);
  Printf.printf "%s\n" (Span.to_string span);
  Printf.printf "%s\n" (Span.to_string span2);
  Printf.printf "%s\n" (Span.to_string span3);
  Printf.printf "%s\n" (Name.to_string name);
  Printf.printf "%s\n" (Path.to_string path);
  Printf.printf "%s\n" (Path.debug_string path)
