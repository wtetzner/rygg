

module Env = Env
module Span = Span

module Position : sig
  type t =
    | Location of Span.Location.t
    | Span of Span.t
    | No_position

  val exists : t -> bool

  val merge : t -> t -> t
end = struct
  type t =
    | Location of Span.Location.t
    | Span of Span.t
    | No_position

  let exists pos =
    match pos with
    | No_position -> false
    | _ -> true

  let merge left right =
    match left, right with
    | Location l1, Location l2 -> Location l1
    | Span s1, Span s2 -> Span (Span.merge s1 s2)
    | Location l, Span s -> Location (Span.Location.merge l s.start_pos)
    | Span s, Location l -> Location (Span.Location.merge s.start_pos l)
    | No_position, other -> other
    | other, No_position -> other
end

module Message : sig
  type tag =
    | None
    | Error
    | Ok

  val print_tag : tag -> unit

  val print_pos : Position.t -> unit

  val print_msg : tag -> Position.t -> string -> unit

  val print_msgln : tag -> Position.t -> string -> unit
end = struct
  type tag =
    | None
    | Error
    | Ok

  let tag_exists tag =
    match tag with
    | None -> false
    | _ -> true

  let print_tag_str style tag =
    print_string "[";
    ANSITerminal.(printf style "%s" tag);
    print_string "]"

  let print_tag tag =
    match tag with
    | None -> ()
    | Error -> print_tag_str [ANSITerminal.red] "ERROR"
    | Ok -> print_tag_str [ANSITerminal.green] "OK"

  let print_pos pos =
    let open Position in
    let module Location = Span.Location in
    match pos with
    | Location loc ->
      ANSITerminal.(
        printf [yellow] "%s" (Location.source loc);
        print_string [] ":";
        printf [Bold] "%d:%d" (Location.line loc) (Location.column loc)
      )
    | Span span ->
       let start = span.start_pos in
       let endp = span.end_pos in
       if (Location.line start) = (Location.line endp) then
         ANSITerminal.(
         printf [yellow] "%s" (Location.source start);
         print_string [] ":";
         printf [Bold] "%d:%d-%d"
           (Location.line start)
           (Location.column start)
           (Location.column endp)
         )
       else
         ANSITerminal.(
         printf [yellow] "%s" (Location.source start);
         print_string [] ":";
         printf [Bold] "%d:%d-%d:%d"
           (Location.line start)
           (Location.column start)
           (Location.line endp)
           (Location.column endp)
         )
    | No_position -> ()

  let print_msg tag pos msg =
    if tag_exists tag then
      (print_tag tag;
       print_string " ")
    else
      ();
    if Position.exists pos then
      (print_pos pos;
       print_string " ")
    else
      ();
    print_string msg

  let print_msgln tag pos msg =
    print_msg tag pos msg;
    print_newline ()
end
