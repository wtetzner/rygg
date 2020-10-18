module Env = Env
module Span = Span
module Loc = Loc
module Input = Input

module Rygg = Rygg

module Position : sig
  type t =
    | Loc of Loc.t
    | Span of Span.t
    | No_position

  val exists : t -> bool

  val merge : t -> t -> t
end = struct
  type t =
    | Loc of Loc.t
    | Span of Span.t
    | No_position

  let exists pos =
    match pos with
    | No_position -> false
    | _ -> true

  let merge left right =
    match left, right with
    | Loc l1, Loc l2 -> Loc l1
    | Span s1, Span s2 -> Span (Span.merge s1 s2)
    | Loc l, Span _ -> Loc l
    | Span s, Loc _ -> Loc (Span.start s)
    | No_position, other -> other
    | other, No_position -> other
end

module FileMap = Map.Make(String)

module Files : sig
  type t = (string FileMap.t) ref
  val empty : t
  val get : t -> string -> string
end = struct
  type t = (string FileMap.t) ref
  let empty = ref FileMap.empty

  let load_string filename =
    Core.In_channel.with_file
      filename ~f:(fun f -> Core.In_channel.input_all f)

  let get files name =
    if FileMap.mem name !files then
      FileMap.find name !files
    else
      let text = load_string name in
      files := FileMap.add name text !files;
      text
end

module Message : sig
  type tag =
    | None
    | Error
    | Ok

  val print_tag : tag -> unit

  val print_tag_msg : tag -> string -> unit

  val print_pos : Position.t -> unit

  val print_msg : tag -> Position.t -> string -> Files.t -> unit

  val print_msgln : tag -> Position.t -> string -> Files.t -> unit
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

  let print_tag_msg tag msg =
    print_tag tag;
    print_string " ";
    print_endline msg

  let print_pos pos =
    let open Position in
    let module Loc = Loc in
    match pos with
    | Loc loc ->
      ANSITerminal.(
        printf [yellow] "%s" (Loc.filename loc);
        print_string [] ":";
        printf [Bold] "%d:%d" (Loc.line loc) (Loc.column loc)
      )
    | Span span ->
       let start = Span.start span in
       let endp = Span.start span in
       if (Loc.line start) = (Loc.line endp) then
         ANSITerminal.(
         printf [yellow] "%s" (Loc.filename start);
         print_string [] ":";
         printf [Bold] "%d:%d-%d"
           (Loc.line start)
           (Loc.column start)
           (Loc.column endp)
         )
       else
         ANSITerminal.(
         printf [yellow] "%s" (Loc.filename start);
         print_string [] ":";
         printf [Bold] "%d:%d-%d:%d"
           (Loc.line start)
           (Loc.column start)
           (Loc.line endp)
           (Loc.column endp)
         )
    | No_position -> ()

  let lookup_line_start text line =
    let len = String.length text in
    let current = ref 1 in
    let pos = ref 0 in
    while !pos < len && !current < line do
      let chr = String.get text !pos in
      match chr with
      | '\n' -> (pos := !pos + 1; current := !current + 1)
      | '\r' ->
         if !pos < len - 1 && String.get text (!pos + 1) = '\n' then
           (current := !current + 1; pos := !pos + 2)
         else
           (pos := !pos + 1)
      | _ -> (pos := !pos + 1)
    done;
    if !current = line then
      Some !pos
    else
      None

  let is_newline text pos =
    let len = String.length text in
    if pos < len then
      let chr = String.get text pos in
      match chr with
      | '\n' -> true
      | '\r' ->
         if pos < len - 1 && String.get text (pos + 1) = '\n' then
           true
         else
           false
      | _ -> false
    else
      false

  let lookup_line text line =
    let start = lookup_line_start text line in
    match start with
    | Some start ->
       (let len = String.length text in
        let line_len = ref 0 in
        while not (is_newline text (start + !line_len))
              && (!line_len + start) < len do
          line_len := !line_len + 1
        done;
        Some (String.sub text start !line_len))
    | None -> None

  let get_some opt =
    match opt with
    | Some x -> x
    | None -> raise (Failure "Called get_some on None")

  let print_caret pos line =
    print_string "  ";
    let is_whitespace c = c == ' ' || c == '\t' in
    let current = ref 0 in
    while !current < pos do
      let chr = String.get line !current in
      if is_whitespace chr then Printf.printf "%c" chr else Printf.printf "%s" " ";
      current := !current + 1
    done;
    ANSITerminal.(printf [magenta] "%s" "^")

  let print_span pos len line =
    print_caret pos line;
    let current = ref 0 in
    while !current < len - 1 do
      ANSITerminal.(printf [magenta] "%s" "^");
      current := !current + 1
    done

  let print_msg tag pos msg files =
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
    print_string msg;
    (match pos with
     | Position.Loc loc ->
        let line = Loc.line loc in
        let text = Files.get files (Loc.filename loc) in
        let line_text = (get_some (lookup_line text line)) in
        Printf.printf "\n  %s\n" line_text;
        print_caret (Loc.column loc) line_text
     | Position.Span span ->
        let line = Loc.line (Span.start span) in
        let text = Files.get files (Loc.filename (Span.start span)) in
        let line_text = (get_some (lookup_line text line)) in
        Printf.printf "\n  %s\n" line_text;
        let start = Loc.column (Span.start span) in
        let endp = Loc.column (Span.finish span) in
        print_span start (endp - start) line_text
     | _ -> ())

  let print_msgln tag pos msg files =
    print_msg tag pos msg files;
    print_newline ()
end
