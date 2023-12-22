open Atd.Ast

(* To make code generation functional (and composable), we use a rope
   abstraction. In addition to these two [Rope] functions, we use [Rope.empty],
   [Rope.concat] and [Rope.to_string]. *)
let v = Rope.of_string
let ( ^ ) = Rope.( ^ )

exception NotSupported of string

module Rust = struct
  (* mapping from atd builtins to their Rust counterparts *)
  let builtins =
    [ ("int", "i64"); ("float", "f64"); ("bool", "bool"); ("string", "String") ]

  (* @see <https://github.com/ahrefs/atd/blob/master/atdj/src/atdj_names.ml#L4>
     copied *)
  let to_camel_case (s : string) =
    let res = Bytes.of_string s in
    let offset = ref 0 in
    let upper = ref true in
    let f = function
      | '_' -> upper := true
      | ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') as x ->
        upper := true;
        Bytes.set res !offset x;
        incr offset
      | _ as x ->
        if !upper then (
          Bytes.set res !offset (Char.uppercase_ascii x);
          upper := false
        )
        else Bytes.set res !offset x;
        incr offset
    in
    String.iter f s;
    Bytes.to_string (Bytes.sub res 0 !offset)

  let rec r_full_module (_, module_body) =
    let use_stmts =
      let stmts =
        [
          v "// auto-generatored by porc -- DO NOT EDIT";
          v "use binprot::macros::{BinProtRead, BinProtWrite}";
          v "use binprot::{BinProtRead, BinProtSize, BinProtWrite}";
        ]
      in
      Rope.concat ~sep:(v ";\n") stmts
    in
    (* body's items are separated by two newlines *)
    let body = Rope.concat ~sep:(v "\n\n") (r_module_body module_body) in

    (* use statements come before body, and the two are separated by a
       semicolon *)
    use_stmts ^ v ";" ^ body

  and r_module_body module_body =
    (* body contains a number of items *)
    List.map r_item module_body

  and r_item (Type type_def) =
    (* an item is a type definition *)
    let _, (name, type_params, _), type_expr = type_def in
    (* type parameters, as they appear in the left-hand side alongside the name
       of the type *)
    let type_params =
      match type_params with
      | [] -> Rope.empty
      | _ ->
        let cameld_params =
          List.map (fun param -> v (to_camel_case param)) type_params
        in
        (* when we have type parameters, we enclose them in angle brackets *)
        v "<" ^ Rope.concat ~sep:(v ",") cameld_params ^ v ">"
    in
    (* In Rust, recursive type instances must be Box'd, unless the are enclosed
       inside a Rust Vec 'd. The first two are arguments to [r_type_expr] are
       used to support the identification of recursive types whose Rust
       instantiation must be Box'd. The second argument is a flag denoting
       whether the type instance has a [list] (translated to Vec) ancestor. *)
    let expr = r_type_expr name false type_expr in
    let item =
      match type_expr with
      | Sum _ ->
        (* e.g. translate {[ let x = [ A | B ] ]} to {[ enum X(A,B,) ]} *)
        v "enum" ^ v " " ^ v (to_camel_case name) ^ type_params ^ expr
      | Record _ ->
        (* e.g. translate {[ type x = { y : int } ]} to {[ struct X { y : i64, }
           ]} *)
        v "struct" ^ v " " ^ v (to_camel_case name) ^ type_params ^ expr
      | Tuple _ ->
        (* e.g. translate {[ type x = (int * float) ]} to {[ struct X(i64,f64);
           ]} *)
        v "struct" ^ v " " ^ v (to_camel_case name) ^ type_params ^ expr ^ v ";"
      | Option _
      (* e.g. translate {[ type x = int option ]} to {[ type X = Option<i64>;
         ]} *)
      | Name _
      (* e.g. translate {[ type x = int y ]} to {[ type X = Y<i64>; ]} *)
      | List _
      (* e.g. translate {[ type x = int list ]} to {[ type X = Vec<i64>; ]} *)
        ->
        v "type" ^ v " "
        ^ v (to_camel_case name)
        ^ type_params ^ v "=" ^ expr ^ v ";"
      | Tvar _ -> assert false
      | Nullable _ -> raise (NotSupported "nullable")
      | Shared _ -> raise (NotSupported "shared")
      | Wrap _ -> raise (NotSupported "wrap")
    in
    (* attributes necessary to trigger bin-prot deriving functions *)
    let attribute =
      v "#[derive(BinProtRead, BinProtWrite, Debug, PartialEq)]"
    in
    attribute ^ v "\n" ^ item

  and r_type_expr parent ancestor_is_list type_expr =
    match type_expr with
    | Sum (_, variants, _) ->
      v "{" ^ r_sum parent ancestor_is_list variants ^ v "}"
    | Record (_, fields, _) ->
      v "{" ^ r_record parent ancestor_is_list fields ^ v "}"
    | Tuple (_, cell, _) -> v "(" ^ r_cell parent ancestor_is_list cell ^ v ")"
    | List (_, type_expr, _) ->
      v "Vec" ^ v "<" ^ r_type_expr parent true type_expr ^ v ">"
    | Option (_, type_expr, _) ->
      v "Option" ^ v "<" ^ r_type_expr parent ancestor_is_list type_expr ^ v ">"
    | Name (_, name_te, _) -> r_name parent ancestor_is_list name_te
    | Tvar (_, tvar) -> v (to_camel_case tvar)
    | Nullable _ | Shared _ | Wrap _ -> assert false

  (* translate sum type (Rust enum) *)
  and r_sum parent ancestor_is_list variants =
    let ss =
      List.map
        (fun variant ->
          match variant with
          | Inherit _ -> raise (NotSupported "inheritance")
          | Variant (_, (name, _), type_expr_opt) -> (
            let s = v name in
            match type_expr_opt with
            | None -> s
            | Some type_expr ->
              s ^ v "(" ^ r_type_expr parent ancestor_is_list type_expr ^ v ")"
          )
        )
        variants
    in
    Rope.concat ~sep:(v ",") ss ^ v ","

  (* translate an element of a tuple *)
  and r_cell parent ancestor_is_list cell =
    let ss =
      List.map
        (fun (_, type_expr, _) -> r_type_expr parent ancestor_is_list type_expr)
        cell
    in
    Rope.concat ~sep:(v ",") ss

  (* translate a record (Rust struct) *)
  and r_record parent ancestor_is_list fields =
    let ss =
      List.map
        (fun field ->
          match field with
          | `Field simple_field ->
            let _, (name, _, _), type_expr = simple_field in
            v name ^ v ":" ^ r_type_expr parent ancestor_is_list type_expr
          | `Inherit _ -> raise (NotSupported "inheritance")
        )
        fields
    in
    Rope.concat ~sep:(v ",") ss ^ v ","

  (* translate type aliasing and parametric type instantiation *)
  and r_name parent ancestor_is_list (_, name, type_exprs) =
    match List.assoc_opt name builtins with
    | Some builtin_name ->
      (* builtin types are not parameterized *)
      assert (List.is_empty type_exprs);
      v builtin_name
    | None ->
      let e = v (to_camel_case name) in
      let e_box =
        if
          name = parent
          (* recursive references *)
          && not ancestor_is_list (* all ancestor types are not a list *)
        then v "Box<" ^ e ^ v ">"
        else (* Box not necessary *) e
      in
      let tes =
        match type_exprs with
        | [] -> Rope.empty
        | _ ->
          let tes = List.map (r_type_expr parent ancestor_is_list) type_exprs in
          v "<" ^ Rope.concat ~sep:(v ",") tes ^ v ">"
      in
      e_box ^ tes
end

module OCaml = struct
  let r_full_module (_, _module_body) = raise (NotSupported "OCaml")
end

(* read the contents of an input channel into a string *)
let read_all =
  let rec loop buf ch =
    let continue =
      try
        let line = input_line ch in
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
        true
      with End_of_file -> false
    in
    if continue then loop buf ch
  in
  fun ch ->
    let buf = Buffer.create 1000 in
    loop buf ch;
    Buffer.contents buf

let run rope_of_atd input_path output_path formatter =
  (* open an input file (or just use [stdin]) *)
  let input_ch =
    match input_path with
    | None -> stdin
    | Some path ->
      let ch = open_in path in
      at_exit (fun () -> close_in ch);
      ch
  in
  (* read ATD file *)
  let atd, _ = Atd.Util.read_channel input_ch in
  (* generate code into a rope *)
  let s = rope_of_atd atd in
  (* convert rope to a string *)
  let code = Rope.to_string s in

  (* optionally format the code using an external formatting program *)
  let formatted_code =
    match formatter with
    | None -> code
    | Some formatter ->
      let in_ch, out_ch = Unix.open_process formatter in
      output_string out_ch code;
      close_out out_ch;
      read_all in_ch
  in
  match output_path with
  | None -> print_endline formatted_code
  | Some path ->
    let output_ch = open_out path in
    output_string output_ch formatted_code;
    close_out output_ch

let main lang input_path output_path =
  match lang with
  | `Rust -> run Rust.r_full_module input_path output_path (Some "rustfmt")
  | `OCaml -> run OCaml.r_full_module input_path output_path (Some "ocamlformat")

open Cmdliner

let _ =
  let cmd =
    let doc =
      "generate OCaml or Rust code that {de}serializes values of types defined \
       in an ATD file. With both language targets, the {de}serialization uses \
       Bin_prot."
    in
    let input_path =
      let doc = "path of input ATD file (absent: stdin)" in
      Arg.(
        value & opt (some file) None & info [ "i"; "input" ] ~docv:"PATH" ~doc
      )
    in
    let output_path =
      let doc = "path of output code file (absent: stdout)" in
      Arg.(
        value
        & opt (some string) None
        & info [ "o"; "output" ] ~docv:"PATH" ~doc
      )
    in
    let lang =
      let doc = "language target" in
      let string_lang_assoc =
        [
          ("ocaml", `OCaml); ("OCaml", `OCaml); ("rust", `Rust); ("Rust", `Rust);
        ]
      in
      Arg.(
        required
        & opt (some (enum string_lang_assoc)) None
        & info [ "l"; "lang" ] ~docv:"LANG" ~doc
      )
    in
    Cmd.v (Cmd.info "porc" ~doc)
      Term.(const main $ lang $ input_path $ output_path)
  in
  Cmd.eval ~catch:false cmd
