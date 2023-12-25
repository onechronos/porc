open Atd.Ast
open Common

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

module SM = Stdlib.Map.Make (String)

(* for a number of items, create a map from each item's name to its definition
   (including type parameters, if any *)
let name_to_def_map items =
  List.fold_left
    (fun map item ->
      let (Type (_, (name, type_params, _), type_expr)) = item in
      SM.add name (type_params, type_expr) map
    )
    SM.empty items

let rec r_full_module (_, body) =
  let use_stmts =
    let stmts =
      [
        v "// auto-generatored by porc -- DO NOT EDIT";
        v "#![allow(dead_code)]";
        v "use binprot::macros::{BinProtRead, BinProtWrite};";
        v "use binprot::{BinProtRead, /* BinProtSize, */ BinProtWrite};";
      ]
    in
    Rope.concat ~sep:(v "\n") stmts
  in

  let bodies = Atd.Util.tsort body in
  let bodies = List.map r_body bodies in

  let bodies = Rope.concat ~sep:(v "\n\n") bodies in

  (* use statements come before body, and the two are separated by a
     semicolon *)
  use_stmts ^ v "\n" ^ bodies

and r_body (is_recursive, body) =
  Rope.concat ~sep:(v "\n") (r_items is_recursive body)

and r_items _is_recursive items =
  (* build mapping from an item name to its definition *)
  let name_to_def_map = name_to_def_map items in

  (* a body contains a number of items *)
  List.map (r_item name_to_def_map) items

and r_item name_to_def_map (Type type_def) =
  (* an item is a type definition *)
  let _, (name, type_params, _), type_expr = type_def in
  (* type parameters, as they appear in the left-hand side alongside the name of
     the type *)
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
     inside a Rust Vec 'd. The first two are arguments to [r_type_expr] are used
     to support the identification of recursive types whose Rust instantiation
     must be Box'd. The second argument is a flag denoting whether the type
     instance has a [list] (translated to Vec) ancestor. *)
  let expr = r_type_expr name_to_def_map false type_expr in

  (* attributes necessary to trigger bin-prot deriving functions *)
  let attribute = v "#[derive(BinProtRead, BinProtWrite, Debug, PartialEq)]" in

  match type_expr with
  | Sum _ ->
    (* e.g. translate {[ let x = [ A | B ] ]} to {[ enum X(A,B,) ]} *)
    attribute ^ v "\n" ^ v "pub enum" ^ v " "
    ^ v (to_camel_case name)
    ^ type_params ^ expr
  | Record _ ->
    (* e.g. translate {[ type x = { y : int } ]} to {[ struct X { y : i64, }
       ]} *)
    attribute ^ v "\n" ^ v "pub struct" ^ v " "
    ^ v (to_camel_case name)
    ^ type_params ^ expr
  | Tuple _ ->
    (* e.g. translate {[ type x = (int * float) ]} to {[ struct X(i64,f64);
       ]} *)
    attribute ^ v "\n" ^ v "pub struct" ^ v " "
    ^ v (to_camel_case name)
    ^ type_params ^ expr ^ v ";"
  | Option _
  (* e.g. translate {[ type x = int option ]} to {[ type X = Option<i64>; ]} *)
  | Name _
  (* e.g. translate {[ type x = int y ]} to {[ type X = Y<i64>; ]} *)
  | List _
  (* e.g. translate {[ type x = int list ]} to {[ type X = Vec<i64>; ]} *) ->
    v "pub type" ^ v " "
    ^ v (to_camel_case name)
    ^ type_params ^ v "=" ^ expr ^ v ";"
  | Tvar _ -> assert false
  | Nullable _ -> raise (NotSupported "nullable")
  | Shared _ -> raise (NotSupported "shared")
  | Wrap _ -> raise (NotSupported "wrap")

and r_type_expr name_to_def_map ancestor_is_list type_expr =
  match type_expr with
  | Sum (_, variants, _) ->
    v "{" ^ r_sum name_to_def_map ancestor_is_list variants ^ v "}"
  | Record (_, fields, _) ->
    v "{" ^ r_record name_to_def_map ancestor_is_list fields ^ v "}"
  | Tuple (_, cell, _) ->
    v "(" ^ r_cell name_to_def_map ancestor_is_list cell ^ v ")"
  | List (_, type_expr, _) ->
    v "Vec" ^ v "<" ^ r_type_expr name_to_def_map true type_expr ^ v ">"
  | Option (_, type_expr, _) ->
    v "Option" ^ v "<"
    ^ r_type_expr name_to_def_map ancestor_is_list type_expr
    ^ v ">"
  | Name (_, name_te, _) -> r_name name_to_def_map ancestor_is_list name_te
  | Tvar (_, tvar) -> v (to_camel_case tvar)
  | Nullable _ | Shared _ | Wrap _ -> assert false

(* translate sum type (aka algebraic datatype, Rust enum) *)
and r_sum name_to_def_map ancestor_is_list variants =
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
            s ^ v "("
            ^ r_type_expr name_to_def_map ancestor_is_list type_expr
            ^ v ")"
        )
      )
      variants
  in
  Rope.concat ~sep:(v ",") ss ^ v ","

(* translate an element of a tuple *)
and r_cell name_to_def_map ancestor_is_list cell =
  let ss =
    List.map
      (fun (_, type_expr, _) ->
        r_type_expr name_to_def_map ancestor_is_list type_expr
      )
      cell
  in
  Rope.concat ~sep:(v ",") ss

(* translate a record (Rust struct) *)
and r_record name_to_def_map ancestor_is_list fields =
  let ss =
    List.map
      (fun field ->
        match field with
        | `Field simple_field ->
          let _, (name, _, _), type_expr = simple_field in
          v name ^ v ":"
          ^ r_type_expr name_to_def_map ancestor_is_list type_expr
        | `Inherit _ -> raise (NotSupported "inheritance")
      )
      fields
  in
  Rope.concat ~sep:(v ",") ss ^ v ","

(* translate type aliasing and parametric type instantiation *)
and r_name name_to_def_map ancestor_is_list (_, name, type_exprs) =
  match List.assoc_opt name builtins with
  | Some builtin_name ->
    (* builtin types are not parameterized *)
    assert (List.is_empty type_exprs);
    v builtin_name
  | None ->
    let e = v (to_camel_case name) in
    let e_box =
      if
        SM.mem name name_to_def_map
        (* recursive references *)
        && not ancestor_is_list (* all ancestor types are not a list *)
      then v "Box<" ^ e ^ v ">"
      else (* Box not necessary *) e
    in
    let tes =
      match type_exprs with
      | [] -> Rope.empty
      | _ ->
        let tes =
          List.map (r_type_expr name_to_def_map ancestor_is_list) type_exprs
        in
        v "<" ^ Rope.concat ~sep:(v ",") tes ^ v ">"
    in
    e_box ^ tes
