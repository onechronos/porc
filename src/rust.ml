open Atd.Ast
open Common

type type_reference = {
  module_ : string; (* the external module containing the type *)
  type_ : string; (* the name of the type *)
}

module TRS = Stdlib.Set.Make (struct
  type t = type_reference

  let compare = Stdlib.compare
end)

let type_reference : annot -> type_reference option =
 fun annot ->
  match List.assoc_opt "rust" annot with
  | None -> None
  | Some (_, fields) -> (
    match (List.assoc_opt "module" fields, List.assoc_opt "t" fields) with
    | Some (_, Some module_), Some (_, Some type_) -> Some { module_; type_ }
    | _ ->
      failwith
        "expecting ocaml annotation with structure <ocaml module=\"Xyz\" \
         t=\"abc\">"
  )

(* mapping from atd builtins to their Rust counterparts *)
let builtins =
  [
    ("unit", "()");
    ("int", "i64");
    ("float", "f64");
    ("bool", "bool");
    ("string", "String");
  ]

let to_camel_case = Camelsnakekebab.upper_camel_case

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
      List.map v
        [
          "// auto-generatored by porc -- DO NOT EDIT";
          "#![allow(dead_code)]";
          "#![allow(unused_imports)]";
          "use binprot::macros::{BinProtRead, BinProtWrite};";
          "use binprot::{BinProtRead, /* BinProtSize, */ BinProtWrite};";
        ]
    in
    Rope.concat ~sep:(v "\n") stmts
  in

  let bodies = Atd.Util.tsort body in
  let bodies_code, reference_set =
    List.fold_left
      (fun (bodies_code, reference_set) body ->
        let body_code, reference_set_b = r_body body in
        (body_code :: bodies_code, TRS.union reference_set_b reference_set)
      )
      ([], TRS.empty) bodies
  in

  let use_modules =
    TRS.fold
      (fun reference use_modules ->
        use_modules ^ v "use crate::" ^ v reference.module_ ^ v ";" (* TODO *)
      )
      reference_set Rope.empty
  in

  let bodies = Rope.concat ~sep:(v "\n\n") bodies_code in

  (* use statements come before body, and the two are separated by a
     semicolon *)
  use_stmts ^ v "\n" ^ use_modules ^ bodies

and r_body (is_recursive, body) =
  let items, reference_set = r_items is_recursive body in
  let body_code = Rope.concat ~sep:(v "\n") items in
  (body_code, reference_set)

and r_items _is_recursive items =
  (* build mapping from an item name to its definition *)
  let name_to_def_map = name_to_def_map items in

  (* a body contains a number of items *)
  let items_code, reference_set =
    List.fold_left
      (fun (items_code, reference_set) item ->
        let item_code, reference_opt = r_item name_to_def_map item in
        let reference_set =
          match reference_opt with
          | None -> reference_set
          | Some reference -> TRS.add reference reference_set
        in
        (item_code :: items_code, reference_set)
      )
      ([], TRS.empty) items
  in
  (items_code, reference_set)

and r_item name_to_def_map (Type type_def) =
  (* an item is a type definition *)
  let _, (name, type_params, annot), type_expr = type_def in
  let camel_name = v (to_camel_case name) in

  match type_reference annot with
  | Some reference ->
    let item =
      v "pub type " ^ camel_name ^ v "=" ^ v reference.module_ ^ v "::"
      ^ v (to_camel_case reference.type_)
      ^ v ";"
    in
    (item, Some reference)
  | None ->
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
    let expr = r_type_expr 0 name_to_def_map false type_expr in

    (* attributes necessary to trigger bin-prot deriving functions *)
    let attribute =
      v "#[derive(BinProtRead, BinProtWrite, Debug, PartialEq)]"
    in
    let item =
      match type_expr with
      | Sum _ ->
        (* e.g. translate {[ let x = [ A | B ] ]} to {[ pub enum X(A,B,) ]} *)
        attribute ^ v "\n" ^ v "pub enum" ^ v " " ^ camel_name ^ type_params
        ^ expr
      | Record _ ->
        (* e.g. translate {[ type x = { y : int } ]} to {[ pub struct X { pub y
           : i64, } ]} *)
        attribute ^ v "\n" ^ v "pub struct" ^ v " " ^ camel_name ^ type_params
        ^ expr
      | Tuple _ ->
        (* e.g. translate {[ type x = (int * float) ]} to {[ pub struct X(pub
           i64,pub f64); ]} *)
        attribute ^ v "\n" ^ v "pub struct" ^ v " " ^ camel_name ^ type_params
        ^ expr ^ v ";"
      | Option _
      (* e.g. translate {[ type x = int option ]} to {[ type X = Option<i64>;
         ]} *)
      | Name _
      (* e.g. translate {[ type x = int y ]} to {[ type X = Y<i64>; ]} *)
      | List _
      (* e.g. translate {[ type x = int list ]} to {[ type X = Vec<i64>; ]} *)
        ->
        v "pub type" ^ v " " ^ camel_name ^ type_params ^ v "=" ^ expr ^ v ";"
      | Tvar _ -> assert false
      | Nullable _ -> raise (NotSupported "nullable")
      | Shared _ -> raise (NotSupported "shared")
      | Wrap _ -> raise (NotSupported "wrap")
    in
    (item, None)

and r_type_expr depth name_to_def_map ancestor_is_list type_expr =
  match type_expr with
  | Sum (_, variants, _) ->
    v "{" ^ r_sum depth name_to_def_map ancestor_is_list variants ^ v "}"
  | Record (_, fields, _) ->
    v "{" ^ r_record depth name_to_def_map ancestor_is_list fields ^ v "}"
  | Tuple (_, cell, _) ->
    (if depth = 0 then v "(" else Rope.empty)
    ^ r_tuple depth name_to_def_map ancestor_is_list cell
    ^ if depth = 0 then v ")" else Rope.empty
  | List (_, type_expr, _) ->
    v "Vec" ^ v "<"
    ^ r_type_expr (depth + 1) name_to_def_map true type_expr
    ^ v ">"
  | Option (_, type_expr, _) ->
    v "Option" ^ v "<"
    ^ r_type_expr (depth + 1) name_to_def_map ancestor_is_list type_expr
    ^ v ">"
  | Name (_, name_and_type_exprs, _) ->
    r_name depth name_to_def_map ancestor_is_list name_and_type_exprs
  | Tvar (_, tvar) -> v (to_camel_case tvar)
  | Nullable _ -> raise (NotSupported "nullable")
  | Shared _ -> raise (NotSupported "shared")
  | Wrap _ -> raise (NotSupported "wrap")

(* translate sum type (aka algebraic datatype, Rust enum) *)
and r_sum depth name_to_def_map ancestor_is_list variants =
  let ss =
    List.map
      (fun variant ->
        match variant with
        | Inherit _ -> raise (NotSupported "inheritance")
        | Variant (_, (name, _), type_expr_opt) -> (
          let s = v name in
          match type_expr_opt with
          | None -> s
          | Some type_expr -> (
            let te =
              r_type_expr (depth + 1) name_to_def_map ancestor_is_list type_expr
            in
            match type_expr with
            | Record _ -> s ^ te
            | _ -> s ^ v "(" ^ te ^ v ")"
          )
        )
      )
      variants
  in
  Rope.concat ~sep:(v ",") ss ^ v ","

(* translate an element of a tuple *)
and r_tuple depth name_to_def_map ancestor_is_list cell =
  let ss =
    List.map
      (fun (_, type_expr, _) ->
        let expr =
          r_type_expr (depth + 1) name_to_def_map ancestor_is_list type_expr
        in
        (if depth = 0 then v "pub " else Rope.empty) ^ expr
      )
      cell
  in
  Rope.concat ~sep:(v ",") ss

(* translate a record (Rust struct) *)
and r_record depth name_to_def_map ancestor_is_list fields =
  let ss =
    List.map
      (fun field ->
        match field with
        | `Field simple_field ->
          let _, (name, _, _), type_expr = simple_field in
          (if depth = 0 then v "pub " else Rope.empty)
          ^ v name ^ v ":"
          ^ r_type_expr (depth + 1) name_to_def_map ancestor_is_list type_expr
        | `Inherit _ -> raise (NotSupported "inheritance")
      )
      fields
  in
  Rope.concat ~sep:(v ",") ss ^ v ","

(* translate type aliasing and parametric type instantiation *)
and r_name depth name_to_def_map ancestor_is_list (_, name, type_exprs) =
  match List.assoc_opt name builtins with
  | Some builtin_name ->
    (* builtin types are not parameterized *)
    assert (List.is_empty type_exprs);
    v builtin_name
  | None ->
    let camel_name = v (to_camel_case name) in
    let maybe_box =
      if
        SM.mem name name_to_def_map
        (* recursive references *)
        && not ancestor_is_list (* all ancestor types are not a list *)
      then v "Box<" ^ camel_name ^ v ">"
      else (* Box not necessary *) camel_name
    in
    let bracketed_type_exprs =
      match type_exprs with
      | [] -> Rope.empty
      | _ ->
        let type_exprs =
          List.map
            (r_type_expr (depth + 1) name_to_def_map ancestor_is_list)
            type_exprs
        in
        v "<" ^ Rope.concat ~sep:(v ",") type_exprs ^ v ">"
    in
    maybe_box ^ bracketed_type_exprs
