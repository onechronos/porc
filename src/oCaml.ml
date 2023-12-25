open Atd.Ast
open Common

let rec r_full_module (_, module_body) =
  (* body's items are separated by two newlines *)
  Rope.concat ~sep:(v "\n\n") (r_module_body module_body)

and r_module_body module_body =
  (* body contains a number of items *)
  List.map r_item module_body

and r_item (Type type_def) =
  (* an item is a type definition *)
  let _, (name, type_params, _), type_expr = type_def in
  (* type parameters, as they appear in the left-hand side alongside the name of
     the type *)
  let type_params =
    match type_params with
    | [] -> Rope.empty
    | _ ->
      let tick_params = List.map (fun p -> v "'" ^ v p) type_params in
      v "(" ^ Rope.concat ~sep:(v ",") tick_params ^ v ")"
  in
  let expr = r_type_expr type_expr in
  v "type " ^ type_params ^ v name ^ v "=" ^ expr ^ v "[@@deriving bin_prot]"

and r_type_expr type_expr =
  match type_expr with
  | Sum (_, variants, _) -> r_sum variants
  | Record (_, fields, _) -> r_record fields
  | Tuple (_, cell, _) -> r_cell cell
  | List (_, type_expr, _) -> v "(" ^ r_type_expr type_expr ^ v ") list"
  | Option (_, type_expr, _) -> v "(" ^ r_type_expr type_expr ^ v ") option"
  | Name (_, name_te, _) -> r_name name_te
  | Tvar (_, tvar) -> v tvar
  | Nullable _ | Shared _ | Wrap _ -> assert false

and r_sum variants =
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
            s ^ v " of " ^ v "(" ^ r_type_expr type_expr ^ v ")"
        )
      )
      variants
  in
  Rope.concat ~sep:(v "|") ss

(* translate an element of a tuple *)
and r_cell cell =
  let ss = List.map (fun (_, type_expr, _) -> r_type_expr type_expr) cell in
  v "(" ^ Rope.concat ~sep:(v "*") ss ^ v ")"

and r_record fields =
  let ss =
    List.map
      (fun field ->
        match field with
        | `Field simple_field ->
          let _, (name, _, _), type_expr = simple_field in
          v name ^ v ":" ^ r_type_expr type_expr
        | `Inherit _ -> raise (NotSupported "inheritance")
      )
      fields
  in
  v "{" ^ Rope.concat ~sep:(v ";") ss ^ v "}"

(* translate type aliasing and parametric type instantiation *)
and r_name (_, name, type_exprs) =
  match type_exprs with
  | [] -> v name
  | _ ->
    let tes = List.map r_type_expr type_exprs in
    v "(" ^ Rope.concat ~sep:(v ",") tes ^ v ") " ^ v name
