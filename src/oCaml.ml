open Atd.Ast
open Common

let rec r_full_module (_, body) =
  (* auto-generation comment *)
  let comment = v "(* auto-generatoed by porc -- DO NOT EDIT *)" in

  (* needed to support list *)
  let open_module = v "open Bin_prot.Std" in

  let bodies = Atd.Util.tsort body in
  let bodies = List.map r_body bodies in
  Rope.concat ~sep:(v "\n") (comment :: open_module :: bodies)

and r_body (is_recursive, body) =
  Rope.concat ~sep:(v "\n") (r_items is_recursive body)

and r_items is_recursive items =
  (* body contains a number of items *)
  match (is_recursive, items) with
  | _, [] -> assert false
  | false, _ -> List.map (r_item true) items
  | true, item_h :: item_t ->
    let h = r_item true item_h in
    let t = List.map (r_item false) item_t in
    h :: t

and r_item is_first (Type type_def) =
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
  let type_keyword = match is_first with true -> "type" | false -> "and" in
  v type_keyword ^ v " " ^ type_params ^ v name ^ v "=" ^ expr
  ^ v "[@@deriving bin_io]"

and r_type_expr type_expr =
  match type_expr with
  | Sum (_, variants, _) -> r_sum variants
  | Record (_, fields, _) -> r_record fields
  | Tuple (_, tuple, _) -> r_tuple tuple
  | List (_, type_expr, _) -> v "(" ^ r_type_expr type_expr ^ v ") list"
  | Option (_, type_expr, _) -> v "(" ^ r_type_expr type_expr ^ v ") option"
  | Name (_, name_te, _) -> r_name name_te
  | Tvar (_, tvar) -> v "'" ^ v tvar
  | Nullable _ | Shared _ | Wrap _ -> raise (NotSupported "inheritance")

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
          | Some type_expr -> s ^ v " of " ^ r_type_expr type_expr
        )
      )
      variants
  in
  Rope.concat ~sep:(v "|") ss

(* translate an element of a tuple *)
and r_tuple tuple =
  let ss = List.map (fun (_, type_expr, _) -> r_type_expr type_expr) tuple in
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
