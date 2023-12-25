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

let run rope_of_atd input_path output_path formatter_and_args =
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
    match formatter_and_args with
    | None -> code
    | Some (formatter, formatter_args) ->
      let formatter_args_0 = Array.of_list (formatter :: formatter_args) in
      let in_ch, out_ch = Unix.open_process_args formatter formatter_args_0 in
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

let main lang input_path output_path disable_formatting =
  let code_of_atd =
    match lang with
    | `Rust -> Rust.r_full_module
    | `OCaml -> OCaml.r_full_module
  in
  let formatter =
    match disable_formatting with
    | true -> None
    | false -> (
      match lang with
      | `Rust -> Some ("rustfmt", [ "--emit"; "stdout" ])
      | `OCaml -> Some ("ocamlformat", [ "-"; "--impl" ])
    )
  in
  run code_of_atd input_path output_path formatter

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
    let disable_formatting =
      let doc =
        "disable formatting by the (language-appropriate) external formatting \
         program (enabled by default) "
      in
      Arg.(value & flag & info [ "f"; "disable-formatting" ] ~doc)
    in

    Cmd.v (Cmd.info "porc" ~doc)
      Term.(const main $ lang $ input_path $ output_path $ disable_formatting)
  in
  Cmd.eval ~catch:false cmd
