open Test

let _ =
  let a = [ 1.2; 2e3; -4005. ] in
  let b = Some [ -200.; infinity; 0. ] in
  let c = { x = 4; y = "foo"; z = () } in
  let d = A true in
  let e = (40001, -7e7) in
  let f = (1234, 100e100) in
  let g = G1 true in
  let h = G2 d in
  let i = 0 in
  let j = I 88 in
  let k = J j in
  let m = { h = G1 314; fg = G1 neg_infinity } in
  let n = X { i = 42 } in
  let s =
    List
      [
        List [ List [ Atom "asdf"; List [ Atom "567" ] ]; Atom "***" ];
        Atom "+++";
      ]
  in
  let t = Node (Leaf 1.1, 2.2, Node (Leaf 11., 22., Leaf 33.)) in
  let el =
    {
      el =
        [
          AA a;
          BB b;
          CC c;
          DD d;
          EE e;
          FF f;
          GG g;
          HH h;
          II i;
          JJ j;
          KK k;
          MM m;
          NN n;
          SS s;
          TT t;
        ];
    }
  in

  let buf_in = Bin_prot.Utils.bin_dump bin_writer_el el in
  let len_in = Bin_prot.Common.buf_len buf_in in

  let el_out = bin_read_el ~pos_ref:(ref 0) buf_in in
  let buf_out = Bin_prot.Utils.bin_dump bin_writer_el el_out in
  let len_out = Bin_prot.Common.buf_len buf_out in
  assert (len_in = len_out);

  for i = 0 to len_in - 1 do
    let c_in = Char.code buf_in.{i} in
    let c_out = Char.code buf_out.{i} in
    assert (c_in = c_out);
    Printf.printf "%d\n" c_in
  done
