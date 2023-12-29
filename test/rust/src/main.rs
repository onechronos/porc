use binprot::{BinProtRead, BinProtWrite};

mod test;
use test::*;

fn test_roundtrip<T>(t_in: T)
where
    T: BinProtRead + BinProtWrite + PartialEq + std::fmt::Debug,
{
    let mut data_in: Vec<u8> = Vec::new();
    t_in.binprot_write(&mut data_in).unwrap();
    // println!("{:#?}", data_in);
    let mut slice_out = data_in.as_slice();
    let t_out = T::binprot_read(&mut slice_out).unwrap();
    // this fails whenever a NaN is in the value:
    // assert_eq!(t_in, t_out)
    let mut data_out: Vec<u8> = Vec::new();
    t_out.binprot_write(&mut data_out).unwrap();
    assert_eq!(data_in, data_out);
    let mut i = 0;
    while i < data_in.len() {
        println!("{}", data_in[i]);
        i += 1
    }
}

fn main() {
    let a = vec![1.2, 2e3, -4005.];
    let b = Some(vec![-200., /* f64::NAN, */ f64::INFINITY, 0.]);
    let c = C {
        x: 4,
        y: "foo".to_string(),
        z: (),
    };
    let d = D::A(true);
    let e = E(40001, -7e7);
    let f = E(1234, 100e100);
    let g = G::G1(true);
    let h = H::G2(D::A(true) /* copy(d) */);
    let i = 0;
    let j = J::I(88);
    let k = K::J(Box::new(J::I(88) /* copy(j) */));
    let m = M {
        h: H::G1(314),
        fg: G::G1(f64::NEG_INFINITY),
    };
    let n = N::X { i: 42 };
    let s = Sexpr::List(vec![
        Sexpr::List(vec![
            Sexpr::List(vec![
                Sexpr::Atom("asdf".to_string()),
                Sexpr::List(vec![Sexpr::Atom("567".to_string())]),
            ]),
            Sexpr::Atom("***".to_string()),
        ]),
        Sexpr::Atom("+++".to_string()),
    ]);
    let t = Tree::Node(
        Box::new(Tree::Leaf(1.1)),
        2.2,
        Box::new(Tree::Node(
            Box::new(Tree::Leaf(11.)),
            22.,
            Box::new(Tree::Leaf(33.)),
        )),
    );

    let aa = Everything::AA(a);
    let bb = Everything::BB(b);
    let cc = Everything::CC(c);
    let dd = Everything::DD(d);
    let ee = Everything::EE(e);
    let ff = Everything::FF(f);
    let gg = Everything::GG(g);
    let hh = Everything::HH(h);
    let ii = Everything::II(i);
    let jj = Everything::JJ(j);
    let kk = Everything::KK(k);
    let mm = Everything::MM(m);
    let nn = Everything::NN(n);
    let ss = Everything::SS(s);
    let tt = Everything::TT(t);

    let el = El {
        el: vec![aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, kk, mm, nn, ss, tt],
    };
    test_roundtrip(el);
}
