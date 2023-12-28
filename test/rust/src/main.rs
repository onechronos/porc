use binprot::{BinProtRead, BinProtWrite};

mod test;

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
    let c = test::C {
        x: 4,
        y: "foo".to_string(),
        z: (),
    };
    let d = test::D::A(true);
    let e = test::E(40001, -7e7);
    let f = test::E(1234, 100e100);
    let g = test::G::G1(true);
    let h = test::H::G2(test::D::A(true) /* copy(d) */);
    let i = 0;
    let j = test::J::I(88);
    let k = test::K::J(Box::new(test::J::I(88) /* copy(j) */));
    let m = test::M {
        h: test::H::G1(314),
        fg: test::G::G1(f64::NEG_INFINITY),
    };
    let n = test::N::X { i: 42 };
    let s = test::Sexpr::List(vec![
        test::Sexpr::List(vec![
            test::Sexpr::List(vec![
                test::Sexpr::Atom("asdf".to_string()),
                test::Sexpr::List(vec![test::Sexpr::Atom("567".to_string())]),
            ]),
            test::Sexpr::Atom("***".to_string()),
        ]),
        test::Sexpr::Atom("+++".to_string()),
    ]);
    let t = test::Tree::Node(
        Box::new(test::Tree::Leaf(1.1)),
        2.2,
        Box::new(test::Tree::Node(
            Box::new(test::Tree::Leaf(11.)),
            22.,
            Box::new(test::Tree::Leaf(33.)),
        )),
    );

    let aa = test::Everything::AA(a);
    let bb = test::Everything::BB(b);
    let cc = test::Everything::CC(c);
    let dd = test::Everything::DD(d);
    let ee = test::Everything::EE(e);
    let ff = test::Everything::FF(f);
    let gg = test::Everything::GG(g);
    let hh = test::Everything::HH(h);
    let ii = test::Everything::II(i);
    let jj = test::Everything::JJ(j);
    let kk = test::Everything::KK(k);
    let mm = test::Everything::MM(m);
    let nn = test::Everything::NN(n);
    let ss = test::Everything::SS(s);
    let tt = test::Everything::TT(t);

    let el = test::El {
        el: vec![aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, kk, mm, nn, ss, tt],
    };
    test_roundtrip(el);
}
