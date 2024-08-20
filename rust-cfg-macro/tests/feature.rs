use rust_cfg_macro::feature;

#[test]
fn expression() {
    let x = feature!(if "foo" { 13 } else { 42 });

    assert_eq!(x, 42);
}
