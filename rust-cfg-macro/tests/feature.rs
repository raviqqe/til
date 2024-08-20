use rust_cfg_macro::feature;

#[test]
fn expression() {
    let x = feature!(if "test" { 42 } else { 13 });

    assert_eq!(x, 42);
}
