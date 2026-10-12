fn main() {
    // cspell: disable-next-line
    println!("cargo:rustc-link-arg-bins=-nostartfiles");
    // cspell: disable-next-line
    println!("cargo:rustc-link-arg-bins=-nostdlib");
    println!("cargo:rustc-link-arg-bins=-static");
}
