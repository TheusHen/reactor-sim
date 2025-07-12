fn main() {
    println!("cargo:rustc-link-search=native=../ada_core/build");
    println!("cargo:rustc-link-lib=dylib=reactor");
}