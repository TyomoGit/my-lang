fn main() {
    #[cfg(debug_assertions)]
    println!(r"cargo:rustc-link-search=./target/debug");

    #[cfg(not(debug_assertions))]
    println!(r"cargo:rustc-link-search=./target/release");
}
