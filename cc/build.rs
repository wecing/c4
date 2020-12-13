extern crate protoc_rust;

fn main() {
    println!("cargo:rerun-if-changed=ast.proto");
    protoc_rust::Codegen::new()
        .out_dir("src")
        .inputs(&["ast.proto"])
        .include(".")
        .run()
        .expect("protoc");
    println!("cargo:rerun-if-changed=ir.proto");
    protoc_rust::Codegen::new()
        .out_dir("src")
        .inputs(&["ir.proto"])
        .include(".")
        .run()
        .expect("protoc");
}
