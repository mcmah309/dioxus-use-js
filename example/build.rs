use std::path::PathBuf;

use dioxus_use_js::BunTsCompile;

fn main() {
    BunTsCompile::builder()
        .src_files(vec![PathBuf::from("js-utils/src/example.ts")])
        .output_dir(PathBuf::from("assets"))
        .minify(false)
        .build()
        .run();
}
