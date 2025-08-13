use std::env::current_dir;
use std::path::PathBuf;
use std::process::Command;

use bon::Builder;

#[cfg_attr(docsrs, doc(cfg(feature = "compile")))]
#[derive(Builder)]
pub struct BunTsCompile {
    /// Files to build from
    src_files: Vec<PathBuf>,
    /// The output directory
    output_dir: PathBuf,
    /// If set the output will be a single file
    output_file: Option<String>,
    #[builder(default = true)]
    minify: bool,
    /// Extra bun build flags
    extra_flags: Option<Vec<String>>,
    /// If true, the normal run command will not execute if bun is not installed, but will still print and error. Useful for CI.
    #[builder(default = false)]
    skip_if_bun_missing: bool,
}

impl BunTsCompile {
    pub fn run(&self) {
        fn bun_exists() -> bool {
            Command::new("bun")
                .arg("--version")
                .output()
                .map(|o| o.status.success())
                .unwrap_or(false)
        }
        if self.skip_if_bun_missing && !bun_exists() {
            eprintln!("Bun does not exist, skipping bun build.");
            return;
        }

        let src_files = self
            .src_files
            .iter()
            .map(|e| {
                e.to_str()
                    .expect("a src file is not a valid utf-8 string")
                    .to_owned()
            })
            .collect::<Vec<_>>();
        let output_dir = self
            .output_dir
            .to_str()
            .expect("The output file is not a valid utf-8 string")
            .to_owned();

        let mut args = Vec::new();
        args.push("build".to_owned());
        args.extend(src_files);

        if let Some(output_file) = &self.output_file {
            args.extend([
                "--compile".to_owned(),
                "--outfile".to_owned(),
                self.output_dir
                    .join(output_file)
                    .to_str()
                    .unwrap()
                    .to_owned(),
            ])
        } else {
            args.extend(["--outdir".to_owned(), output_dir]);
        }
        if self.minify {
            args.extend([
                "--minify-syntax".to_owned(),
                "--minify-whitespace".to_owned(),
                // todo Note cannot currently use `--minify-identifiers` since the macro will not find the js functions during validation
                // "--minify-identifiers".to_owned(),
            ]);
        }
        args.push("--target=browser".to_owned());
        args.extend(self.extra_flags.iter().flatten().cloned());

        // Run bun build
        let output = Command::new("bun")
            .args(&args)
            .output()
            .expect("Failed to execute bun build");
        if !output.status.success() {
            panic!(
                "bun build failed.\nArgs: `{}`\nCurrent Directory: `{}`\nStderr:\n{}",
                args.join(" "),
                current_dir().unwrap().display(),
                String::from_utf8_lossy(&output.stderr)
            );
        }
    }
}
