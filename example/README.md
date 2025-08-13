# Example

## Building JS From TS

This example uses the `BunTsCompile` struct in `build.rs` so no build command is needed. But if you were to do this by hand, it would look like:
```bash
bun build js-utils/src/example.ts --outdir assets
```

## Running Example

```bash
dx serve
```