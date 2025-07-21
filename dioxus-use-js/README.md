# `dioxus-use-js`

A macro that generates Rust bindings to JavaScript or TypeScript functions, for use with [`Dioxus`](https://github.com/DioxusLabs/dioxus).
Works across **Web**, **Desktop**, and **Mobile** — no `wasm-bindgen` required.

---

## JavaScript Usage

You can write plain JavaScript and bind exported functions directly.

```js
// example.js

export function greeting(from, to) {
    return `Hello ${to}, this is ${from} speaking from JavaScript!`;
}
```

Bind it in Rust:

```rust
use dioxus_use_js::use_js;

use_js!("assets/example.js"::greeting);
```

**Generated Rust signature**:

```rust
async fn greeting(from: impl Serialize, to: impl Serialize) -> Result<Value, JsError>;
```

Use it like:

```rust
let val = greeting("Alice", "Bob").await?;
let s: String = serde_json::from_value(val)?;
```

---

## TypeScript Usage

If you use TypeScript, the macro will parse types to produce more accurate Rust bindings.

```ts
// example.ts

export function greeting(from: string, to: string): string {
    return `Hello ${to}, this is ${from} speaking from JavaScript!`;
}
```

Compile with:

```sh
bun build example.ts --outfile example.js
```

Bind with:

```rust
use_js! {
    ts: "src/example.ts",
    bundle: "assets/example.js",
    functions: {greeting}
}
```

**Generated Rust signature**:

```rust
async fn greeting(from: &str, to: &str) -> Result<String, JsError>;
```

---

## Macro Syntax

### Js

```rust
use_js!("bundle.js"::function);
use_js!("bundle.js"::{func1, func2});
use_js!("bundle.js"::*);
```

### Ts

```rust
use_js! {
    ts: "src/file.ts",
    bundle: "assets/file.js",
    functions: {name1, name2}
}
```

---

## Type Mapping

| TypeScript            | Rust Input       | Rust Output       |
| --------------------- | ---------------- | ----------------- |
| `string`              | `&str`           | `String`          |
| `number`              | `&f64`           | `f64`             |
| `boolean`             | `&bool`          | `bool`            |
| `T \| null`           | `&Option<T>`     | `Option<T>`       |
| `T[]`                 | `&[T]`           | `Vec<T>`          |
| `Map<T, TT>`          | `&HashMap<T, TT>`| `HashMap<T, TT>`   |
| `Set<T>`              | `&HashSet<T>`    | `HashSet<T>`    |
| `any`, `unknown`, no type, invalid type      | `impl serde::Serialize` | `serde_json::Value`|
| `JsValue<T>`             | `&JsValue`       | `JsValue`         |

---

## `JsValue`

Use `JsValue` in TS to **bypass serialization** and pass native JS values as opaque references between Rust and JavaScript.

### Example (TypeScript)

```ts
type JsValue<T = any> = T;

type MyObject = {
    name: string;
    method: (value: number) => number;
};

export function createJsObject(): JsValue<MyObject> {
    return {
        name: "example",
        method: function (value) {
            return value + 25;
        },
    };
}

export function useJsObject(value: JsValue<MyObject>): number {
    let result = value.method(2);
    return result;
}
```

**Generated Rust signature**:

```rust
pub async fn createJsObject() -> Result<Value, JsError>;

pub async fn useJsObject(value: JsValue) -> Result<f64, dJsError>;
```
