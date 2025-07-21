# `dioxus-use-js`

A macro that does compile time checks and generates Rust bindings to JavaScript or TypeScript functions, for use with [`Dioxus`](https://github.com/DioxusLabs/dioxus).
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

```rust,ignore
use dioxus_use_js::use_js;

use_js!("assets/example.js"::greeting);
```

**Generated Rust signature**:

```rust,ignore
async fn greeting(from: impl Serialize, to: impl Serialize) -> Result<Value, JsError>;
```

Use it like:

```rust,ignore
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
bun build src/example.ts --outfile assets/example.js
```

Bind with:

```rust,ignore
use_js!("src/example.ts", "assets/example.js"::{greeting});
```

**Generated Rust signature**:

```rust,ignore
async fn greeting(from: &str, to: &str) -> Result<String, JsError>;
```

---

## Macro Syntax

### Js

```rust,ignore
use_js!("bundle.js"::function);
use_js!("bundle.js"::{func1, func2});
use_js!("bundle.js"::*);
```

### Ts

```rust,ignore
use_js!("source.ts", "bundle.js"::function);
use_js!("source.ts", "bundle.js"::{func1, func2});
use_js!("source.ts", "bundle.js"::*);
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

## `JsValue`: Javascript References

This special TypeScript type signals to the macro to **bypass serialization** and pass native JS values as opaque references between Rust and JavaScript. The macro generates the glue code required. The JS value is automatically disposed when all references on the Rust side go out of scope.

### Example Usage

**TypeScript:**

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

```rust,ignore
pub async fn createJsObject() -> Result<JsValue, JsError>;

pub async fn useJsObject(value: &JsValue) -> Result<f64, JsError>;
```

## `RustCallback`: Passing Closures from Rust to JavaScript

This special TypeScript type signals to the macro that a **Rust async closure** will be passed into the JavaScript function. The macro generates the glue code required. This enables advanced interop patterns, such as calling Rust logic from within JS — all while preserving type safety.

### Example Usage

**TypeScript:**

```ts
type RustCallback<A = any, R = any> = (arg: A) => Promise<R>;

export async function useCallback(
  startingValue: number,
  callback: RustCallback<number, number>
): Promise<number> {
  let doubledValue = startingValue * 2;
  let result = await callback(doubledValue); // Calls back into Rust
  return result * 2;
}
```

**Rust:**

```rust,ignore
let callback_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
    // Rust async closure that will be called by JS
    let callback = async |value: f64| Ok(value * 2.0);

    // Pass it into the JS function
    let result = useCallback(&2.0, callback).await?;
    Ok(result)
});
```
