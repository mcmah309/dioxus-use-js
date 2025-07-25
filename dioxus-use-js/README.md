# `dioxus-use-js`
[<img alt="github" src="https://img.shields.io/badge/github-mcmah309/dioxus--use--js-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/mcmah309/dioxus-use-js)
[<img alt="crates.io" src="https://img.shields.io/crates/v/dioxus-use-js.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/dioxus-use-js)
[<img alt="test status" src="https://img.shields.io/github/actions/workflow/status/mcmah309/dioxus-use-js/rust.yml?branch=master&style=for-the-badge" height="20">](https://github.com/mcmah309/dioxus-use-js/actions/workflows/rust.yml)


A macro that generates Rust bindings to JavaScript or TypeScript functions, with compile time checks. For use with [`Dioxus`](https://github.com/DioxusLabs/dioxus). No need to use [eval](https://docs.rs/dioxus-document/latest/dioxus_document/fn.eval.html) directly anymore!
Works across all `eval` supported platforms (**Web**, **Desktop**, **Mobile**, and **liveview**) — no `wasm-bindgen` required.

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
async fn greeting<T: DeserializeOwned>(from: impl Serialize, to: impl Serialize) -> Result<T, JsError>;
```

Use it like:

```rust,ignore
let val: String = greeting("Alice", "Bob").await?;
```

---

## TypeScript Usage

If you use TypeScript, the macro will parse types to produce more accurate Rust bindings. See the [Type Mapping](#type-mapping) section for details on how TypeScript types are mapped to Rust types.

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

### Built-in TypeScript Types

| TypeScript            | Rust Input       | Rust Output       |
| --------------------- | ---------------- | ----------------- |
| `string`              | `&str`           | `String`          |
| `number`              | `f64`           | `f64`             |
| `boolean`             | `bool`          | `bool`            |
| `T \| null`           | `Option<&T>`     | `Option<T>`       |
| `T[]`                 | `&[T]`           | `Vec<T>`          |
| `Map<T, TT>`          | `&HashMap<T, TT>`| `HashMap<T, TT>`   |
| `Set<T>`              | `&HashSet<T>`    | `HashSet<T>`    |
| `void`, `undefined`, `never`, `null` | `-` | `()` |
| `any`, `unknown`, `object`, `-`, `*`     | `impl serde::Serialize` | `T: serde::de::DeserializeOwned` |
| `Promise<T>`              | `&T`    | `T`    |

### Special Types

| TypeScript            | Rust Input       | Rust Output       |
| --------------------- | ---------------- | ----------------- |
| `Json`    | `&serde_json::Value` | `serde_json::Value` |
| `JsValue<T>`, `JsValue`              | `&JsValue`       | `JsValue`         |
| `RustCallback<T,TT>`     | `impl AsyncFnMut(T) -> Result<TT, Box<dyn Error + Send + Sync>>` | `-`|
| `RustCallback<void,TT>`     | `impl AsyncFnMut() -> Result<TT, Box<dyn Error + Send + Sync>>` | `-`|

---
## Special Types

Special types are types not included in the regular Typescript type system, but are understood by the `use_js!` macro and may augment the generated binding code.

### `Json`

Json is a simple type that represents valid json. This type can best nested.

```ts
type Json = string | number | boolean | null | { [key: string]: Json } | Json[];
```

#### Example Usage

**TypeScript:**

```ts
type Json = string | number | boolean | null | { [key: string]: Json } | Json[];

export function json(): Json[] {
    return [
        {"key": "value"},
        {"key": "value"},
    ];
}
```

**Generated Rust signature**:

```rust,ignore
pub async fn json() -> Result<Vec<Value> ,JsError>;
```

### `JsValue`: Javascript References

This special TypeScript type signals to the macro to **bypass serialization** and pass native JS values as opaque references between Rust and JavaScript. The macro generates the glue code required. The JS value is automatically disposed when all references on the Rust side go out of scope. Only the following are valid representations:

| Valid Ts Uses               | Input  | Output|
| :-------------------------- | :----- | :---- |
| `JsValue<T>`, `JsValue`                    | `&JsValue`   | `JsValue` |
| `Promise<JsValue<T>>`, `Promise<JsValue>`           | `-`   | `JsValue` |
| `JsValue<T> \| null` `JsValue \| null`           | `Option<&JsValue>`   | `Option<JsValue>` |
| `Promise<JsValue<T> \| null>`, `Promise<JsValue \| null>`           | `-`   | `Option<JsValue>` |

```ts
type JsValue<T = any> = T;
```

#### Example Usage

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

**Usage:**

```rust,ignore
let js_value_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
    // No serialization!
    // The value is kept on the js side and a reference to it is kept on the rust side.
    // The value is automatically disposed when all rust references no longer exist.
    let js_value = createJsObject().await?;
    let output = useJsObject(&js_value).await?;
    // Since `js_value` is dropped here and all references no longer exist,
    // the referenced value will be disposed on the js side.
    Ok(output)
});
```

### `RustCallback`: Passing Closures from Rust to JavaScript

This special TypeScript type signals to the macro that a **Rust async closure** will be passed into the JavaScript function. The macro generates the glue code required. This enables advanced interop patterns, such as calling Rust logic from within JS — all while preserving type safety. This type cannot be nested.

```ts
type RustCallback<A, R> = (arg: A) => Promise<R>;
```

If input `A` is `void` then the Rust closure will take no input.
If output `R` is `void` then the Rust closure will return no output.

> **Note:** On the Javascript side, within the same exported function invocation, do not call another callback before the previous callback has completed. Otherwise, the returned values from Rust may go to the wrong callback. 
> 
> Bad:
> ```ts
> let promise1 = callback();
> let promise2 = callback();
> ```
> Good:
> ```ts
> let value1 = await callback();
> let value2 = await callback();
> ```
> The obvious exception to this is if none or only one of the callbacks in process return a value, then this is fine.

    

#### Example Usage

**TypeScript:**

```ts
type RustCallback<A, R> = (arg: A) => Promise<R>;

export async function useCallback(
  startingValue: number,
  callback: RustCallback<number, number>
): Promise<number> {
  let doubledValue = startingValue * 2;
  let quadValue = await callback(doubledValue); // Calls back into Rust
  return quadValue * 2;
}
```

**Generated Rust signature**:

```rust,ignore
pub async fn useCallback(
    startingValue: f64,
    mut callback: impl AsyncFnMut(f64) -> Result<f64, Box<dyn Error + Send + Sync>>,
) -> Result<f64, JsError>;
```

**Usage:**

```rust,ignore
let callback_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
    // Rust async closure that will be called by JS
    let callback = async |value: f64| Ok(value * 2.0);

    // Pass it into the JS function
    let value = useCallback(2.0, callback).await?;
    Ok(value)
});
```