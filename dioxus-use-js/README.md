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
// assets/example.js

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
// js-utils/example.ts

export function greeting(from: string, to: string): string {
    return `Hello ${to}, this is ${from} speaking from JavaScript!`;
}
```

Build with:

```sh
bun build js-utils/example.ts --outfile assets/example.js
```
> See [BunBuild](https://docs.rs/dioxus-use-js/latest/dioxus_use_js/struct.BunBuild.html) for use in `build.rs` as well - [Example](https://github.com/mcmah309/dioxus-use-js/blob/master/example/build.rs)

Bind with:

```rust,ignore
use_js!("js-utils/example.ts", "assets/example.js"::{greeting});
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
| `RustCallback<T,TT>`     | `dioxus::core::Callback<T, impl Future<Output = Result<TT, serde_json::Value>> + 'static>` | `-`|
| `Drop`     | `-` | `-`|


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

This special TypeScript type signals to the macro that a **Rust async callback** will be passed into the JavaScript function. The macro generates the glue code required. This enables advanced interop patterns, such as calling Rust logic from within JS — all while preserving type safety. This type cannot be nested.

```ts
type RustCallback<A, R> = (arg: A) => Promise<R>;
```
`A` and `R` can only be:
- `string` 
- `number`
- `boolean`
- `T | null`
- `T[]`
- `Map<T, TT>`
- `Set<T>`
- `void`
- `Json`

Multiple invocations of a `RustCallback` can be inflight at the same time e.g.
```js
let results = await Promise.all([callback(1), callback(2)]);
```
Depending on the side rust logic (e.g. different network requests), some invocations may finish before others. They do not debounce, wait for finish, or cancel previous requests. If this is desired, one would have to implement this logic themselves. Therefore it may be best to wait for the previous response to finish e.g.
```js
let result1 = await callback(1);
let result2 = await callback(2);
```
The lifecycle of a `RustCallback` is tied to the lifecycle of the component the function was called in. i.e when the component drops all ongoing requests will be canceled on the rust side and awaiting promises on the js side will throw notifying that the component has been dropped. Any additional calls to the callback on the js side will also throw.

Since the lifecycle of `RustCallback` is not tied to the function invocation. The function can return and all `RustCallback`'s will function until the component is dropped.

On the rust side, if the callback returns an `Err` then the js `Promise` will be rejected with that serialized value.

#### Example Usage

**TypeScript:**

```ts
type RustCallback<A, R> = (arg: A) => Promise<R>;

export async function useCallback(
  startingValue: number,
  doubleIt: RustCallback<number, number>
): Promise<number> {
  let doubledValue = await doubleIt(startingValue); // Calls back into Rust
  return doubledValue;
}
```

**Generated Rust signature**:

```rust,ignore
pub async fn useCallback(
    startingValue: f64,
    doubleIt: dioxus::core::Callback<f64, impl Future<Output = Result<f64, serde_json::Value>> + 'static>,
) -> Result<f64, JsError>;
```

**Usage:**

```rust,ignore
// Rust async closure that will be called by JS
let cb = use_callback(move |value: f64| async move {
    Ok(value * 2.0)
});
let callback_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
    // Pass it into the JS function
    let value = useCallback(2.0, callback).await?;
    Ok(value)
});
```

### `Drop`: Hook Into The Component Drop LifeCycle

`Drop` is used to hook into the component drop lifecycle on the js side.
```ts
type Drop = Promise<void>;
```
When the promise completes, the component the function was invoked from has been dropped. As such, all `RustCallback` parameters will now throw if invoked. Therefore, `Drop` can be used to remove any handlers (e.g. `drop.then(() => document.removeEventListener('click', handler))`) or abort early from a function invocation.

`Drop` is different from all the other special types in that it does not rely on any external context provided by user of the function containing it. Therefore, no user facing rust code will be generated for it.

`Drop` is also available in plain js. Any function parameter named `drop` without a type will be treated as `Drop`.

#### Example Usage

**TypeScript:**
```ts
export async function dropExample(
  value: number,
  drop: Drop
): Promise<number>;
```
**Generated Rust signature**:

```rust,ignore
pub async fn dropExample(
    value: f64,
) -> Result<f64, JsError>;
```