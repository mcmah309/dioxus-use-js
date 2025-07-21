use dioxus::{logger::tracing::Level, prelude::*};
use dioxus_use_js::{use_js, JsError};

// Generate the following functions at compile time with the correct Rust types determined from the `ts` source:
use_js! {
    ts: "ts/example.ts",
    bundle: "assets/example.js",
    functions: {greeting, createJsObjectWithFunction, useObjectsFunction},
}
// Note: The above format with `ts` source is required for `JsValue` to work. Otherwise it cannot be
// determined that a shim is needed and direct serialization and deserialization is used instead.

// Generate a function without the correct Rust types:
// use_js!("assets/example.js"::greeting);

// Generate multiple functions:
// use_js!("assets/example.js"::{greeting, createJsObjectWithFunction, useObjectsFunction});

// Generate all exported functions:
// use_js!("assets/example.js"::*);

fn main() {
    dioxus::logger::init(Level::TRACE).unwrap();
    launch(App);
}

#[component]
fn App() -> Element {
    let function_calling_example: Resource<Result<String, JsError>> = use_resource(|| async move {
        let from = "john";
        let to = "dave";
        // Now we can call the generated function directly!
        let greeting = greeting(from, to).await?;
        Ok(greeting)
    });

    let js_value_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
        // No serialization!
        // The value is kept on the js side and a reference to it is kept on the rust side.
        // The value is automatically disposed when all rust references no longer exist.
        let js_value = createJsObjectWithFunction().await?;
        let js_value_output = useObjectsFunction(&js_value).await?;
        // `js_value` will be disposed on the js side since we are not returning it, thus all references no longer exist.
        Ok(js_value_output)
    });

    rsx!(
        div {
            h1 { "Dioxus `use_js!` macro example!" }
            p {
                "Simple function calling:"
                {
                    match &*function_calling_example.read() {
                        Some(Ok(function_calling_example)) => rsx! {
                            p { style: "color:green", "{function_calling_example}" }
                        },
                        Some(Err(e)) => rsx! {
                            p { style: "color:red", "Error: {e}" }
                        },
                        None => rsx! {
                            p { style: "color:blue", "Running js..." }
                        },
                    }
                }
            }
            p {
                "JsValue function calling:"
                {
                    match &*js_value_example.read() {
                        Some(Ok(js_value_example)) => rsx! {
                            p { style: "color:green", "{js_value_example}" }
                        },
                        Some(Err(e)) => rsx! {
                            p { style: "color:red", "Error: {e}" }
                        },
                        None => rsx! {
                            p { style: "color:blue", "Running js..." }
                        },
                    }
                }
                "Check the logs, you should also see something like 'Successfully dropped JsValue and cleaned up JavaScript object'"
            }
        }
    )
}
