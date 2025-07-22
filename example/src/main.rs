use dioxus::{logger::tracing::Level, prelude::*};
use dioxus_use_js::{use_js, JsError};

// Use typescript to generate the following functions at compile time
// with the correct Rust types determined from the source:
use_js!("ts/example.ts", "assets/example.js"::*);
// Note: Typescript is not needed, as seen in the below commented out examples.
// But it is required for exact Rust type generation }and `JsValue`.

// Javascript can also be used directly without typescript.
// Generate a function without the correct Rust types:
// use_js!("assets/example.js"::greeting);

// Generate multiple functions:
// use_js!("assets/example.js"::{greeting});

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
        let output = greeting(from, to).await?;
        Ok(output)
    });

    let js_value_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
        // No serialization!
        // The value is kept on the js side and a reference to it is kept on the rust side.
        // The value is automatically disposed when all rust references no longer exist.
        let js_value = createJsObject().await?;
        let output = useJsObject(&2.0, &js_value).await?;
        // Since `js_value` is dropped here and all references no longer exist,
        // the referenced value will be disposed on the js side.
        Ok(output)
    });

    let callback1_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
        // Rust side closure callable from javascript
        let callback = async |value: f64| Ok(value * 2.0);
        let output = useCallback1(&2.0, callback).await?;
        Ok(output)
    });

    let callback2_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
        let callback = async || Ok(30.0);
        let output = useCallback2(callback).await?;
        Ok(output)
    });

    let callback3_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
        let callback = async |value: f64| {
            dioxus::logger::tracing::trace!("Callback3 was called on the rust side with value `{value}`");
            Ok(())
        };
        let output = useCallback3(&4.0, callback).await?;
        Ok(output)
    });

    let callback4_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
        let callback = async || {
            dioxus::logger::tracing::trace!("Callback4 was called on the rust side with no value");
            Ok(())
        };
        let output = useCallback4(&10.0, callback).await?;
        Ok(output)
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
                "`JsValue` - object method calling:"
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
                small {
                    "Check the logs, you should also see something like 'Successfully dropped JsValue and cleaned up JavaScript object'"
                }
            }
        }
        p {
            "`RustCallback`:"
            p {
                "Input Output Callback - Expected 16 got:"
                {
                    match &*callback1_example.read() {
                        Some(Ok(callback_example)) => rsx! {
                            p { style: "color:green", "{callback_example}" }
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
                "Output Only Callback - Expected 60 got:"
                {
                    match &*callback2_example.read() {
                        Some(Ok(callback_example)) => rsx! {
                            p { style: "color:green", "{callback_example}" }
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
                "Input Only Callback - Expected 8 and see logs for a tracing log of: 'Callback3 was called on the rust side with value `12`'"
                {
                    match &*callback3_example.read() {
                        Some(Ok(callback_example)) => rsx! {
                            p { style: "color:green", "{callback_example}" }
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
                "No Input Or Output Callback - Expected 20 and see logs for a tracing log of: 'Callback4 was called on the rust side with no value'"
                {
                    match &*callback4_example.read() {
                        Some(Ok(callback_example)) => rsx! {
                            p { style: "color:green", "{callback_example}" }
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
        }
    )
}
