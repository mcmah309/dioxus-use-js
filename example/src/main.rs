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
        let output = useJsObject(2.0, &js_value).await?;
        // Since `js_value` is dropped here and all references no longer exist,
        // the referenced value will be disposed on the js side.
        Ok(output)
    });

    let callback1_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
        // Rust side closure callable from javascript
        let callback = async |value: f64| Ok(value * 2.0);
        let output = useCallback1(2.0, callback).await?;
        Ok(output)
    });

    let mut call = use_signal(|| 5);

    let callback2_example: Resource<Result<f64, JsError>> = use_resource(move || async move {
        let callback = async || {
            call += 1;
            Ok(30.0)
        };
        let output = useCallback2(callback).await?;
        Ok(output)
    });

    let callback3_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
        let callback = async |value: f64| {
            dioxus::logger::tracing::trace!(
                "Callback3 was called on the rust side with value `{value}`"
            );
            Ok(())
        };
        let output = useCallback3(4.0, callback).await?;
        Ok(output)
    });

    let callback4_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
        let callback = async || {
            dioxus::logger::tracing::trace!("Callback4 was called on the rust side with no value");
            Ok(())
        };
        let output = useCallback4(10.0, callback).await?;
        Ok(output)
    });

    rsx!(
        "call is {call}",
        main { style: "padding: 2rem; font-family: sans-serif; line-height: 1.6;",
            h1 { "Dioxus `use_js!` Macro Example" }

            section {
                h2 { "Simple JS Function Call" }
                {example_result(&function_calling_example.read())}
            }

            section {
                h2 { "`JsValue`: Object Method Call" }
                {example_result(&js_value_example.read())}
                small {
                    "Check logs for cleanup message: \
                    'Successfully dropped JsValue and cleaned up JavaScript object'"
                }
            }

            section {
                h2 { "`RustCallback` Examples" }
                div {
                    h3 {
                        "Input & Output Callback (expected 16):"
                        {example_result(&callback1_example.read())}
                    }
                }
                div {
                    h3 {
                        "Output Only Callback (expected 60):"
                        {example_result(&callback2_example.read())}
                    }
                }

                div {
                    h3 { "Input Only Callback (expected 8):" }
                    {example_result(&callback3_example.read())}
                    small { "Check logs for: 'Callback3 was called on the rust side with value `12`'" }
                }

                div {
                    h3 { "No Input Or Output (expected 20):" }
                    {example_result(&callback4_example.read())}
                    small { "Check logs for: 'Callback4 was called on the rust side with no value'" }
                }
            }
        }
    )
}

fn example_result(result: &Option<Result<impl std::fmt::Display, JsError>>) -> Element {
    match result {
        Some(Ok(val)) => rsx!(
            p { style: "color:green", "{val}" }
        ),
        Some(Err(e)) => rsx!(
            p { style: "color:red", "Error: {e}" }
        ),
        None => rsx!(
            p { style: "color:blue", "Running js..." }
        ),
    }
}
