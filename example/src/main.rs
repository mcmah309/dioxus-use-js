use dioxus::{core::use_drop, logger::tracing::Level, prelude::*};
use dioxus_use_js::{JsError, use_js};
use futures::{SinkExt, StreamExt};

// Use typescript to generate the following functions at compile time
// with the correct Rust types determined from the source:
use_js!("js-utils/src/example.ts", "assets/example.js"::*);
// Use pure js
use_js!("assets/other.js"::*);

#[allow(non_snake_case)]
pub async fn on_click2(
    mut invoke_cb: impl AsyncFnMut(Vec<f64>) -> Result<(), Box<dyn std::error::Error + Send + Sync>>,
) -> Result<(), dioxus_use_js::JsError> {
    const MODULE: Asset = asset!("assets/example.js", AssetOptions::builder().with_hash_suffix(false));
    const FUNC_NAME: &str = "on_click";
    #[doc = r#""#]
    fn ___above_is_the_generated_js___() {}

    // let js = format!(
    //     "const{{DioxusRequestResponse}}=await import(\"wrapped_channel.js\");const dioxus = new DioxusRequestResponse(dioxus);const{{on_click}}=await import(\"{}\");const invoke_cb=async(v)=>{{dioxus.req(2,v);}};const cb_dropped=async()=>{{await dioxus.req(3,null);}};let _r_;try{{_r_=await on_click(invoke_cb, cb_dropped);}}catch(e){{console.warn(\"Executing `on_click` threw:\", e);dioxus.req(1,null);}}dioxus.req(0,_r_);return null;",
    //     MODULE
    // );
    let js = r#"
const {
    DioxusRequestResponse
} = await import("wrapped_channel.js");
let _d_ = new DioxusRequestResponse(dioxus);
_d_.start();
const {
    on_click
} = await import("assets/example.js");
const invoke_cb = async (v) => {
    await _d_.req(2, v);
};
const cb_dropped = async () => {
    await _d_.req(3, null);
};
try {
    let _r_ = await on_click(invoke_cb, cb_dropped);
    _d_.ok(_r_);
} catch (e) {
    console.warn("Executing `on_click` threw:", e);
    _d_.err();
}
_d_.req(0, _r_);
return null;
"#;

    let mut eval = dioxus::document::eval(js);
    loop {
        let value = eval
            .recv::<dioxus_use_js::SerdeJsonValue>()
            .await
            .map_err(|e| dioxus_use_js::JsError::Eval {
                func: FUNC_NAME,
                error: e,
            })?;
        match value {
            dioxus_use_js::SerdeJsonValue::Array(values) => {
                if values.len() != 2 || values.len() != 3 { // todo
                    unreachable!("{}", dioxus_use_js::__SEND_VALIDATION_MSG)
                }
                let mut iter = values.into_iter();
                let action_ = match iter.next().unwrap() {
                    dioxus_use_js::SerdeJsonValue::Number(action_) => action_,
                    _ => unreachable!("{}", dioxus_use_js::__INDEX_VALIDATION_MSG),
                };
                let value = iter.next().unwrap();
                match action_
                    .as_u64()
                    .expect(dioxus_use_js::__INDEX_VALIDATION_MSG)
                {
                    0 => {
                        return dioxus_use_js::serde_json_from_value(value).map_err(|e|{
                            dioxus_use_js::JsError::Eval {
                                func:FUNC_NAME,error:dioxus::document::EvalError::Serialization(e),
                            }
                        }).and_then(|e|{
                            if matches!(e,dioxus_use_js::SerdeJsonValue::Null){
                                Ok(())
                            }else {
                                Err(dioxus_use_js::JsError::Eval {
                                    func:FUNC_NAME,error:dioxus::document::EvalError::Serialization(<dioxus_use_js::SerdeJsonError as dioxus_use_js::SerdeDeError> ::custom(dioxus_use_js::__BAD_VOID_RETURN.to_owned()))
                                })
                            }
                        });
                    }
                    1 => {
                        return Err(dioxus_use_js::JsError::Threw { func: FUNC_NAME });
                    }
                    2u64 => {
                        dioxus::prelude::spawn( async move {
                        let value = dioxus_use_js::serde_json_from_value(value).map_err(|e| {
                            dioxus_use_js::JsError::Eval {
                                func: FUNC_NAME,
                                error: dioxus::document::EvalError::Serialization(e),
                            }
                        })?;
                        let value = match invoke_cb(value).await {
                            Ok(value) => value,
                            Err(error) => {
                                return Err(dioxus_use_js::JsError::Callback {
                                    func: FUNC_NAME,
                                    callback: "invoke_cb",
                                    error: error,
                                });
                            }
                        };
                        eval.send(dioxus_use_js::SerdeJsonValue::Null)
                            .map_err(|e| dioxus_use_js::JsError::Eval {
                                func: FUNC_NAME,
                                error: e,
                            })?;
                        });
                    }
                    _ => unreachable!("{}", dioxus_use_js::__BAD_CALL_MSG),
                }
            }
            _ => unreachable!("{}", dioxus_use_js::__SEND_VALIDATION_MSG),
        }
    }
}

fn main() {
    dioxus::logger::init(Level::WARN).unwrap();
    launch(App);
}

#[component]
fn App() -> Element {
    let do_nothing: Resource<Result<String, JsError>> = use_resource(|| async move {
        let _: () = do_nothing().await?;
        Ok("Perfect".to_owned())
    });

    let function_calling_example: Resource<Result<String, JsError>> = use_resource(|| async move {
        let from = "john";
        let to = "dave";
        // Now we can call the generated function directly!
        let output = greeting(from, to).await?;
        Ok(output)
    });

    let throws_example: Resource<Result<JsError, String>> = use_resource(|| async move {
        match throws().await {
            Ok(_) => return Err("This is unexpected output".to_owned()),
            Err(err) => return Ok(err),
        };
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

    let js_value_promise_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
        // Example using Promise<JsValue<T>>
        let js_value = createJsObjectPromise().await?;
        let output = useJsObject(5.0, &js_value).await?;
        Ok(output)
    });

    let js_value_nullable_example: Resource<Result<f64, JsError>> = use_resource(|| async move {
        // Example with nullable JsValue - returns null, so we get None
        let js_value_option = createJsObjectPromiseNullable().await?;
        let output = useJsObjectNullable(3.0, js_value_option.as_ref()).await?;
        let output = output.unwrap_or(-1000.0);
        Ok(output)
    });

    let mut callback1_signal = use_signal(|| "Callback1 not yet called :(".to_owned());
    let callback1_example: Resource<Result<f64, JsError>> = use_resource(move || async move {
        // Rust side closure callable from javascript
        let callback = async |value: f64| {
            callback1_signal
                .write()
                .replace_range(.., "Callback1 called! Sleeping for 2 second...");
            sleep(2000.0).await?;
            callback1_signal
                .write()
                .replace_range(.., "Callback1 called!");
            Ok(value * 2.0)
        };
        let output = callback1(2.0, callback).await?;
        Ok(output)
    });

    let mut callback2_signal = use_signal(|| "Callback2 not yet called :(".to_owned());
    let callback2_example: Resource<Result<f64, JsError>> = use_resource(move || async move {
        let callback = async || {
            callback2_signal
                .write()
                .replace_range(.., "Callback2 called! Sleeping for 2 second...");
            sleep(2000.0).await?;
            callback2_signal
                .write()
                .replace_range(.., "Callback2 called!");
            Ok(30.0)
        };
        let output = callback2(callback).await?;
        Ok(output)
    });

    let mut callback3_signal = use_signal(|| "Callback3 not yet called :(".to_owned());
    let callback3_example: Resource<Result<f64, JsError>> = use_resource(move || async move {
        let callback = async |_: f64| {
            callback3_signal
                .write()
                .replace_range(.., "Callback3 called! Sleeping for 2 second...");
            sleep(2000.0).await?;
            callback3_signal
                .write()
                .replace_range(.., "Callback3 called!");
            Ok(())
        };
        let output = callback3(4.0, callback).await?;
        Ok(output)
    });

    let mut callback4_signal = use_signal(|| "Callback4 not yet called :(".to_owned());
    let callback4_example: Resource<Result<f64, JsError>> = use_resource(move || async move {
        let callback = async || {
            callback4_signal
                .write()
                .replace_range(.., "Callback4 called! Sleeping for 2 second...");
            sleep(2000.0).await?;
            callback4_signal
                .write()
                .replace_range(.., "Callback4 called!");
            Ok(())
        };
        let output = callback4(10.0, callback).await?;
        Ok(output)
    });

    let mut callback5_signal = use_signal(|| "Callback5 not yet called :(".to_owned());
    let callback5_example: Resource<Result<String, JsError>> = use_resource(move || async move {
        let callback = async |json: serde_json::Value| {
            let value1 = json[0].as_i64().unwrap_or_default();
            let value2 = json[1].as_i64().unwrap_or_default();
            callback5_signal.write().replace_range(
                ..,
                &format!("Callback5 called! with values `[{value1}, {value2}]`"),
            );
            Ok(())
        };
        let _: () = callback5(callback).await?;
        Ok("()".to_owned())
    });

    let callback6_example: Resource<Result<JsError, String>> = use_resource(move || async move {
        let callback = async || {
            #[derive(Debug)]
            struct TestError(String);
            impl std::error::Error for TestError {}
            impl std::fmt::Display for TestError {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.0)
                }
            }
            return Err(Box::new(TestError("Error from callback".to_owned()))
                as Box<dyn std::error::Error + Send + Sync>);
        };
        match callback6(callback).await {
            Ok(_) => return Err("This is unexpected output".to_owned()),
            Err(err) => return Ok(err),
        };
    });

    let token = use_hook(|| tokio_util::sync::CancellationToken::new());
    let drop_token = token.clone();
    use_drop(move || {
        drop_token.cancel();
    });
    let mut on_click_signal: Signal<(f64, f64)> = use_signal(|| (0.0, 0.0));
    let callback7_example: Resource<Result<String, JsError>> = use_resource(move || {
        let value = token.clone();
        async move {
            // let dropped = async move || {
            //     error!("Entering");
            //     value.cancelled().await;
            //     error!("Leaving");
            //     Ok(())
            // };
            let callback = async |points: Vec<f64>| {
                warn!("Hit");
                if points.len() != 2 {
                    panic!("Unexpected nimber of arguments");
                }
                let x = points[0];
                let y = points[1];
                on_click_signal.set((x, y));
                Ok(())
            };

            on_click2(callback).await?;
            Ok("()".to_owned())
        }
    });

    rsx!(
        main { style: "padding: 2rem; font-family: sans-serif; line-height: 1.6;",

            h1 { "Dioxus `use_js!` Macro Example" }

            section {
                h2 { "Simple JS Function Call" }
                {example_result(&do_nothing.read())}
            }

            section {
                h2 { "Simple TS Function Call" }
                {example_result(&function_calling_example.read())}
            }

            section {
                h2 { "Function That Throws Example" }
                {example_result(&throws_example.read())}
            }

            section {
                h2 { "`JsValue` Examples" }
                small {
                    "Check logs for cleanup messages: \
                    'Cleaned up JavaScript object'"
                }
                div {
                    h3 { "Object Method Call (expected 27):" }
                    {example_result(&js_value_example.read())}
                }
                div {
                    h3 { "Promise (expected 30):" }
                    {example_result(&js_value_promise_example.read())}
                }
                div {
                    h3 { "Nullable (expected: -1000):" }
                    {example_result(&js_value_nullable_example.read())}
                }
            }

            section {
                h2 { "`RustCallback` Examples" }
                div {
                    h3 { "Input & Output Callback (expected 16):" }
                    {example_result(&callback1_example.read())}
                    small { "Signal: {callback1_signal}" }
                }
                div {
                    h3 { "Output Only Callback (expected 60):" }
                    {example_result(&callback2_example.read())}
                    small { "Signal: {callback2_signal}" }
                }

                div {
                    h3 { "Input Only Callback (expected 8):" }
                    {example_result(&callback3_example.read())}
                    small { "Signal: {callback3_signal}" }
                }

                div {
                    h3 { "No Input Or Output (expected 20):" }
                    {example_result(&callback4_example.read())}
                    small { "Signal: {callback4_signal}" }
                }
                div {
                    h3 { "No Input Only Callback (expected ()) and [1, 2] for the signal value:" }
                    {example_result(&callback5_example.read())}
                    small { "Signal: {callback5_signal}" }
                }
                div {
                    h3 { "Callback That Returns A callback Error:" }
                    {example_result(&callback6_example.read())}
                }
                div {
                    h3 { "Callback With Click Event (click anywhere on the document):" }
                    {example_result(&callback7_example.read())}
                    small { "Last Click Position: ({on_click_signal.read().0}, {on_click_signal.read().1})" }
                }
            }
        }
    )
}

fn example_result(
    result: &Option<Result<impl std::fmt::Display, impl std::fmt::Display>>,
) -> Element {
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
