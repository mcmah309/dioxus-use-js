#![doc = include_str!("../README.md")]

use std::{fmt::Display, sync::Arc};

pub use dioxus_use_js_macro::use_js;

// We export these so downstreams don't need `serde` or `serde_json` directly
pub use serde_json::Error as SerdeJsonError;
// exports used by macro.
pub use serde::Serialize as SerdeSerialize;
pub use serde_json::Value as SerdeJsonValue;

#[doc(hidden)]
pub use serde_json::from_value as serde_json_from_value;

// We do not export this so the dioxus version doing the eval is the same, otherwise it may compile but using two different versions of dioxus at runtime will likely cause a runtime error
// be two different versions of dioxus in the graph
// pub use dioxus::document::eval as dioxus_document_eval;
// pub use dioxus::document::EvalError as DioxusEvalError;



//************************************************************************//

/// An error related to the execution of a javascript operation
#[derive(Debug)]
pub enum JsError {
    Eval(dioxus::document::EvalError),
    Deserialize(SerdeJsonError),
}

impl std::fmt::Display for JsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsError::Eval(e) => write!(f, "JavaScript evaluation error: {}", e),
            JsError::Deserialize(e) => write!(f, "Deserialization output error: {}", e),
        }
    }
}

impl std::error::Error for JsError {}

//************************************************************************//

/// A reference to a javascript value that can be held on the dioxus side and passed to functions generated
/// by this crate.
/// 
/// An instance of this is created or used by e.g.
/// ```rust,ignore
/// dioxus_use_js::use_js! {
///     ts: "ts/example.ts",
///     bundle: "assets/example.js",
///     functions: usingJsValue,
/// }
/// ```
/// Where `"ts/example.ts"` uses this marker type
/// ```ts
/// type JsValue<T = any> = T;
/// ```
/// And `usingJsValue` uses `JsValue`.
/// 
/// This uses `Arc` internally and the value on the js side is destroyed when the last reference is dropped
// Dev Note: No `serde::Serialize` or `serde::Deserialize` on purpose since the value is destroyed when dropped
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct JsValue(Arc<Inner>);

/// Abstraction used to implement the one time drop
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Inner(String);

impl JsValue {
    #[deprecated(note = "This constructor is for internal use only. Do not use directly.")]
    #[doc(hidden)]
    pub fn internal_create(id: String) -> Self {
        Self(Arc::new(Inner(id)))
    }

    #[deprecated(note = "This is for internal use only. Do not use directly.")]
    #[doc(hidden)]
    pub fn internal_get(&self) -> &str {
        self.0.0.as_str()
    }
}

impl Display for JsValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value in js: window[\"{}\"]", self.0.0)
    }
}

impl Drop for Inner {
    fn drop(&mut self) {
        let object_name = std::mem::take(&mut self.0);
        // work around for no async drop trait
        dioxus::core::spawn_forever(async move {
            let eval = dioxus::document::eval(
                r#"
const objectName = await dioxus.recv();
if (window.hasOwnProperty(objectName)) {
    delete window[objectName];
}
return null;
"#,
            );
            if let Err(error) = eval.send(object_name.as_str()) {
                dioxus::logger::tracing::error!(
                    "Failed to send object name to clean up `window[\"{object_name}\"]`. Error: {error}"
                );
            }
            if let Err(error) = eval.await {
                dioxus::logger::tracing::error!(
                    "Failed to clean up JavaScript object `window[\"{object_name}\"]`. Error: {error}"
                );
            } else {
                dioxus::logger::tracing::trace!(
                    "Successfully dropped JsValue and cleaned up JavaScript object `window[\"{object_name}\"]`."
                );
            }
        });
    }
}
