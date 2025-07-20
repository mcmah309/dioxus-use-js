#![doc = include_str!("../README.md")]

use std::{fmt::Display, sync::Arc};

pub use dioxus_use_js_macro::*;

//************************************************************************//

/// An error related to the execution of a javascript operation
#[derive(Debug)]
pub enum JsError {
    Eval(dioxus::document::EvalError),
    Deserialize(serde_json::Error),
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
/// This uses `Arc` internally and the value on the js side is destroyed when the last reference is dropped
// Note: No `serde::Serialize` or `serde::Deserialize` on purpose since the value is destroyed when dropped
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
        dioxus::prelude::spawn_forever(async move {
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
