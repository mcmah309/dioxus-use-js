#![cfg_attr(docsrs, feature(doc_cfg))]
#![doc = include_str!("../README.md")]

use std::{error::Error, fmt::Display, sync::Arc};

#[cfg(feature = "build")]
mod build;
#[cfg(feature = "build")]
pub use build::*;

pub use dioxus_use_js_macro::use_js;

// We export these so downstreams don't need `serde` or `serde_json` directly
// exports used by macro.
#[doc(hidden)]
pub use serde::Serialize as SerdeSerialize;
#[doc(hidden)]
pub use serde::de::DeserializeOwned as SerdeDeDeserializeOwned;
#[doc(hidden)]
pub use serde::de::Error as SerdeDeError;
#[doc(hidden)]
pub use serde_json::Error as SerdeJsonError;
#[doc(hidden)]
pub use serde_json::Value as SerdeJsonValue;
#[doc(hidden)]
pub use serde_json::from_value as serde_json_from_value;
#[doc(hidden)]
pub const __SEND_VALIDATION_MSG: &str = "Should always send back a value that is an array of two.";
#[doc(hidden)]
pub const __INDEX_VALIDATION_MSG: &str = "The first sent back value should always be a u64.";
#[doc(hidden)]
pub const __BAD_CALL_MSG: &str = "Should only attempt to call known actions.";
#[doc(hidden)]
pub const __BAD_VOID_RETURN: &str =
    "A function that should return no value instead returned a value";
// We do not export this so the dioxus version doing the eval is the same, otherwise it may compile but using two different versions of dioxus at runtime will likely cause a runtime error
// be two different versions of dioxus in the graph
// pub use dioxus::document::eval as dioxus_document_eval;
// pub use dioxus::document::EvalError as DioxusEvalError;

//************************************************************************//

fn _send_sync_error_assert() {
    fn is_send<T: Send>(_: &T) {}
    fn is_sync<T: Sync>(_: &T) {}
    fn is_error<T: Error>(_: &T) {}

    let o: JsError = JsError::Callback {
        func: "",
        callback: "",
        error: Box::new(std::io::Error::new(std::io::ErrorKind::Other, ""))
            as Box<dyn Error + Send + Sync>,
    };
    is_send(&o);
    is_sync(&o);
    is_error(&o);
}

/// An error related to the execution of a javascript operation
#[derive(Debug)]
pub enum JsError {
    /// Error occurred during dioxus evalution.
    /// If this occurs, it usually mean your js is not valid or the wrong type was returned from
    /// your js function
    Eval {
        /// The name of the js function
        func: &'static str,
        error: dioxus::document::EvalError,
    },
    /// A js function that threw a value during execution. The actual error value is logged on the js side as a `console.error`.
    Threw {
        /// Name of the js function
        func: &'static str,
    },
    /// Error occurred during a callback to a rust function
    Callback {
        /// The name of the function
        func: &'static str,
        /// The name of the callback
        callback: &'static str,
        /// The error from the callback
        error: Box<dyn Error + Send + Sync>,
    },
}

impl std::fmt::Display for JsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsError::Eval { func: name, error } => {
                write!(f, "JavaScript function '{}' eval error: {}", name, error)
            }
            JsError::Threw { func: name } => {
                write!(
                    f,
                    "JavaScript function '{}' threw an error during execution",
                    name
                )
            }
            JsError::Callback {
                func: name,
                callback,
                error,
            } => {
                write!(
                    f,
                    "JavaScript function '{}' callback '{}' error: {}",
                    name, callback, error
                )
            }
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
/// dioxus_use_js::use_js!("ts/example.ts", "assets/example.js"::usingJsValue);
/// ```
/// Where `"ts/example.ts"` uses this marker type
/// ```ts
/// type JsValue<T = any> = T;
/// ```
///
/// This uses `Arc` internally and the value on the js side is destroyed when the last reference is dropped
// Dev Note: No `serde::Serialize` or `serde::Deserialize` on purpose since the value is destroyed when dropped
#[derive(
    serde::Serialize, serde::Deserialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub struct JsValue(Arc<Inner>);

/// Abstraction used to implement the one time drop
#[derive(
    serde::Serialize, serde::Deserialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
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
delete window[objectName];
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
