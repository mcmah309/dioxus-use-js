#![doc = include_str!("../README.md")]

pub use dioxus_use_js_macro::*;

//************************************************************************//

pub trait EvalResultExt {
    fn deserialize<T: serde::de::DeserializeOwned>(
        self
    ) -> Result<T, JsError>;
}

impl EvalResultExt for Result<serde_json::Value, dioxus_document::EvalError> {
    fn deserialize<T: serde::de::DeserializeOwned>(
        self
    ) -> Result<T, JsError> {
        self
        .map_err(JsError::Eval)
        .and_then(|v| serde_json::from_value(v).map_err(JsError::Deserialize))
    }
}

/// An error related to the execution of a javascript operation
#[derive(Debug)]
pub enum JsError {
    Eval(dioxus_document::EvalError),
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

// Note: No `Clone` on purpose since the value is destroyed when dropped
#[derive(serde::Serialize, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct JsValue(pub(crate) String);