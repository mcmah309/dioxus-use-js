#![doc = include_str!("../README.md")]

pub use dioxus_use_js_impl::*;

pub fn deserialize<T: serde::de::DeserializeOwned>(
    value: Result<serde_json::Value, dioxus_lib::document::EvalError>,
) -> Result<T, JsError> {
    value
        .map_err(JsError::Eval)
        .and_then(|v| serde_json::from_value(v).map_err(JsError::Deserialize))
}

#[derive(Debug)]
pub enum JsError {
    Eval(dioxus_lib::document::EvalError),
    Deserialize(serde_json::Error),
}

impl std::fmt::Display for JsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsError::Eval(e) => write!(f, "JavaScript evaluation error: {}", e),
            JsError::Deserialize(e) => write!(f, "Deserialization error: {}", e),
        }
    }
}

impl std::error::Error for JsError {}