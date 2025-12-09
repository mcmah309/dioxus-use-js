#![cfg_attr(docsrs, feature(doc_cfg))]
#![doc = include_str!("../README.md")]

use std::ops::{Deref, DerefMut};
use std::pin::Pin;
use std::task::{Context, Poll};
use std::{error::Error, fmt::Display, sync::Arc};

#[cfg(feature = "build")]
mod build;
#[cfg(feature = "build")]
pub use build::*;

use dioxus::document::{self, Eval};
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
pub const __CALLBACK_SEND_VALIDATION_MSG: &str =
    "Callbacks should always send back a value that is an array of three.";
#[doc(hidden)]
pub const __RESULT_SEND_VALIDATION_MSG: &str =
    "Result should always send back a value that is an array of two.";
#[doc(hidden)]
pub const __INDEX_VALIDATION_MSG: &str = "The index value was an unexpected type";
#[doc(hidden)]
pub const __BAD_CALL_MSG: &str = "Should only attempt to call known actions.";
#[doc(hidden)]
pub const __BAD_VOID_RETURN: &str =
    "A function that should return no value instead returned a value";
#[doc(hidden)]
pub const __UNEXPECTED_CALLBACK_TYPE: &str = "The callback was called with the wrong type";
// We do not export this so the dioxus version doing the eval is the same, otherwise it may compile but using two different versions of dioxus at runtime will likely cause a runtime error
// be two different versions of dioxus in the graph
// pub use dioxus::document::eval as dioxus_document_eval;
// pub use dioxus::document::EvalError as DioxusEvalError;

//************************************************************************//

fn _send_sync_error_assert() {
    fn is_send<T: Send>(_: &T) {}
    fn is_sync<T: Sync>(_: &T) {}
    fn is_error<T: Error>(_: &T) {}

    let o: JsError = JsError::Threw { func: "" };
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
    /// A js function that threw a value during execution. The actual error value is logged on the js side as a warning.
    Threw {
        /// Name of the js function
        func: &'static str,
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
pub struct JsValue(Arc<JsValueInner>);

/// Abstraction used to implement the one time drop
#[derive(
    serde::Serialize, serde::Deserialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
struct JsValueInner(String);

impl JsValue {
    #[deprecated(note = "This constructor is for internal use only. Do not use directly.")]
    #[doc(hidden)]
    pub fn internal_create(id: String) -> Self {
        Self(Arc::new(JsValueInner(id)))
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

impl Drop for JsValueInner {
    fn drop(&mut self) {
        let object_name = std::mem::take(&mut self.0);
        dioxus::core::spawn_forever(async move {
            let eval =
                dioxus::document::eval(&format!("delete window[\"{object_name}\"];return null;"));
            if let Err(error) = eval.await {
                dioxus::logger::tracing::error!(
                    "Failed to clean up JavaScript object `window[\"{object_name}\"]`. Error: {error}"
                );
            } else {
                dioxus::logger::tracing::trace!(
                    "Cleaned up JavaScript object `window[\"{object_name}\"]`."
                );
            }
        });
    }
}

/// Signals the drop of a component.
/// 
/// Dev Note: When a normal Eval drops. It does not signal to the channel that it has been dropped.
/// Thus any `await dioxus.recv()` will be awaiting forever, on web we could send one last signal
/// to notify of this (i.e. only use one `await dioxus.recv()` in a seperate async closure
/// after all the parameters have been sent to signal drop), but on desktop the channel
/// is closed at this point.
/// Therefore we store the drop promise on the window and dropping this
/// struct will send the value to resolve the promise.
#[doc(hidden)]
pub struct SignalDrop(String);

impl SignalDrop {
    pub fn new(invocation_id: String) -> Self {
        SignalDrop(invocation_id)
    }
}

impl Drop for SignalDrop {
    fn drop(&mut self) {
        let invocation_id = std::mem::take(&mut self.0);
        dioxus::core::spawn_forever(async move {
            // Note the extra `d` for drop
            let eval =
                dioxus::document::eval(&format!("let i=\"{invocation_id}d\";let p=window[i];delete window[i];p();dioxus.close();return null;"));
            if let Err(error) = eval.await {
                dioxus::logger::tracing::error!(
                    "Failed to notify of drop for invocation `window[\"{invocation_id}d\"]`. Error: {error}"
                );
            } else {
                dioxus::logger::tracing::trace!(
                    "Notified of drop for invocation `window[\"{invocation_id}d\"]`."
                );
            }
        });
    }
}

/// Used in generated code.
#[doc(hidden)]
#[derive(Clone)]
pub struct CallbackResponder(Arc<CallbackResponderInner>);

struct CallbackResponderInner(Eval);

impl CallbackResponder {
    pub fn new(invocation_id: &str) -> Self {
        // r = [id, ok, data], i = id, o = ok, d = data, f = window[function_id], x = f[id] = [resolve, reject]
        CallbackResponder(Arc::new(CallbackResponderInner(dioxus::document::eval(
            &format!(
                "while(true){{let r=await dioxus.recv();if(!Array.isArray(r))break;let f=window[\"{invocation_id}\"];if(f==null)break;let i=r[0],o=r[1],d=r[2],x=f[i];delete f[i];if(o)x[0](d);else x[1](d);}}",
            ),
        ))))
    }

    pub fn respond<T: serde::Serialize>(&self, request_id: u64, is_ok: bool, data: T) {
        let payload = (request_id, is_ok, data);

        let result = self.0.0.send(payload);
        if let Err(e) = result {
            dioxus::logger::tracing::error!(
                "Failed to send callback response for invocation '{}' request '{}': {}",
                request_id,
                is_ok,
                e
            );
        }
    }
}

impl Drop for CallbackResponderInner {
    fn drop(&mut self) {
        // Send a non-array value to break the loop
        let result = self.0.send(serde_json::Value::Null);
        if let Err(e) = result {
            dioxus::logger::tracing::error!("Failed to shut down callback responder: {}", e);
        }
    }
}

#[doc(hidden)]
pub struct PendingFuture;

impl Future for PendingFuture {
    type Output = ();

    fn poll(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<Self::Output> {
        Poll::Pending
    }
}

// pub struct JsNotifyOnce(Arc<JsNotifyOnceInner>);

// impl JsNotifyOnce {
//     pub fn new(notify_on_drop: bool) -> Self {
//         static NOTIFY_ID_COUNTER: std::sync::atomic::AtomicU64 =
//             std::sync::atomic::AtomicU64::new(0);
//         let id = format!(
//             "__js-notify-{}",
//             NOTIFY_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
//         );
//         dioxus::document::eval(&format!(
//             "let r;let p=new Promise((res)=>{{r=res;}});window[\"{id}-p\"]=p;window[\"{id}-r\"]=r;"
//         ));
//         Self(Arc::new(JsNotifyOnceInner {
//             id,
//             notify_on_drop,
//             is_notified: AtomicBool::new(false),
//         }))
//     }

//     pub fn notify(&self) {
//         self.0.notify();
//     }

//     fn id(&self) -> &str {
//         &self.0.id
//     }
// }

// struct JsNotifyOnceInner {
//     id: String,
//     notify_on_drop: bool,
//     is_notified: AtomicBool,
// }

// impl JsNotifyOnceInner {
//     fn notify(&self) {
//         if self
//             .is_notified
//             .swap(true, std::sync::atomic::Ordering::AcqRel)
//         {
//             return;
//         }
//         let id = self.id.clone();
//         dioxus::core::spawn_forever(async move {
//             let eval = dioxus::document::eval(&format!(
//                 "delete window[\"{id}-p\"];let r=window[\"{id}-r\"];if(r==undefined){{return null;}}r.resolve();delete window[\"{id}-r\"];return null;"
//             ));
//             if let Err(error) = eval.await {
//                 dioxus::logger::tracing::error!(
//                     "Failed to notify JavaScript object `window[\"{id}\"]`. Error: {error}"
//                 );
//             } else {
//                 dioxus::logger::tracing::trace!("Notified JavaScript object `window[\"{id}\"]`.");
//             }
//         });
//     }
// }

// impl Drop for JsNotifyOnceInner {
//     fn drop(&mut self) {
//         if self.notify_on_drop {
//             self.notify();
//         }
//     }
// }
