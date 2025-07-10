use dioxus::prelude::*;
use dioxus_use_js::{use_js, EvalResultExt, JsError};

// Generate the greeting function at compile time
use_js!("assets/example.js"::greeting);

// Or generate multiple functions:
// use_js!("assets/example.js"::{greeting, add});

// Or generate all exported functions:
// use_js!("assets/example.js"::*);

fn main() {
    launch(App);
}

#[component]
fn App() -> Element {
    let future: Resource<Result<String, JsError>> = use_resource(|| async move {
        let from = "dave";
        let to = "john";

        // Now we can call the generated function directly!
        greeting(from, to).await.deserialize()
    });

    rsx!(
        div {
            h1 { "Dioxus `use_js!` macro example!" }
            {
                match &*future.read() {
                    Some(Ok(greeting)) => rsx! {
                        p { "Greeting from JavaScript: {greeting}" }
                    },
                    Some(Err(e)) => rsx! {
                        p { "Error: {e}" }
                    },
                    None => rsx! {
                        p { "Running js..." }
                    },
                }
            }
        }
    )
}
