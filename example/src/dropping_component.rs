use dioxus::prelude::*;
use dioxus_use_js::use_js;

use_js!("js-utils/src/example.ts", "assets/example.js"::{sleep, drop_example});

#[component]
pub(crate) fn Dropping() -> Element {
    let mut switch = use_signal(|| true);
    let value = use_resource(move || async move {
        sleep(5000.0).await.unwrap();
        switch.toggle();
    });
    if *switch.read() {
        rsx!(
            ToDrop {}
        )
    } else {
        rsx!(
            div { "Dropped: Clicks are no longer logged and handler is cleaned up" }
        )
    }
}

#[component]
fn ToDrop() -> Element {
    let cb = use_callback(move |point: Vec<f64>| async move {
        let x = point[0];
        let y = point[1];
        info!("Clicked at point ({}, {})", x, y);
        // Multiple can be inflight at the same time
        sleep(1000.0).await.unwrap();
        info!("Finished processing point ({}, {})", x, y);
        Ok(())
    });
    let value = use_resource(move || async move {
        drop_example(cb).await.unwrap()
    });
    let value = value.value().read().unwrap_or(0.0);
    rsx!(
        div { "5 seconds until drop. Click around and see logs for messages" }
        div { "Value should be 44: `{value}`" }
    )
}
