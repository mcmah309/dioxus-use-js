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
let _r_;
try {
    _r_ = await on_click(invoke_cb, cb_dropped);
} catch (e) {
    console.warn("Executing `on_click` threw:", e);
    _d_.req(1, null);
}
_d_.req(0, _r_);
return null;