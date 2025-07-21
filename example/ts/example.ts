/// This is a reserved marker type that tells the `use_js!` macro to not do serialization and
/// deserialization, but instead create a shim and return an opaque proxy object that can be used to
/// reference the internal js object, so it can be passed around on the rust side.
type JsValue = any;

/** 
 * Creates a greeting
*/
export function greeting(from, to: string): string {
    return `Hello ${to}, this is ${from} speaking from JavaScript!`;
}

/// Creates a js value that is not serialized
export function createJsObjectWithFunction(): JsValue {
    return {
        name: "example",
        method: function (value) {
            return value + 25;
        },
    };
}

/// Uses a js value
export function useObjectsFunction(value: JsValue): number {
    let result = value.method(2);
    return result;
}