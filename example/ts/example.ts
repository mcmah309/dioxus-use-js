/// This is a reserved marker type that tells the `use_js!` macro to not do serialization and
/// deserialization, but instead create a shim and return an opaque proxy object that can be used to
/// reference the internal js object, so it can be passed around on the rust side. 
type JsValue<T = any> = T;

// input of void means takes no arguments
// output of void means it returns no arguments
type RustCallback<A, R> = (arg: A) => Promise<R>;



/** 
 * Creates a greeting
*/
export function greeting(from, to: string): string {
    return `Hello ${to}, this is ${from} speaking from JavaScript!`;
}

type MyObject = {
    name: string;
    method: (value: number) => number;
};

/// Creates a js value that is not serialized
export function createJsObject(): JsValue<MyObject> {
    return {
        name: "example",
        method: function (value) {
            return value + 25;
        },
    };
}

/// Uses a js value
export function useJsObject(input: number, value: JsValue<MyObject>): number {
    let result = value.method(input);
    return result;
}

export async function useCallback(startingValue: number ,callback: RustCallback<number,number>): Promise<number> {
    let doubledValue = startingValue * 2;
    let quadrupledValue = await callback(doubledValue);
    if (quadrupledValue != doubledValue * 2) {
        throw new Error("Callback did not double value");
    }
    let finalValue = quadrupledValue * 2;
    return finalValue;
}