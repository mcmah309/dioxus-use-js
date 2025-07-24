/// This is a reserved marker type that tells the `use_js!` macro to not do serialization and
/// deserialization, but instead create a shim and return an opaque proxy object that can be used to
/// reference the internal js object, so it can be passed around on the rust side. 
type JsValue<T = any> = T;

// input of void means takes no arguments
// output of void means it returns no arguments
type RustCallback<A, R> = (arg: A) => Promise<R>;

type Json = string | number | boolean | null | { [key: string]: Json } | Json[];

/** 
 * Creates a greeting
*/
export function greeting(from, to: string): string {
    return `Hello ${to}, this is ${from} speaking from JavaScript!`;
}

export async function sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
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

export async function useCallback1(startingValue: number, callback: RustCallback<number, number>): Promise<number> {
    let doubledValue = startingValue * 2;
    let quadrupledValue = await callback(doubledValue);
    if (quadrupledValue != doubledValue * 2) {
        throw new Error("Callback example 1 did not double value");
    }
    let finalValue = quadrupledValue * 2;
    return finalValue;
}

export async function useCallback2(callback: RustCallback<void, number>): Promise<number> {
    let value = await callback();
    if (value != 30) {
        throw new Error("Callback example 2 did not send value of 30");
    }
    let finalValue = value * 2;
    return finalValue;
}

export async function useCallback3(startingValue: number, callback: RustCallback<number, void>): Promise<number> {
    let value = await callback(startingValue + 8);
    if (value != null) {
        throw new Error("Callback example 3 did not send back correct value for void");
    }
    return startingValue + 4;
}

export async function useCallback4(startingValue: number, callback: RustCallback<void, void>): Promise<number> {
    let value = await callback();
    if (value != null) {
        throw new Error("Callback example 4 did not send back correct value for void");
    }
    return startingValue + 10;
}

// Functions not used in example but still generated through `*`
//************************************************************************//

export async function untyped(input) {
    return null;
}

export function json(input: Json[]): Json[] {
    return input;
}

// Compile errors
//************************************************************************//

// export function nestedVoid(): void[] {
//     return [];
// }

// export function inputVoid(input: void) {}