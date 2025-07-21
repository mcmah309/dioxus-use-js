/** 
 * Creates a greeting
*/
export function greeting(from, to) {
    return `Hello ${to}, this is ${from} speaking from JavaScript!`;
}

/// Creates a js value that is not serialized
export function createJsObject() {
    return {
        name: "example",
        method: function (value) {
            return value + 25;
        },
    };
}

/// Uses a js value
export function useJsObject(input, value) {
    let result = value.method(input);
    return result;
}

export async function useCallback(startingValue,  callback) {
    let doubledValue = startingValue * 2;
    let quadrupledValue = await callback(doubledValue);
    if (quadrupledValue != doubledValue * 2) {
        throw new Error("Callback did not double value");
    }
    let finalValue = quadrupledValue * 2;
    return finalValue;
}