/** 
 * Creates a greeting
*/
export function greeting(from, to) {
    return `Hello ${to}, this is ${from} speaking from JavaScript!`;
}

/// Creates a js value that is not serialized
export function createJsObjectWithFunction() {
    return {
        name: "example",
        method: function (value) {
            return value + 25;
        },
    };
}

/// Uses a js value
export function useObjectsFunction(value) {
    let result = value.method(2);
    return result;
}