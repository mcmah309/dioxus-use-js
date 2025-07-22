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

export async function useCallback1(startingValue ,callback) {
    let doubledValue = startingValue * 2;
    let quadrupledValue = await callback(doubledValue);
    if (quadrupledValue != doubledValue * 2) {
        throw new Error("Callback example 1 did not double value");
    }
    let finalValue = quadrupledValue * 2;
    return finalValue;
}

export async function useCallback2(callback) {
    let value = await callback();
    if (value != 30) {
        throw new Error("Callback example 2 did not send value of 30");
    }
    let finalValue = value * 2;
    return finalValue;
}

export async function useCallback3(startingValue ,callback) {
    let value = await callback(startingValue + 8);
    if (value != null) {
        throw new Error("Callback example 3 did not send back correct value for void");
    }
    return startingValue + 4;
}

export async function useCallback4(startingValue ,callback) {
    let value = await callback();
    if (value != null) {
        throw new Error("Callback example 4 did not send back correct value for void");
    }
    return startingValue + 10;
}