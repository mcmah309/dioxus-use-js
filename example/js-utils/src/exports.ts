export async function createJsObjectPromiseNullable(): Promise<(JsValue<MyObject> | null)> {
    return Promise.resolve(null);
}

export function useJsObjectNullable(input: number, value: JsValue<MyObject> | null): number | null {
    let result = value?.method(input) ?? null;
    return result;
}
