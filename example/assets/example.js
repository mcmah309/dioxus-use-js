// js-utils/src/example.ts
function greeting(from, to) {
  return `Hello ${to}, this is ${from} speaking from JavaScript!`;
}
async function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
function createJsObject() {
  return {
    name: "example",
    method: function(value) {
      return value + 25;
    }
  };
}
function useJsObject(input, value) {
  let result = value.method(input);
  return result;
}
async function createJsObjectPromise() {
  return Promise.resolve(createJsObject());
}
async function createJsObjectPromiseNullable() {
  return Promise.resolve(null);
}
function useJsObjectNullable(input, value) {
  let result = value?.method(input) ?? null;
  return result;
}
async function useCallback1(startingValue, callback) {
  let doubledValue = startingValue * 2;
  let quadrupledValue = await callback(doubledValue);
  if (quadrupledValue != doubledValue * 2) {
    throw new Error("Callback example 1 did not double value");
  }
  let finalValue = quadrupledValue * 2;
  return finalValue;
}
async function useCallback2(callback) {
  let value = await callback();
  if (value != 30) {
    throw new Error("Callback example 2 did not send value of 30");
  }
  let finalValue = value * 2;
  return finalValue;
}
async function useCallback3(startingValue, callback) {
  let value = await callback(startingValue + 8);
  if (value != null) {
    throw new Error("Callback example 3 did not send back correct value for void");
  }
  return startingValue + 4;
}
async function useCallback4(startingValue, callback) {
  let value = await callback();
  if (value != null) {
    throw new Error("Callback example 4 did not send back correct value for void");
  }
  return startingValue + 10;
}
async function untyped(input) {
  return null;
}
function json(input) {
  return [input];
}
export {
  useJsObjectNullable,
  useJsObject,
  useCallback4,
  useCallback3,
  useCallback2,
  useCallback1,
  untyped,
  sleep,
  json,
  greeting,
  createJsObjectPromiseNullable,
  createJsObjectPromise,
  createJsObject
};
