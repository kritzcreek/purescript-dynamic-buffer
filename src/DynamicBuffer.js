export function lengthImpl(array) {
  return array.length;
};

export function setImpl(array, offset, x) {
  array[offset] = x;
};

export function setAllImpl(array, offset, xs) {
  array.set(xs, offset);
};

export function allocate(size) {
  return new Uint8Array(size);
};

export function subarray(array, offset, length) {
  return array.subarray(offset, length);
};

export function whenE(pred, action) {
  if (pred) {
    action();
  }
};

export function toStringImpl(array) {
  let numbers = [];
  array.forEach(b => numbers.push("0x" + b.toString(16).toUpperCase()));
  return "[" + numbers.join(", ") + "]";
};

export function encodeUtf8(s) {
  let te = new TextEncoder();
  return te.encode(s)
};

export function throwImpl(s) {
  return function () {
    throw new Error(s);
  };
};
