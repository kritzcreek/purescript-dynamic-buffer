exports.lengthImpl = function(array) {
  return array.length;
};

exports.setImpl = function(array, offset, x) {
  array[offset] = x;
};

exports.setAllImpl = function(array, offset, xs) {
  array.set(xs, offset);
};

exports.allocate = function(size) {
  return new Uint8Array(size);
};

exports.subarray = function(array, offset, length) {
  return array.subarray(offset, length);
};

exports.whenE = function(pred, action) {
  if (pred) {
    action();
  }
};

exports.toStringImpl = function(array) {
  let numbers = [];
  array.forEach(b => numbers.push("0x" + b.toString(16).toUpperCase()));
  return "[" + numbers.join(", ") + "]";
};

exports.encodeUtf8 = function(s) {
  let te = new TextEncoder();
  return te.encode(s)
};

exports.throwImpl = function(s) {
  return function () {
    throw new Error(s);
  };
};
