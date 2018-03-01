// module Data.BigInt.Extras

var Decimal = require("decimal.js");

exports.truncated = function(x) {
  return x.truncated();
}
