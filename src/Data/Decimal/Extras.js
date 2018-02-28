// module Data.BigInt.Extras

var Decimal = require("decimal.js");

exports.truncated = function(x) {
  return x.truncated();
}

exports.isInteger = function(x) {
  return x.isInteger();
}