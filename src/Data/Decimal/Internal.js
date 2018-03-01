// module Data.Decimal.Internal

var Decimal = require("decimal.js");

exports.truncated = function(x) {
  return x.truncated();
}
