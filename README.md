# purescript-precise-datetime

[![Build Status](https://travis-ci.org/awakesecurity/purescript-precise-datetime.svg?branch=master)](https://travis-ci.org/awakesecurity/purescript-precise-datetime)

Dates with nanosecond precision.


## Motivation

This package preserves nanosecond precision even in cases where the underlying language runtime does not.

For example, in JavaScript:

```javascript
const rfc3339String = "1985-03-13T12:34:56.123456789Z";
const date = new Date(rfc3339String);
console.log(date.toISOString()); // 1985-03-13T20:34:56.123Z
```


## Dependencies

Since this package depends on [purescript-decimals](https://pursuit.purescript.org/packages/purescript-decimals), it also requires installing `decimal.js` from npm when targeting JavaScript.
