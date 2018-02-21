# purescript-precise-datetime

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

Since this package depends on [purescript-bigints](https://pursuit.purescript.org/packages/purescript-bigints), it also requires installing `big-integer` from npm when targeting JavaScript.
