`servant-api-check` is a library to statically check whether servant apis are
valid or not, while providing better type error messages for invalid servant
apis.

## Status

This library is experimental, feedback is appreciated. If the feedback is good,
interpretations like `servant-server` might consider using `servant-api-check`
in the future.

## TODO

- disallow GET endpoints with request bodies
- disallow two ReqBody combinators in one endpoint
- disallow overlapping endpoints
