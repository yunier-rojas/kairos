## Kairos

Kairos is a very specific HTTP proxy. Its purpose is to allow APIs
to migrate to webhook operations without introducing breaking changes
to the public. It does that by transforming legacy requests into
webhook operations and then waits and send the result when it is
available.

## Use case
The main use case of this application is when a public API with
normal synchronous operations (meaning the client makes a request
and blocks until it get the result) introduce async operations based
on webhooks.

