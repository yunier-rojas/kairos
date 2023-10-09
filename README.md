## Kairos

Kairos is a very specific HTTP proxy. Its purpose is to allow APIs
to migrate to webhook operations without introducing breaking changes
to the public. It does that by transforming legacy requests into
webhook operations and then waits and send the result when it is
available.

## Use case

### Background

In modern software development, there is a growing need for asynchronous communication to improve system performance,
scalability, and responsiveness. However, many existing APIs and clients are built on synchronous request-response
patterns. Transitioning these APIs to fully asynchronous webhooks can be challenging, as it requires significant changes
to the API structure and may disrupt existing clients. To address this issue, Kairos, an HTTP proxy server, is designed
to facilitate the migration of synchronous APIs to asynchronous webhooks seamlessly.

### Problem Statement

Imagine a scenario where an organization wants to transition its existing synchronous APIs to asynchronous webhooks
without causing disruptions for the clients currently using these APIs. This transition involves the following
challenges:

- Existing clients are designed for synchronous communication and may not handle asynchronous callbacks efficiently.
- The organization needs to maintain backward compatibility to prevent breaking changes for existing clients.

### Kairos Proxy Server

**Objective:**

The primary goal of Kairos Proxy Server is to enable a smooth migration from synchronous APIs to asynchronous webhooks
while preserving compatibility with existing clients.

**How it works:**

**a. Synchronous to Asynchronous Transformation:**
   - Kairos intercepts incoming synchronous API requests from clients.
   - It transforms these requests into asynchronous webhook operations.
   - The original synchronous request is kept open until a webhook is received.

**b. Webhook Operation Handling:**
   - Kairos forwards the transformed request to the target API or service that supports asynchronous webhooks.
   - It listens for the webhook callback from the target service.

**c. Wait and Send Mechanism:**
   - Kairos holds the client's request until the webhook callback is received from the target service.
   - Once the callback is received, Kairos sends the result back to the original client as a response.

**d. Backward Compatibility:**
   - Existing clients continue to send synchronous requests as before.
   - Kairos ensures that these clients receive a response promptly, maintaining the illusion of synchronous communication.
   - Clients are not required to make any changes to their code or workflow.

**e. Gradual Migration:**
   - The organization can transition APIs to support webhooks at their own pace.
   - As each API or service is updated to support webhooks, Kairos can be configured to handle requests for that specific API asynchronously.

**4. Benefits:**

- Seamless Transition: Kairos allows organizations to transition to asynchronous communication gradually, reducing the risk of breaking existing clients.
- Improved Performance: The use of asynchronous webhooks can enhance system performance and responsiveness.
- Minimal Client Impact: Existing clients can continue to function without modification while taking advantage of asynchronous capabilities.
- Future-Proofing: Kairos sets the foundation for the organization to adopt modern, asynchronous communication patterns.

**5. Conclusion:**

Kairos Proxy serves as a component for organizations looking to modernize their APIs by transitioning
from synchronous to asynchronous communication. It enables this transition without disrupting existing clients and
ensures that the organization can adapt to evolving software architecture trends while maintaining backward
compatibility.


## Local development

Kairos provides `kairos.config.example` as an example configuration for the proxy to route request to the testing API.

```bash
cp kairos.config.example kairos.config
```

Make uses `rebar3` to start Kairos application to used it locally.

```bash
make shell
```



```bash
# breaking-change webhook-based API example
# This is an example of how a normal webhook API works.
# It is meant as a testing operation for the transformation
# between async API calls and webhook based.

curl -X POST --location "http://localhost:8082/api/webhook/test" \
    -H "X-Webhook-Url: https://httpbin.org/post" \
    -H "Content-Type: application/json" \
    -d "{
          \"data\": {
            \"a\": 1,
            \"b\": 2
          }
        }"
```


```bash
# Example of transforming a legacy API call into the new webhook-based.
# This transformation is completely transparent to the client to ensure
# backward compatibility with any previous version.

curl -X POST --location "http://localhost:8080/api/sync/test" \
    -H "Content-Type: application/json" \
    -d "{
          \"data\": {
            \"a\": 1,
            \"b\": 2
          }
        }"
```
