# Cowboy opentelemetry plugin

Add `cowboy_otel_h` first to your cowboy stream handlers, it will pick up propagated spans to cowboy,
and surround the request with a span.
Also add `cowboy_otel_middleware` after your `cowboy_router` middlewares, it will attach the created
span from the stream process in the process executing middlewares and your cowboy handler, so subspans are connected.

```erlang
    ProtocolOpts = #{
        stream_handlers => [cowboy_otel_h, cowboy_stream_h],
        middlewares => [cowboy_router, cowboy_otel_middleware, cowboy_handler],
        ...
    }
```

## Semantic convention

Goal is to follow https://opentelemetry.io/docs/specs/semconv/http/http-spans/

## Sampler

The `cowboy_otel_sampler` supports dropping traces with by any named attribute matching a regexp.

If a single attribute match, then the span is dropped.

Here is a way to apply it only for root spans, i.e. if it is a health check.

```
{opentelemetry, [
    {sampler, {parent_based, #{root => {cowboy_otel_sampler, #{'http.route' => <<"^/livez">>}}}}}
    ]}
```


## Tests

FIXME: Add test-suite  

## Release

FIXME: Add github build  
FIXME: Make hex release  
