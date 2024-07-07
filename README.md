# PromGleam

[![Package Version](https://img.shields.io/hexpm/v/promgleam)](https://hex.pm/packages/promgleam)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/promgleam/)

A [Prometheus](https://prometheus.io) client library for Gleam, based on [prometheus.erl](https://github.com/deadtrickster/prometheus.erl)

## Installation

```sh
gleam add promgleam
```

## Examples

### Creating and incrementing a `Counter`

> A counter is a cumulative metric that represents a single monotonically increasing counter whose value can only increase or be reset to zero on restart. For example, you can use a counter to represent the number of requests served, tasks completed, or errors.
[(Source)](https://prometheus.io/docs/concepts/metric_types/#counter)

```gleam
import promgleam/metrics/counter.{create_counter, increment_counter}

create_counter(
  registry: "default",
  name: "http_requests_total",
  help: "Total number of HTTP requests",
  labels: [ "method", "route", "status" ],
)

increment_counter(
  registry: "default",
  name: "http_requests_total",
  labels: [ "GET", "/", "200" ],
  value: 1,
)
```

### Creating and setting a `Gauge`

> A gauge is a metric that represents a single numerical value that can arbitrarily go up and down.
Gauges are typically used for measured values like temperatures or current memory usage, but also "counts" that can go up and down, like the number of concurrent requests.
[(Source)](https://prometheus.io/docs/concepts/metric_types/#gauge)

```gleam
import promgleam/metrics/gauge.{create_gauge, set_gauge}

create_gauge(
  registry: "default",
  name: "cache_size",
  help: "Number of items in the cache",
  labels: [ "cache_name" ],
)

set_gauge(
  registry: "default",
  name: "cache_size",
  labels: [ "image_cache" ],
  value: 123,
)
```

### Creating and observing a `Histogram`

> A histogram samples observations (usually things like request durations or response sizes) and counts them in configurable buckets. It also provides a sum of all observed values.
[(Source)](https://prometheus.io/docs/concepts/metric_types/#histogram)

```gleam
import promgleam/metrics/histogram.{create_histogram, observe_histogram}

create_histogram(
  registry: "default",
  name: "http_request_duration_seconds",
  help: "Duration of HTTP requests in seconds",
  labels: [ "method", "route", "status" ],
  buckets: [ 0.1, 0.25, 0.5, 1.0, 1.5 ],
)

observe_histogram(
  registry: "default",
  name: "http_request_duration_seconds",
  labels: [ "GET", "/", "200" ],
  value: 0.23,
)
```

Both `measure_histogram` and `measure_histogram_seconds` can be used to wrap a function to
automatically observe the execution time whenever the function is invoked, rather than manually
measuring it and reporting it with `observe_histogram`.

```gleam
import promgleam/metrics/histogram.{measure_histogram}

fn my_function_to_measure() {
  use <- measure_histogram(
    registry: "default",
    name: "function_execution_time",
    labels: [ "my_function_to_measure" ],
  )

  // Do something slow here
}
```

### Generating buckets

This library provides utility functions to create `buckets` for a Histogram:

```gleam
import promgleam/buckets.{exponential, linear}

exponential(start: 1.0, factor: 2, count: 5) // Ok([1.0, 2.0, 4.0, 8.0, 10.0])
linear(start: 1.0, step: 3.0, count: 4) // Ok([1.0, 4.0, 7.0, 10.0])
```
### Printing the contents of a metric registry

This can be done by using one of the `print_as` functions:
- `print_as_text` - Serialises the registry using the [Prometheus text-based format](https://github.com/prometheus/docs/blob/main/content/docs/instrumenting/exposition_formats.md#text-format-example) into a `String`
- `print_as_prometheus` - Serialises the registry using the [Prometheus Protobuf format](https://github.com/prometheus/docs/blob/main/content/docs/instrumenting/exposition_formats.md#protobuf-format) into a `BitArray`

```gleam
import promgleam/registry.{print_as_text, print_as_protobuf}

print_as_text(registry_name: "default")
print_as_protobuf(registry_name: "default")
```

---
Further documentation can be found at <https://hexdocs.pm/promgleam>.
