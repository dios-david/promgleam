//// A histogram samples observations (usually things like request durations or response sizes)
//// and counts them in configurable buckets. It also provides a sum of all observed values.

import gleam/int.{to_string}
import gleam/string.{from_utf_codepoints}
import promgleam/buckets.{type Buckets}
import promgleam/internal/prometheus_error.{
  type PrometheusError, InvalidBuckets, InvalidMetricArity, MfAlreadyExists,
  NoBuckets, UnknownMetric,
}

@external(erlang, "ffi_histogram", "create_histogram")
fn ffi_create_histogram(
  registry: String,
  name: String,
  help: String,
  labels: List(String),
  buckets: Buckets,
) -> Result(Nil, PrometheusError)

/// Creates a new Histogram metric.
///
/// # Examples
///
/// ```gleam
/// create_histogram(
///   registry: "default",
///   name: "http_request_duration_seconds",
///   help: "Duration of HTTP requests in seconds",
///   labels: [ "method", "route", "status" ],
///   buckets: [ 0.1, 0.25, 0.5, 1.0, 1.5 ],
/// )
/// ```
pub fn create_histogram(
  registry registry: String,
  name name: String,
  help help: String,
  labels labels: List(String),
  buckets buckets: Buckets,
) -> Result(Nil, String) {
  let result = ffi_create_histogram(registry, name, help, labels, buckets)

  case result {
    Ok(_) -> Ok(Nil)
    Error(InvalidBuckets(_, reason)) ->
      Error("Invalid buckets: " <> from_utf_codepoints(reason))
    Error(MfAlreadyExists(_, _)) -> Error("Metric already exists")
    Error(NoBuckets(_)) -> Error("No buckets were provided")
    _ -> Error("Unknown error")
  }
}

@external(erlang, "ffi_histogram", "observe_histogram")
fn ffi_observe_histogram(
  registry: String,
  name: String,
  labels: List(String),
  value: Float,
) -> Result(Nil, PrometheusError)

/// Observes a value in the Histogram.
///
/// # Examples
///
/// ```gleam
/// observe_histogram(
///   registry: "default",
///   name: "http_request_duration_seconds",
///   labels: [ "GET", "/", "200" ],
///   value: 0.23,
/// )
/// ```
pub fn observe_histogram(
  registry registry: String,
  name name: String,
  labels labels: List(String),
  value value: Float,
) -> Result(Nil, String) {
  let result = ffi_observe_histogram(registry, name, labels, value)

  case result {
    Ok(_) -> Ok(Nil)
    Error(UnknownMetric(_, metric_name)) ->
      Error("Unknown metric: " <> metric_name)
    Error(InvalidMetricArity(given, expected)) ->
      Error(
        "Invalid metric arity (labels mismatch): given "
        <> to_string(given)
        <> ", expected "
        <> to_string(expected),
      )
    _ -> Error("Unknown error")
  }
}