//// A gauge is a metric that represents a single numerical value that can arbitrarily go
//// up and down.
////
//// Gauges are typically used for measured values like temperatures or current memory usage, but
//// also "counts" that can go up and down, like the number of concurrent requests.

import gleam/int.{to_string}
import internal/prometheus_error.{
  type PrometheusError, InvalidMetricArity, MfAlreadyExists, UnknownMetric,
}

@external(erlang, "ffi_gauge", "create_gauge")
fn ffi_create_gauge(
  registry: String,
  name: String,
  help: String,
  labels labels: List(String),
) -> Result(Nil, PrometheusError)

/// Creates a new Gauge metric.
///
/// # Examples
///
/// ```gleam
/// create_gauge(
///   registry: "default",
///   name: "cache_size",
///   help: "Number of items in the cache",
///   labels: [ "cache_name" ],
/// )
/// ```
pub fn create_gauge(
  registry registry: String,
  name name: String,
  help help: String,
  labels labels: List(String),
) -> Result(Nil, String) {
  let result = ffi_create_gauge(registry, name, help, labels)

  case result {
    Ok(_) -> Ok(Nil)
    Error(MfAlreadyExists(_, _)) -> Error("Metric already exists")
    _ -> Error("Unknown error")
  }
}

@external(erlang, "ffi_gauge", "set_gauge")
fn ffi_set_gauge(
  registry: String,
  name: String,
  labels: List(String),
  value: Int,
) -> Result(Nil, PrometheusError)

/// Sets the value of the Gauge.
///
/// # Examples
/// ```gleam
/// set_gauge(
///   registry: "default",
///   name: "cache_size",
///   labels: [ "image_cache" ],
///   value: 123,
/// )
/// ```
pub fn set_gauge(
  registry registry: String,
  name name: String,
  labels labels: List(String),
  value value: Int,
) -> Result(Nil, String) {
  let result = ffi_set_gauge(registry, name, labels, value)

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
