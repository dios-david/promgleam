//// A counter is a cumulative metric that represents a single monotonically increasing counter whose
//// value can only increase or be reset to zero on restart. For example, you can use a counter to
//// represent the number of requests served, tasks completed, or errors.
////
//// Do not use a counter to expose a value that can decrease. For example, do not use a counter for
//// the number of currently running processes; instead use a gauge.

import gleam/int.{to_string}
import internal/prometheus_error.{
  type PrometheusError, InvalidMetricArity, MfAlreadyExists, UnknownMetric,
}

@external(erlang, "ffi_counter", "create_counter")
fn ffi_create_counter(
  registry: String,
  name: String,
  help: String,
  labels: List(String),
) -> Result(Nil, PrometheusError)

/// Creates a new Counter metric.
///
/// # Examples
///
/// ```gleam
/// create_counter(
///   registry: "default",
///   name: "http_requests_total",
///   help: "Total number of HTTP requests",
///   labels: [ "method", "route", "status" ],
/// )
/// ```
pub fn create_counter(
  registry registry: String,
  name name: String,
  help help: String,
  labels labels: List(String),
) -> Result(Nil, String) {
  let result = ffi_create_counter(registry, name, help, labels)

  case result {
    Ok(_) -> Ok(Nil)
    Error(MfAlreadyExists(_, _)) -> Error("Metric already exists")
    _ -> Error("Unknown error")
  }
}

@external(erlang, "ffi_counter", "increment_counter")
fn ffi_increment_counter(
  registry: String,
  name: String,
  labels: List(String),
  value: Int,
) -> Result(Nil, PrometheusError)

/// Increments the Counter with the given value.
///
/// # Examples
/// ```gleam
/// increment_counter(
///   registry: "default",
///   name: "http_requests_total",
///   labels: [ "GET", "/", "200" ],
///   value: 1,
/// )
/// ```
pub fn increment_counter(
  registry registry: String,
  name name: String,
  labels labels: List(String),
  value value: Int,
) -> Result(Nil, String) {
  let result = ffi_increment_counter(registry, name, labels, value)

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
