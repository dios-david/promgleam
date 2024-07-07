//// A histogram samples observations (usually things like request durations or response sizes)
//// and counts them in configurable buckets. It also provides a sum of all observed values.

import gleam/int.{to_float, to_string}
import gleam/result.{replace}
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

type Timestamp

@external(erlang, "erlang", "timestamp")
fn ffi_timestmap() -> Timestamp

@external(erlang, "timer", "now_diff")
fn ffi_time_diff(a: Timestamp, b: Timestamp) -> Int

/// Measures a function execution time in milliseconds and observes that in the Histogram.
///
/// # Examples
///
/// ```gleam
/// fn my_function_to_measure() {
///   use <- measure_histogram(
///     registry: "default",
///     name: "function_execution_time",
///     labels: [ "my_function_to_measure" ],
///   )
///
///   // Do something slow here
///
///   "return_value"
/// }
///
/// let assert Ok("return_value") = my_function_to_measure()
/// ```
///
/// ```gleam
/// fn my_function_to_measure() {
///   // Do something slow here
///
///   "return_value"
/// }
///
/// let assert Ok("return_value") =
///   my_function_to_measure
///   |> measure_histogram(
///     registry: "default",
///     name: "function_execution_time",
///     labels: [ "my_function_to_measure" ],
///     func: _
///   )
/// ```
pub fn measure_histogram(
  registry registry: String,
  name name: String,
  labels labels: List(String),
  func func: fn() -> anything,
) -> Result(anything, String) {
  let start = ffi_timestmap()
  let return_value = func()
  let time_taken = to_float(ffi_time_diff(ffi_timestmap(), start))

  replace(observe_histogram(registry, name, labels, time_taken), return_value)
}

/// Measures a function execution time in seconds and observes that in the Histogram.
///
/// # Examples
///
/// ```gleam
/// fn my_function_to_measure() {
///   use <- measure_histogram_seconds(
///     registry: "default",
///     name: "function_execution_time",
///     labels: [ "my_function_to_measure" ],
///   )
///
///   // Do something slow here
///
///   "return_value"
/// }
///
/// let assert Ok("return_value") = my_function_to_measure()
/// ```
///
/// ```gleam
/// fn my_function_to_measure() {
///   // Do something slow here
///
///   "return_value"
/// }
///
/// let assert Ok("return_value") =
///   my_function_to_measure
///   |> measure_histogram_seconds(
///     registry: "default",
///     name: "function_execution_time",
///     labels: [ "my_function_to_measure" ],
///     func: _
///   )
/// ```
pub fn measure_histogram_seconds(
  registry registry: String,
  name name: String,
  labels labels: List(String),
  func func: fn() -> anything,
) -> Result(anything, String) {
  let start = ffi_timestmap()
  let return_value = func()
  let time_taken = to_float(ffi_time_diff(ffi_timestmap(), start)) /. 1000.0

  replace(observe_histogram(registry, name, labels, time_taken), return_value)
}
