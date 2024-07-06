import buckets.{type Buckets}
import gleam/int.{to_string}
import gleam/string.{from_utf_codepoints}
import prometheus_error.{
  type PrometheusError, InvalidBuckets, InvalidMetricArity, MfAlreadyExists,
  NoBuckets, UnknownMetric,
}

@external(erlang, "ffi_histogram", "histogram_new")
fn ffi_new_histogram(
  registry: String,
  name: String,
  help: String,
  labels: List(String),
  buckets: Buckets,
) -> Result(Nil, PrometheusError)

pub fn new_histogram(
  registry registry: String,
  name name: String,
  help help: String,
  labels labels: List(String),
  buckets buckets: Buckets,
) -> Result(Nil, String) {
  let result = ffi_new_histogram(registry, name, help, labels, buckets)

  case result {
    Ok(_) -> Ok(Nil)
    Error(InvalidBuckets(_, reason)) ->
      Error("Invalid buckets: " <> from_utf_codepoints(reason))
    Error(MfAlreadyExists(_, _)) -> Error("Metric already exists")
    Error(NoBuckets(_)) -> Error("No buckets were provided")
    _ -> Error("Unknown error")
  }
}

@external(erlang, "ffi_histogram", "histogram_observe")
fn ffi_observe_histogram(
  registry: String,
  name: String,
  labels: List(String),
  value: Float,
) -> Result(Nil, PrometheusError)

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
