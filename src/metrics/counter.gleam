import gleam/int.{to_string}
import prometheus_error.{
  type PrometheusError, InvalidMetricArity, MfAlreadyExists, UnknownMetric,
}

@external(erlang, "ffi_counter", "counter_new")
fn ffi_new_counter(
  registry: String,
  name: String,
  help: String,
  labels: List(String),
) -> Result(Nil, PrometheusError)

pub fn new_counter(
  registry registry: String,
  name name: String,
  help help: String,
  labels labels: List(String),
) -> Result(Nil, String) {
  let result = ffi_new_counter(registry, name, help, labels)

  case result {
    Ok(_) -> Ok(Nil)
    Error(MfAlreadyExists(_, _)) -> Error("Metric already exists")
    _ -> Error("Unknown error")
  }
}

@external(erlang, "ffi_counter", "counter_inc")
fn ffi_inc_counter(
  registry: String,
  name: String,
  labels: List(String),
  value: Int,
) -> Result(Nil, PrometheusError)

pub fn inc_counter(
  registry registry: String,
  name name: String,
  labels labels: List(String),
  value value: Int,
) -> Result(Nil, String) {
  let result = ffi_inc_counter(registry, name, labels, value)

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
