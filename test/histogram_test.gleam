import promgleam/metrics/histogram.{
  create_histogram, measure_histogram, measure_histogram_seconds,
  observe_histogram,
}

const registry_name = "histogram_test_registry"

pub fn create_histogram_test() {
  let assert Ok(_) =
    create_histogram(
      registry: registry_name,
      name: "test",
      help: "test histogram",
      labels: [],
      buckets: [0.1, 0.25, 0.5],
    )

  let assert Error("Metric already exists") =
    create_histogram(
      registry: registry_name,
      name: "test",
      help: "test histogram",
      labels: [],
      buckets: [1.0, 2.5, 5.0],
    )

  let assert Error("No buckets were provided") =
    create_histogram(
      registry: registry_name,
      name: "test2",
      help: "test histogram",
      labels: [],
      buckets: [],
    )

  let assert Error("Invalid buckets: buckets not sorted") =
    create_histogram(
      registry: registry_name,
      name: "test3",
      help: "test histogram",
      labels: [],
      buckets: [5.0, 2.0, 1.0],
    )
}

pub fn histogram_observe_test() {
  let assert Ok(_) =
    create_histogram(
      registry: registry_name,
      name: "ok",
      help: "basic histogram",
      labels: [],
      buckets: [1.0, 2.5, 5.0],
    )
  let assert Ok(_) =
    observe_histogram(
      registry: registry_name,
      name: "ok",
      labels: [],
      value: 3.45,
    )
}

pub fn histogram_observe_non_existing_histogram_test() {
  let assert Error("Unknown metric: non_existing_histogram") =
    observe_histogram(
      registry: registry_name,
      name: "non_existing_histogram",
      labels: [],
      value: 1.0,
    )
}

pub fn histogram_observe_with_labels() {
  let metric_name = "with_labels"

  let assert Ok(_) =
    create_histogram(
      registry: registry_name,
      name: metric_name,
      help: "metric with labels",
      labels: ["wibble", "wobble"],
      buckets: [1.0, 2.5, 5.0],
    )
  let assert Ok(_) =
    observe_histogram(
      registry: registry_name,
      name: metric_name,
      labels: ["A", "B"],
      value: 3.45,
    )

  let assert Error(
    "Invalid metric arity (labels mismatch): given 0, expected 2",
  ) =
    observe_histogram(
      registry: registry_name,
      name: metric_name,
      labels: [],
      value: 1.0,
    )

  let assert Error(
    "Invalid metric arity (labels mismatch): given 1, expected 2",
  ) =
    observe_histogram(
      registry: registry_name,
      name: metric_name,
      labels: ["A"],
      value: 1.0,
    )
}

pub fn histogram_measure_test() {
  let metric_name = "function_execution_time"

  let assert Ok(_) =
    create_histogram(
      registry: registry_name,
      name: metric_name,
      help: "function execution times",
      labels: [],
      buckets: [0.1, 0.5, 1.0],
    )

  let function_to_measure = fn() { "return_value" }

  let assert Ok("return_value") =
    function_to_measure
    |> measure_histogram(
      registry: registry_name,
      name: metric_name,
      labels: [],
      func: _,
    )

  let function_to_measure_with_use = fn() {
    use <- measure_histogram_seconds(
      registry: registry_name,
      name: metric_name,
      labels: [],
    )
    "return_value"
  }

  let assert Ok("return_value") = function_to_measure_with_use()
}
