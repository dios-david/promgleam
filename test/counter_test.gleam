import promgleam/metrics/counter.{create_counter, increment_counter}

const registry_name = "counter_test_registry"

pub fn create_counter_test() {
  let assert Ok(_) =
    create_counter(
      registry: registry_name,
      name: "test",
      help: "test counter",
      labels: [],
    )

  let assert Error("Metric already exists") =
    create_counter(
      registry: registry_name,
      name: "test",
      help: "test counter",
      labels: [],
    )
}

pub fn counter_increment_test() {
  let assert Ok(_) =
    create_counter(
      registry: registry_name,
      name: "ok",
      help: "basic counter",
      labels: [],
    )
  let assert Ok(_) =
    increment_counter(registry: registry_name, name: "ok", labels: [], value: 1)
}

pub fn counter_increment_non_existing_counter_test() {
  let assert Error("Unknown metric: non_existing_counter") =
    increment_counter(
      registry: registry_name,
      name: "non_existing_counter",
      labels: [],
      value: 1,
    )
}

pub fn counter_increment_with_labels() {
  let assert Ok(_) =
    create_counter(
      registry: registry_name,
      name: "with_labels",
      help: "metric with labels",
      labels: ["wibble", "wobble"],
    )
  let assert Ok(_) =
    increment_counter(
      registry: registry_name,
      name: "with_labels",
      labels: ["A", "B"],
      value: 1,
    )

  let assert Error(
    "Invalid metric arity (labels mismatch): given 0, expected 2",
  ) =
    increment_counter(
      registry: registry_name,
      name: "with_labels",
      labels: [],
      value: 1,
    )

  let assert Error(
    "Invalid metric arity (labels mismatch): given 1, expected 2",
  ) =
    increment_counter(
      registry: registry_name,
      name: "with_labels",
      labels: ["A"],
      value: 1,
    )
}
