import metrics/gauge.{new_gauge, set_gauge}

const registry_name = "gauge_test_registry"

pub fn new_gauge_test() {
  let assert Ok(_) =
    new_gauge(
      registry: registry_name,
      name: "test",
      help: "test gauge",
      labels: [],
    )

  let assert Error("Metric already exists") =
    new_gauge(
      registry: registry_name,
      name: "test",
      help: "test gauge",
      labels: [],
    )
}

pub fn gauge_set_test() {
  let assert Ok(_) =
    new_gauge(
      registry: registry_name,
      name: "ok",
      help: "basic gauge",
      labels: [],
    )
  let assert Ok(_) =
    set_gauge(registry: registry_name, name: "ok", labels: [], value: 1)
}

pub fn gauge_set_non_existing_gauge_test() {
  let assert Error("Unknown metric: non_existing_gauge") =
    set_gauge(
      registry: registry_name,
      name: "non_existing_gauge",
      labels: [],
      value: 1,
    )
}

pub fn gauge_set_with_labels() {
  let assert Ok(_) =
    new_gauge(
      registry: registry_name,
      name: "with_labels",
      help: "metric with labels",
      labels: ["wibble", "wobble"],
    )
  let assert Ok(_) =
    set_gauge(
      registry: registry_name,
      name: "with_labels",
      labels: ["A", "B"],
      value: 1,
    )

  let assert Error(
    "Invalid metric arity (labels mismatch): given 0, expected 2",
  ) =
    set_gauge(
      registry: registry_name,
      name: "with_labels",
      labels: [],
      value: 1,
    )

  let assert Error(
    "Invalid metric arity (labels mismatch): given 1, expected 2",
  ) =
    set_gauge(
      registry: registry_name,
      name: "with_labels",
      labels: ["A"],
      value: 1,
    )
}
