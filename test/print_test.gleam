import birdie.{snap}
import buckets.{exponential}
import metrics/counter.{inc_counter, new_counter}
import metrics/gauge.{new_gauge, set_gauge}
import metrics/histogram.{new_histogram, observe_histogram}
import pprint.{format}
import print.{print_as_protobuf, print_as_text}
import registry.{clear_registry}

const counter_name = "my_counter"

const gauge_name = "my_gauge"

const histogram_name = "my_histogram"

pub fn print_as_text_test() {
  let registry_name = "print_as_text_test_registry"

  clear_registry(registry_name)

  let assert Ok(_) =
    new_counter(
      registry: registry_name,
      name: counter_name,
      help: "my counter help text",
      labels: ["wibble", "wobble"],
    )
  let assert Ok(_) =
    inc_counter(
      registry: registry_name,
      name: counter_name,
      labels: ["A", "B"],
      value: 1,
    )
  let assert Ok(_) =
    inc_counter(
      registry: registry_name,
      name: counter_name,
      labels: ["C", "D"],
      value: 2,
    )

  let assert Ok(_) =
    new_gauge(
      registry: registry_name,
      name: gauge_name,
      help: "my gauge help text",
      labels: ["wibble", "wobble"],
    )
  let assert Ok(_) =
    set_gauge(
      registry: registry_name,
      name: gauge_name,
      labels: ["A", "B"],
      value: 1,
    )

  let assert Ok(buckets) = exponential(1.0, 2, 3)

  let assert Ok(_) =
    new_histogram(
      registry: registry_name,
      name: histogram_name,
      help: "my histogram help text",
      labels: ["wibble", "wobble"],
      buckets: buckets,
    )
  let assert Ok(_) =
    observe_histogram(
      registry: registry_name,
      name: histogram_name,
      labels: ["A", "B"],
      value: 3.45,
    )

  print_as_text(registry_name)
  |> snap(title: "print_as_text_test")
}

pub fn print_as_protobuf_test() {
  let registry_name = "print_as_protobuf_test_registry"

  clear_registry(registry_name)

  let assert Ok(_) =
    new_counter(
      registry: registry_name,
      name: counter_name,
      help: "metric with labels",
      labels: ["wibble", "wobble"],
    )
  let assert Ok(_) =
    inc_counter(
      registry: registry_name,
      name: counter_name,
      labels: ["A", "B"],
      value: 1,
    )
  let assert Ok(_) =
    inc_counter(
      registry: registry_name,
      name: counter_name,
      labels: ["C", "D"],
      value: 2,
    )

  print_as_protobuf(registry_name)
  |> format
  |> snap(title: "print_as_protobuf_test")
}
