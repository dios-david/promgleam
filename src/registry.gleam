//// Functions to interact with a metric registry.

/// Serialises the registry using the [Prometheus text-based format](https://github.com/prometheus/docs/blob/main/content/docs/instrumenting/exposition_formats.md#text-format-example).
@external(erlang, "prometheus_text_format", "format")
pub fn print_as_text(registry_name registry_name: String) -> String

/// Serialises the registry using the [Prometheus Protobuf format](https://github.com/prometheus/docs/blob/main/content/docs/instrumenting/exposition_formats.md#protobuf-format).
@external(erlang, "prometheus_protobuf_format", "format")
pub fn print_as_protobuf(registry_name registry_name: String) -> BitArray

/// Removes all the previously registered metrics from a registry.
@external(erlang, "prometheus_registry", "clear")
pub fn clear_registry(name: String) -> Nil
