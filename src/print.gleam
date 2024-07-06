@external(erlang, "prometheus_text_format", "format")
pub fn print_as_text(registry_name registry_name: String) -> String

@external(erlang, "prometheus_protobuf_format", "format")
pub fn print_as_protobuf(registry_name registry_name: String) -> BitArray
