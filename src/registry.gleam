import never.{type Never}

@external(erlang, "prometheus_registry", "clear")
pub fn clear_registry(name: String) -> Never
