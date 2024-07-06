import internal/never.{type Never}

pub type PrometheusError {
  InvalidBuckets(buckets: List(Float), reason: List(UtfCodepoint))
  InvalidMetricArity(given: Int, expected: Int)
  InvalidValue(value: Never, reason: List(UtfCodepoint))
  MfAlreadyExists(registry: Never, metric_name: String)
  NoBuckets(buckets: List(Float))
  UnknownMetric(registry: Never, metric_name: String)
  UnknownError
}
