//// Utility functions to generate buckets for Histogram metrics.

import gleam/string.{from_utf_codepoints}
import promgleam/internal/prometheus_error.{type PrometheusError, InvalidValue}

pub type Buckets =
  List(Float)

@external(erlang, "ffi_buckets", "exponential")
fn ffi_exponential(
  start: Float,
  factor: Int,
  count: Int,
) -> Result(Buckets, PrometheusError)

/// Creates `count` buckets, where the lowest bucket has an upper bound of `start` and each
/// following bucket's upper bound is `factor` times the previous bucket's upper bound.
///
/// # Examples
///
/// ```gleam
/// let assert Ok([1.0, 2.0, 4.0, 8.0]) = exponential(start: 1.0, factor: 2, count: 4)
/// ```
pub fn exponential(
  start start: Float,
  factor factor: Int,
  count count: Int,
) -> Result(Buckets, String) {
  let result = ffi_exponential(start, factor, count)

  case result {
    Ok(buckets) -> Ok(buckets)
    Error(InvalidValue(_, reason)) ->
      Error("Invalid buckets: " <> from_utf_codepoints(reason))
    _ -> Error("Unknown error")
  }
}

@external(erlang, "ffi_buckets", "linear")
fn ffi_linear(
  start: Float,
  step: Float,
  count: Int,
) -> Result(Buckets, PrometheusError)

/// Creates `count` buckets, each `step` wide, where the lowest bucket has an upper bound of
/// `start`.
///
/// # Examples
///
/// ```gleam
/// let assert Ok([1.0, 4.0, 7.0, 10.0]) = linear(start: 1.0, step: 3.0, count: 4)
/// ```
pub fn linear(
  start start: Float,
  step step: Float,
  count count: Int,
) -> Result(Buckets, String) {
  let result = ffi_linear(start, step, count)

  case result {
    Ok(buckets) -> Ok(buckets)
    Error(InvalidValue(_, reason)) ->
      Error("Invalid buckets: " <> from_utf_codepoints(reason))
    _ -> Error("Unknown error")
  }
}
