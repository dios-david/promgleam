//// Utility functions to perform common tasks which are often needed when dealing with metrics.

type Timestamp

@external(erlang, "erlang", "timestamp")
fn ffi_timestmap() -> Timestamp

@external(erlang, "timer", "now_diff")
fn ffi_time_diff(a: Timestamp, b: Timestamp) -> Int

/// Measures the execution time of a function in milliseconds, then returns that with the function
/// return value in a tuple.
///
/// # Examples
///
/// ```gleam
/// fn my_func() {
///   "wibble"
/// }
///
/// let assert #(time_taken, "wibble") = measure(my_func)
/// ```
pub fn measure(func: fn() -> anything) -> #(Int, anything) {
  let start = ffi_timestmap()
  let return_value = func()
  let time_taken = ffi_time_diff(ffi_timestmap(), start)

  #(time_taken, return_value)
}
