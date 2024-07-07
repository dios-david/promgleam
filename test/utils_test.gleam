import promgleam/utils.{measure}

pub fn utils_measure_test() {
  let assert #(_, "hello") = measure(fn() { "hello" })
}
