import promgleam/buckets.{exponential, linear}

pub fn exponential_buckets_test() {
  let assert Ok([1.0, 2.0, 4.0, 8.0, 16.0]) =
    exponential(start: 1.0, factor: 2, count: 5)

  let assert Error("Invalid buckets: Buckets count should be positive") =
    exponential(start: 1.0, factor: 2, count: -2)

  let assert Error("Invalid buckets: Buckets start should be positive") =
    exponential(start: -1.0, factor: 2, count: 2)

  let assert Error("Invalid buckets: Buckets factor should be greater than 1") =
    exponential(start: 1.0, factor: 1, count: 2)
}

pub fn linear_buckets_test() {
  let assert Ok([1.0, 4.0, 7.0, 10.0]) = linear(start: 1.0, step: 3.0, count: 4)

  let assert Error("Invalid buckets: Buckets count should be positive") =
    linear(start: 1.0, step: 2.0, count: -2)
}
