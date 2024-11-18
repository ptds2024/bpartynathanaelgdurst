test_that("cone_radius_apply works correctly", {
  expect_equal(
    cone_radius_apply(c(-1, 5, 8.5, 9.5, 12)),
    c(0, 5 / 8, 1 + 1.5 * sin(0.5), 2.5 - 2 * cos(1.5), 0),
    tolerance = 1e-1
  )
})

test_that("cone_radius_for works correctly", {
  expect_equal(
    cone_radius_for(c(-1, 5, 8.5, 9.5, 12)),
    c(0, 5 / 8, 1 + 1.5 * sin(0.5), 2.5 - 2 * cos(1.5), 0),
    tolerance = 1e-1
  )
})

test_that("cone_radius_vectorized_direct works correctly", {
  expect_equal(
    cone_radius_vectorized_direct(c(-1, 5, 8.5, 9.5, 12)),
    c(0, 5 / 8, 1 + 1.5 * sin(0.5), 2.5 - 2 * cos(1.5), 0),
    tolerance = 1e-1
  )
})

test_that("cone_radius works correctly", {
  expect_equal(cone_radius(-1), 0, tolerance = 1e-1)
  expect_equal(cone_radius(5), 5 / 8, tolerance = 1e-1)
  expect_equal(cone_radius(8.5), 1 + 1.5 * sin(0.5), tolerance = 1e-1)
  expect_equal(cone_radius(9.5), 2.5 - 2 * cos(1.5), tolerance = 1e-1)
  expect_equal(cone_radius(12), 0, tolerance = 1e-1)
})

test_that("cone_radius_map works correctly", {
  expect_equal(
    cone_radius_map(c(-1, 5, 8.5, 9.5, 12)),
    c(0, 5 / 8, 1 + 1.5 * sin(0.5), 2.5 - 2 * cos(1.5), 0),
    tolerance = 1e-1
  )
})

test_that("cone_radius_cpp works correctly", {
  expect_equal(
    cone_radius_cpp(c(-1, 5, 8.5, 9.5, 12)),
    c(0, 5 / 8, 1 + 1.5 * sin(0.5), 2.5 - 2 * cos(1.5), 0),
    tolerance = 1e-1
  )
})

test_that("benchmark_cone_radius works correctly", {
  benchmark <- benchmark_cone_radius(
    implementations = list(
      For_Loop = cone_radius_for,
      Vectorized = cone_radius_vectorized_direct
    ),
    x_values = seq(0, 10, length.out = 100),
    times = 10
  )
  expect_s3_class(benchmark, "microbenchmark")
  expect_true("expr" %in% names(benchmark))
})

test_that("cone_volume works correctly", {
  expect_equal(cone_volume(lower = 0, upper = 10),
               pi * integrate(function(x) cone_radius_cpp(x)^2, lower = 0, upper = 10)$value)
})

test_that("cone_surface throws an error for non-numeric lower input", {
  expect_error(
    cone_surface(lower = "text", upper = 10, epsilon = 1e-3),
    "All inputs \\(lower, upper, epsilon\\) must be numeric."
  )
})

test_that("run_app function exists and is callable", {
  expect_true(is.function(run_app))
})






