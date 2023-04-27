set.seed(0)
x <- matrix(rnorm(50 * 512), 50, 512)
d_model <- 512
num_heads <- 8
dff <- 2048

output <- transformer(x, d_model, num_heads, dff)

test_that("transform-test", {
  expect_equal(output[1],  -0.3096475, tolerance = 1)
})
