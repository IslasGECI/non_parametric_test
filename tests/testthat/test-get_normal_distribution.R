describe("We can get normal distribution from a data", {
  data <- rnorm(100, 3, 2)
  new_data <- get_normal_distribution(data)
  p_value <- ks.test(data, new_data)$p_value
  expect_true(p_value > 0.05)
})
