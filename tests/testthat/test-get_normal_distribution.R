describe("We can get normal distribution from a data", {
  it("First example", {
    data <- rnorm(100, 3, 2)
    new_data <- get_normal_distribution(data)
    p_value <- ks.test(data, new_data)$p.value
    expect_true(p_value > 0.05)
  })
  it("Second example", {
    data <- rnorm(100, 5, 1)
    new_data <- get_normal_distribution(data)
    p_value <- ks.test(data, new_data)$p.value
    expect_true(p_value > 0.05)
  })
  it("Third example", {
    data <- rnorm(100, 7, 2)
    new_data <- get_normal_distribution(data)
    p_value <- ks.test(data, new_data)$p.value
    expect_true(p_value > 0.05)
  })
})
