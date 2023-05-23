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

describe("Test if the data came from normal distribution", {
  it("First example", {
    data <- rnorm(100, 3, 4)
    expect_true(did_come_from_a_normal_distribution(data))
  })
  it("Second example", {
    data <- runif(150, 1, 150)
    expect_false(did_come_from_a_normal_distribution(data))
  })
  it("First example", {
    data <- rnorm(100, 7, 1)
    expect_true(did_come_from_a_normal_distribution(data))
  })
})

describe("Test if the data came from the same distribution", {
  it("First example", {
    data_a <- rnorm(100, 3, 4)
    data_b <- rnorm(100, 3, 4)
    expect_true(did_come_from_the_same_distribution(data_a, data_b))
  })
  it("Second example", {
    data_a <- rnorm(100, 7, 3)
    data_b <- rnorm(100, 3, 4)
    expect_false(did_come_from_the_same_distribution(data_a, data_b))
  })
  it("Third example", {
    data_a <- rnorm(100, 7, 3)
    data_b <- rnorm(100, 7, 3)
    expect_true(did_come_from_the_same_distribution(data_a, data_b))
  })
})
