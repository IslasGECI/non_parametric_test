library(tidyverse)

describe("Are the data of humidity normal", {
  it("All datalogers", {
    humedity <- read_csv("/workdir/tests/data/Humedad.csv", show_col_types = FALSE) |>
      drop_na()
    for (i in 2:ncol(humedity)) {
      expect_false(did_come_from_a_normal_distribution(humedity[[i]]))
    }
  })
})
