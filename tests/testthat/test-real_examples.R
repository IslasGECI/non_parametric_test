library(tidyverse)

describe("Are the data of humidity normal", {
  it("TS1_N", {
    humedity <- read_csv("/workdir/tests/data/Humedad_naturales.csv", show_col_types = FALSE)
  })
})
