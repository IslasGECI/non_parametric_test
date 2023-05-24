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

describe("select_petrel_valley", {
  it("Naturals", {
    humedity <- read_csv("/workdir/tests/data/Humedad_naturales.csv", show_col_types = FALSE) |>
      drop_na()
    petrel_valley <- humedity |>
      select_petrel_valley()
    obtained_names <- names(petrel_valley)
    expected_names <- c("Date - Time", "TS12_N", "TS13_N")
    expect_equal(obtained_names, expected_names)
  })
  it("Naturals and artificials", {
    humedity <- read_csv("/workdir/tests/data/Humedad.csv", show_col_types = FALSE) |>
      drop_na()
    petrel_valley <- humedity |>
      select_petrel_valley()
    obtained_names <- names(petrel_valley)
    expected_names <- c("Date - Time", "TS10_A", "TS11_A", "TS12_N", "TS13_N")
    expect_equal(obtained_names, expected_names)
  })
})

describe("tidy: wide to longer", {
  it("Natural", {
    humedity <- read_csv("/workdir/tests/data/Humedad.csv", show_col_types = FALSE) |>
      drop_na()
    petrel_valley <- humedity |>
      select_petrel_valley()
    longer <- wide_to_longer(petrel_valley)
    obtained_name <- names(longer)
    expected_name <- c("Date - Time", "dataloger", "humedity")
    expect_equal(obtained_name, expected_name)
    obtained_dataloger <- unique(longer$dataloger)
    expected_dataloger <- c("TS12_N", "TS13_N")
    expect_equal(obtained_dataloger, expected_dataloger)
  })
})
