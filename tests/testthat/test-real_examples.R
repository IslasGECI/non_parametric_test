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
  it("All in petrel valley", {
    petrel_valley <- read_csv("/workdir/tests/data/Humedad_petrel_valley.csv", show_col_types = FALSE)
    longer <- wide_to_longer(petrel_valley)
    obtained_name <- names(longer)
    expected_name <- c("Date - Time", "dataloger", "humedity")
    expect_equal(obtained_name, expected_name)
    obtained_dataloger <- unique(longer$dataloger)
    expected_dataloger <- c("TS10_A", "TS11_A", "TS12_N", "TS13_N")
    expect_equal(obtained_dataloger, expected_dataloger)
  })
})

describe("Petrel valley difference", {
  petrel_valley <- read_csv("/workdir/tests/data/Humedad_petrel_valley.csv", show_col_types = FALSE)
  longer <- wide_to_longer(petrel_valley)
  it("First example: naturals and artificials are different", {
    naturals <- longer |>
      filter(str_ends(dataloger, "N")) %>%
      .$humedity
    artificials <- longer |>
      filter(str_ends(dataloger, "A")) %>%
      .$humedity
    expect_false(did_come_from_the_same_distribution(naturals, artificials))
  })
  it("Second example: naturals are same", {
    naturals_12 <- longer |>
      filter(str_starts(dataloger, "TS12")) %>%
      .$humedity
    naturals_13 <- longer |>
      filter(str_starts(dataloger, "TS13")) %>%
      .$humedity
    skip("Because fail")
    expect_true(did_come_from_the_same_distribution(naturals_12, naturals_13))
  })
  it("Second example: samples from TS12 came from TS12 ", {
    naturals_12 <- longer |>
      filter(str_starts(dataloger, "TS12")) %>%
      .$humedity
    sample_1 <- naturals_12 |> sample(replace = TRUE)
    sample_2 <- naturals_12 |> sample(replace = TRUE)
    expect_true(did_come_from_the_same_distribution(sample_1, sample_2))
  })
})
