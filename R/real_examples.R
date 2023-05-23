select_petrel_valley <- function(humedity_data) {
  humedity_data |>
    select(any_of(c("Date - Time", "TS10_A", "TS11_A", "TS12_N", "TS13_N")))
}

wide_to_longer <- function(sitio_data) {
  sitio_data |>
    pivot_longer(!"Date - Time", names_to = "dataloger", values_to = "humedity")
}
