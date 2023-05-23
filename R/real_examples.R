select_petrel_valley <- function(humedity_data) {
  humedity_data |>
    select(any_of(c("TS10_A", "TS11_A", "TS12_N", "TS13_N")))
}

wide_to_longer <- function(sitio_data) {
  tibble::tibble(
    "humedity" = c(0),
    "dataloger" = c(0)
  )
}
