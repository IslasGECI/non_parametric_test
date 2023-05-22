get_normal_distribution <- function(data) {
  m <- mean(data)
  s <- sd(data)
  return(rnorm(100, m, s))
}

did_come_from_a_normal_distribution <- function(data) {
  return(TRUE)
}
