get_normal_distribution <- function(data) {
  m <- mean(data)
  s <- sd(data)
  return(rnorm(100, m, s))
}
