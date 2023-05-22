get_normal_distribution <- function(data) {
  m <- mean(data)
  s <- sd(data)
  return(rnorm(1000, m, s))
}

did_come_from_a_normal_distribution <- function(data) {
  kst <- shapiro.test(data)
  return(kst$p.value > 0.05)
}
