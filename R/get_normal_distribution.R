get_normal_distribution <- function(data) {
  m <- mean(data)
  s <- sd(data)
  return(rnorm(1000, m, s))
}

did_come_from_a_normal_distribution <- function(data) {
  swt <- shapiro.test(data)
  return(swt$p.value > 0.05)
}

did_come_from_the_same_distribution <- function(sample_a, sample_b) {
  kst <- ks.test(sample_a, sample_b)
  return(kst$p.value > 0.05)
}
