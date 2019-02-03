
test_BD <- function() {
  # near an empirical value
  v <- Bernoulli_diff_stat(82, 200, 55, 100)$pValue
  empirical <- 0.02403 # from a large empirical run of extras/RateDiffs.Rmd
  RUnit::checkTrue(abs(v - empirical)<1e-4)

  # symmetry
  v1 <- Bernoulli_diff_stat(2000, 5000, 100, 200)$pValue
  v2 <- Bernoulli_diff_stat(100, 200, 2000, 5000)$pValue
  RUnit::checkTrue(abs(v1 - v2)<1e-7)

  # moderate continuity
  v1 <- Bernoulli_diff_stat(2000, 5000, 100, 200)$pValue
  v2 <- Bernoulli_diff_stat(2000, 5000, 100, 199)$pValue
  RUnit::checkTrue(abs(v1 - v2)<2e-3)
  v2 <- Bernoulli_diff_stat(2000, 4999, 100, 200)$pValue
  RUnit::checkTrue(abs(v1 - v2)<1e-4)

  invisible(NULL)
}
