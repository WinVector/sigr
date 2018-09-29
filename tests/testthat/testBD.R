library('sigr')

context("Benoulli diff")

test_that("testBD: Test Works As Expected", {
  v <- Bernoulli_diff_dist(2000, 5000, 100, 200, 0.1)$pValue
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(2000, 5000, 100, 200)$pValue
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(2000, 5000, 100, 199)$pValue
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(2000, 5000, 100, 199, 0.1)$pValue
  testthat::expect(abs(v - 0.01)<0.01)

  v <- Bernoulli_diff_dist(100, 200, 2000, 5000, 0.1)$pValue
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(100, 200, 2000, 5000)$pValue
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(100, 199, 2000, 5000)$pValue
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(100, 199, 2000, 5000, 0.1)$pValue
  testthat::expect(abs(v - 0.01)<0.01)

  v <- Bernoulli_diff_dist(82, 200, 55, 100)$pValue
  empirical <- 0.02401745 # from extras/RateDiffs.Rmd
  testthat::expect(abs(v - empirical)<1e-3)
})