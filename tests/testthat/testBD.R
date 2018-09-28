library('sigr')

context("Benoulli diff")

test_that("testBD: Test Works As Expected", {
  v <- Bernoulli_diff_dist(2000, 5000, 100, 200, 0.1)$testres$test_sig
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(2000, 5000, 100, 200)$testres$test_sig
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(2000, 5000, 100, 199)$testres$test_sig
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(2000, 5000, 100, 199, 0.1)$testres$test_sig
  testthat::expect(abs(v - 0.01)<0.01)

  v <- Bernoulli_diff_dist(100, 200, 2000, 5000, 0.1)$testres$test_sig
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(100, 200, 2000, 5000)$testres$test_sig
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(100, 199, 2000, 5000)$testres$test_sig
  testthat::expect(abs(v - 0.01)<0.01)
  v <- Bernoulli_diff_dist(100, 199, 2000, 5000, 0.1)$testres$test_sig
  testthat::expect(abs(v - 0.01)<0.01)

})