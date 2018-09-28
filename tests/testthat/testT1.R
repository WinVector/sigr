library('sigr')

context("Excercise Operations")

test_that("testT1: Test Works As Expected", {
  set.seed(25325)
  s1 <- sigr::getRenderingFormat()
  s2 <- sigr::wrapSignificance(1/300)
  s3 <- sigr::wrapFTestImpl(numdf=2,dendf=55,FValue=5.56)

  d <- data.frame(x=c(1,2,3,4,5,6,7,7),
                  y=c(1,1,2,2,3,3,4,4))
  model <- lm(y~x,data=d)
  # summary(model)
  expect <- 3.094301e-05
  d$pred <- predict(model,newdata=d)
  s4 <- sigr::wrapFTest(model)
  testthat::expect(abs(s4$pValue - expect)<1e-6)
  s5 <- sigr::wrapFTest(d,'pred','y')
  testthat::expect(abs(s5$pValue - expect)<1e-6)
})