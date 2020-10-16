
test_T1 <- function() {
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
  expect_true(abs(s4$pValue - expect)<1e-6)
  s5 <- sigr::wrapFTest(d,'pred','y')
  expect_true(abs(s5$pValue - expect)<1e-6)

  invisible(NULL)
}

test_T1()
