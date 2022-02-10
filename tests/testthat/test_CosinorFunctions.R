library(ActCR)
context("ActCosinor and ActExtendCosinor")
test_that("ActCosinor is consistent irrespective of epoch", {
  skip_on_cran()

  ActCosDummy = function(epochSizeSeconds) {
    N = 1440 * (60 / epochSizeSeconds) # Number of epochs per day
    time = seq(1 / N, 7, by = 1 / N)
    counts = sin(time * 2 * pi) + 10
    return(ActCosinor(x = counts, window = 1440 / N))
  }

  coef5 = ActCosDummy(epochSizeSeconds = 5)
  coef60 = ActCosDummy(epochSizeSeconds = 60)
  coef300 = ActCosDummy(epochSizeSeconds = 300)

  expect_equal(coef300, coef5)
  expect_equal(coef5, coef60)
})

test_that("ActExtendCosinor is consistent irrespective of epoch", {
  skip_on_cran()

  ActExtCosDummy = function(epochSizeSeconds) {
    N = 1440 * (60 / epochSizeSeconds) # Number of epochs per day
    time = seq(1 / N, 7, by = 1 / N)
    counts = sin(time * 2 * pi) + 10
    return(ActExtendCosinor(x = counts, window = 1440 / N))
  }

  coef5 = ActExtCosDummy(epochSizeSeconds = 5)
  coef60 = ActExtCosDummy(epochSizeSeconds = 60)
  coef300 = ActExtCosDummy(epochSizeSeconds = 300)

  # Vars we expect to be consistent at 1 decimal place
  varn = c("minimum", "amp", "alpha", "beta", "acrotime",
                               "UpMesor", "DownMesor", "MESOR", "ndays")
  expect_equal(round(unlist(coef300[varn]), digits = 1),
               round(unlist(coef5[varn]), digits = 1))
  expect_equal(round(unlist(coef5[varn]), digits = 1),
               round(unlist(coef60[varn]), digits = 1))
})
