context('Output validity')
test_that('cod_growth generates reasonable values',{
  a <- -0.497
  b <- .1656
  c <- .08588
  d <- -0.004266

  expect_that(cod_growth(a,b,c,d)$Growth_Rate[10]<6, is_true())
  expect_that(max(cod_growth(a,b,c,d)$Temp_C)<30, is_true())
})

