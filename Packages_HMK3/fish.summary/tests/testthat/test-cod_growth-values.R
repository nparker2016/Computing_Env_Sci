test_that('cod_growth generates reasonable values',{
  a <- -0.497
  b <- .1656
  c <- .08588
  d <- -0.004266
  G <- cod_growth(a,b,c,d)
  expect_that(G$Growth_Rate[10]<6, is_true())
  expect_that(max(G$Temp_C)<30, is_true())
  expect_that((a+b+c+d)<1, is_true())
})
