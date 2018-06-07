test_that('fish_catch generates reasonable values',{
  #Create test dataframes
  #P
  species <- c('bluegill', 'bass', 'perch', 'trout', 'sturgeon')
  price <- c( 3, 5, 3, 7, 10 )
  P <- as_tibble(cbind(species, price))
  P$price <- as.numeric(P$price)

#C
  catch_Michigan<- c(500, 100, 400, 50, 10)
  catch_Superior <- c(300, 50, 400, 40, 1)
  C <- as_tibble(cbind(species, catch_Michigan, catch_Superior))
  C$catch_Michigan <- as.numeric(C$catch_Michigan)
  C$catch_Superior <- as.numeric(C$catch_Superior)

  E <- data.frame(fish_catch(P,C))

  #Expectations
  expect_that(E[1,3]<=E[1,5], is_true())
})
