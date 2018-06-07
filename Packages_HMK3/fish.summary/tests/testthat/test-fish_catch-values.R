context('Output validity')
test_that('fish_catch generates reasonable values',{
  #Create test dataframes
  #P
  species <- c('bluegill', 'bass', 'perch', 'trout', 'sturgeon')
  price <- c( 3, 5, 3, 7, 10 )
  P <- data.frame(cbind(species, price))
  P$price <- as.numeric(P$price)

#C
  catch_Michigan<- c(500, 100, 400, 50, 10)
  catch_Superior <- c(300, 50, 400, 40, 1)
  C <- data.frame(cbind(species, catch_Michigan, catch_Superior))
  C$catch_Michigan <- as.numeric(C$catch_Michigan)
  C$catch_Superior <- as.numeric(C$catch_Superior)


  #Expectations
  expect_that(fish_catch(P,C)[1,3]<=fish_catch(P,C)[1,5], equals(TRUE))
})
