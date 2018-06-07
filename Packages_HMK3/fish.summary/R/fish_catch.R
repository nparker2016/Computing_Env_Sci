#'fish_catch
#'
#'Function to evaluate wild caught fish species, price, and revenue in Lake Michigan and Lake Superior
#'
#' @param P prices of fish species of interest
#' @param C number of catch of each fish species at a given location
#' @param G optional parameter for graphical summary of fisheries revenue calculated from catch and price
#' @author Nicol Parker
#' @Examples
#' #Example data for the fish_catch function
#'
#'#obtain data for species and price
#'P <- Price_Data
#'
#'#Obtain catch data by  location
#'C <- Catch_Data
#'
#' fish_catch(P,C)


fish_catch = function(P,C,G){

  MaxFreq <- data.frame(catch_Michigan=as.character(NA), catch_Superior=as.character(NA))
    index <- which.max(C$catch_Michigan)
    MaxFreq$catch_Michigan <- C$species[index]
      index2 <- which.max(C$catch_Superior)
    MaxFreq$catch_Superior <-C$species[index2]
    colnames(MaxFreq) <- c('Max_Freq_Catch_Michigan','Max_Freq_Catch_Superior')


    a <- data.frame(dplyr::full_join(P, C, by='species'))
    Revenue_USD <- dplyr::summarise(a, Revenue_Michigan_USD=sum(price*catch_Michigan), Revenue_Superior_USD=sum(price*catch_Superior))



    Fisheries_Revenue_USD <- dplyr::summarise(Revenue_USD, Fisheries_Revenue_USD=sum(Revenue_USD[,]))

    output <- data.frame(MaxFreq, Revenue_USD, Fisheries_Revenue_USD, stringsAsFactors = FALSE)

  #optional output plot
    w <- data.frame(t(Revenue_USD))
    w$location <- c('Michigan', 'Superior')
    colnames(w) <- c('revenue','location')
    RevenueSummary <- ggplot2::ggplot(w) + ggplot2::geom_col(ggplot2::aes(x=location, y=revenue)) +
      ggplot2::ggtitle('Wild Caught Fish Revenue Summary - Lake Michigan and Lake Superior') +
      ggplot2::ylab('Revenue (USD)')  + ggplot2::xlab(' ')

    if(missing(G)){return(output)
    }else{
      return(list(output, RevenueSummary))
    }

}




