#' cod_growth
#'
#' Function to estimate cod growth at a given temperature (C)
#' @param a first variable in best fit third order polynomial of cod fish growth (for more details see references)
#' @param b second variable in best fit third order polynomial of cod fish growth (for more details see references)
#' @param c third variable in best fit third order polynomial of cod fish growth (for more details see references)
#' @param d first variable in best fit third order polynomial of cod fish growth (for more details see references)
#' @return Estimated growth rate for cod from the temperature range 0-20 degrees C
#' @Examples
#' # For experiemnt A, the variables for the best fit growth polynomial are:
#'   a <- -0.497
#'   b <- .1656
#'   c <- .08588
#'   d <- -0.004266
#' cod_growth(a,b,c,d)
#' @references doi:10.1016/j.aquaculture.2007.06.026
#' @author Nicol Parker

cod_growth = function(a,b,c,d){

  T <- 1:20
  G <- as_tibble(a+b*T+c*T^2+d*T^3)
  G$Temp_C <- T
  colnames(G) <- c('Growth_Rate', 'Temp_C')

  ggplot(G)+ geom_line(aes(x=Growth_Rate,y=Temp_C))
G
}

