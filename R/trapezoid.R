#' Trapezoid Rule
#'
#' @param f a function with a single argument x
#' @param range a vector specifying the lower and upper end points of the range of integration (a and b)
#' @param n the number of intervals for the Trapezoid approximation
#'
#' @return a single value which is the Trapezoid approximation to the integral
#' @export
#'
#' @examples
#' trapezoid(function(x){x^2},c(1,20),200)
#' 2666.362
trapezoid <- function(f, range, n){
  a <- range[1]
  b <- range[2]
  dx <- (b-a)/n
  x <- seq(a,b-dx,dx)
  i <- sum(((f(x)+f(x+dx))/2)*dx)
  return(i)
}
