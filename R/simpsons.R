#' Simpson's Rule
#'
#' @param f a function with a single argument x
#' @param range a vector specifying the lower and upper end points of the range of integration (a and b)
#' @param n the number of intervals for the Simpsons approximation
#'
#' @return a single value which is the Simpsons approximation to the integral
#' @export
#'
#' @examples > simpsons(function(x){1/x},c(1,2),500)
#' 0.6931472
simpsons <- function(f,range,n){
  a <- range[1]
  b <-range[2]
  dx <- (b-a)/n
  x <- seq(a+dx,b,dx)
  i <- sum(((f(x-dx)+4*f((2*x-dx)/2)+f(x))/6)*dx)
  return(i)
}
