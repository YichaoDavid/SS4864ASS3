traperoid <- function(f, range, n){
  a <- range[1]
  b <- range[2]
  dx <- (b-a)/n
  x <- seq(a,b-dx,dx)
  i <- sum(((f(x)+f(x+dx))/2)*dx)
  return(i)
}
