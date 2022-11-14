riemann <- function(f, range, n) {
  a <- range[1]
  b <- range[2]
  dx <- (b - a)/n
  x <- seq(a+dx, b, dx)
  I <- sum(f(x) * dx)
  return(I)
}
