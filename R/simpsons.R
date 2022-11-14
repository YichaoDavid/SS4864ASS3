simpsons <- function(f,range,n){
  a <- range[1]
  b <-range[2]
  dx <- (b-a)/n
  x <- seq(a+dx,b,dx)
  i <- sum(((f(x-dx)+4*f((2*x-dx)/2)+f(x))/6)*dx)
  return(i)
}
