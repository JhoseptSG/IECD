x = c(1,2,3,4,5)

tita_mom <- function(x) {
  y = x**2
  alfa = mean(x)**2 /(mean(y)-mean(x)**2)
  lambda = mean(x) / (mean(y)-mean(x)**2)
  return (c(alfa,lambda))
}


tita_mv <- function(x, n = 50 ) { 
  
  alfa = tita_mom(x)[1]
  i = 0
  
  g <- function (x,alfa) { 
    res = n * log(alfa/mean(x)) + sum(log(x)) - digamma(alfa) 
    return(res)
  }
  
  gprima <- function(x,alfa) { 
    res = - n * trigamma(alfa) - n / alfa
    return(res)
  }
  
   while (i <= n) {
     alfa = alfa - g(x,alfa)/gprima(x,alfa) 
     i = i + 1 
   }
  lambda = alfa / mean(x)
  return (c(alfa,lambda))
}

tita_mv(x,50)



