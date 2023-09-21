u = 4
sigma = 3
alfa = 0.95

IC_mu_var_conocida <- function(x, var_0, nivel) {
  alfa_2 = 1 - nivel
  a = qnorm(1 - alfa_2 / 2)
  n = length(x)
  res = c(mean(x) - a * sqrt(var_0 / n), mean(x) + a * sqrt(var_0 / n))
  return(res)
}

nrep = 1000
muchos_intervalos = replicate(nrep, {
  datos_normales = rnorm(5, mean = u, sd = sigma)
  IC_mu_var_conocida(datos_normales, sigma^2, alfa)  # Usar nivel en lugar de alfa
  
})

sum((muchos_intervalos[1,]<=u) & (muchos_intervalos[2,]>=u))


IC_mu_var_desconocida <- function(x,nivel) {
  alfa_2 = 1 - nivel
  n = length(x)
  a = qt(1-alfa_2/2,n-1)
  se_hat = sqrt(var(x)/n)
  
  length = length(x)
  res = c(mean(x) - a*se_hat, mean(x) + a*se_hat)
  return(res)
}

muchos_intervalos_desc = replicate(nrep,{
  datos_normales = rnorm(5, mean = u, sd = sigma)
  IC_mu_var_desconocida(datos_normales,alfa)
  
})


sum((muchos_intervalos_desc[1,]<=u) & (muchos_intervalos_desc[2,]>=u))





















