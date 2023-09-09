# Ejercicio 14 
# sea x1..xn una muestra aleatoria con distribucion exponencial con lambda = 1. 
# graficar la  ECDF vs CDF Real
n = 100
datos_exp = rexp(n, rate = 1)

ecdf_datos_exp = ecdf(datos_exp)

plot(ecdf_datos_exp)

x <- seq(0, max(datos), length.out = 1000)
y_real_exp = 1 - exp(- x)

plot(ecdf_datos_exp, col = 'blue', xlab = 'Valores', ylab = 'Probabilidad acumulada', main = 'ECDF vs. CDF Real')
lines(x, y_real_exp, col = 'red')
legend("topright", legend = c("ECDF", "CDF Real"), col = c('blue', 'red'), lty = 1)

# estimar el λ de la exponencial por momentos y emv. 
 
λ_mom = 1 / mean(datos_exp) # λ_emv = 1 / mean(datos_exp) coinciden los estimadores 

y_est_exp = 1 - exp(- λ_mom * x)
plot(ecdf_datos_exp, col = 'blue', xlab = 'Valores', ylab = 'Probabilidad acumulada', main = 'ECDF vs. CDF Real')
lines(x, y_real_exp, col = 'red')
lines(x, y_est_exp, col = 'green')
legend("topright", legend = c("ECDF", "CDF Real","CDF est"), col = c('blue', 'red','green'), lty = 1)

#ejercicio 15 


θ_0 = 3


# creo la funcion para calcular los estimadores con distintos n. 
n_vec = c(6,10,20,40,80,200)
θ_mon_mat <- matrix(0, nrow = 6, ncol = 1000)
θ_emv_mat <- matrix(0, nrow = 6, ncol = 1000)
θ_emv_ins_mat <- matrix(0, nrow = 6, ncol = 1000)
for (j in 1:6) {
n = n_vec[j]
θ_mon_vec = c()
θ_emv_vec = c()
θ_emv_ins_vec = c()
for (i in 1:1000 ) { 
  datos_uni = runif(n,0,θ_0) 
  
  θ_mon = 2 * mean(datos_uni)
  θ_emv = max(datos_uni)
  θ_emv_ins = θ_emv * (n+1)/n
  
  θ_mon_vec = append(θ_mon_vec,θ_mon)
  θ_emv_vec = append(θ_emv_vec,θ_emv)
  θ_emv_ins_vec = append(θ_emv_ins_vec,θ_emv_ins)
} 
θ_mon_mat[j, ] <- θ_mon_vec
θ_emv_mat[j, ] <- θ_emv_vec
θ_emv_ins_mat[j, ] <- θ_emv_ins_vec

}
# cambio el formato para facilitar la graficacion 


θ_mon_mat_T = t(θ_mon_mat)
θ_emv_mat_T = t(θ_emv_mat)
θ_emv_ins_mat_T = t(θ_emv_ins_mat)

# grafico los boxplots de los estimadores
boxplot(θ_emv_mat_T, notch = FALSE, names = n_vec,
        xlab = "tamaño de la muestra", ylab = "Valores", main = "Boxplot del EMV")
abline(h=θ_0, col = 'red')


boxplot(θ_mon_mat_T, notch = FALSE, names = n_vec,
        xlab = "tamaño de la muestra", ylab = "Valores", main = "Boxplot del Enom")
abline(h=θ_0, col = 'red')


boxplot(θ_emv_ins_mat_T, notch = FALSE, names = n_vec,
        xlab = "tamaño de la muestra", ylab = "Valores", main = "Boxplot del EMV sin sesgo")
abline(h=θ_0, col = 'red')


# grafico los ecm estimados vs las n. 

est_mom_ecm_mom <- numeric(6)
est_mom_ecm_emv <- numeric(6)
est_mom_ecm_emv_ins <- numeric(6)

for (i in 1:6) {
  est_mom_ecm_mom[i] = mean((θ_mon_mat[i,] - θ_0)^2)
  est_mom_ecm_emv[i] = mean((θ_emv_mat[i,]- θ_0)^2)
  est_mom_ecm_emv_ins[i] = mean((θ_emv_ins_mat[i,]- θ_0)^2)
}


plot(n_vec, est_mom_ecm_mom, col = 'red', type = 'b', pch = 19,
     xlab = "Tamaño de la muestra", ylab = "Valor", main = "Comparación de Estimadores")

# Agregar líneas para otros estimadores
points(n_vec, est_mom_ecm_emv, col = 'darkgreen', type = 'b', pch = 19)
points(n_vec, est_mom_ecm_emv_ins, col = 'blue', type = 'b', pch = 19)

# Agregar leyenda
legend("topright", legend = c("Estimador MOM", "Estimador EMV", "Estimador EMV_INS"),
       col = c("red", "darkgreen", "blue"), lty = 1, pch = c(19, 19, 19, 16))


