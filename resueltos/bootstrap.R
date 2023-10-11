---
title: "Bootstrap"
output: html_document
date: "2023-10-11"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown
La cantidad de particulas que emite una fuente radiactiva en un minuto es una variable
aleatoria con distribucion de Poisson de parametro λ. Se midio la cantidad de emisiones en
intervalos de 1 minuto, obteniendo las siguientes 15 muestras

500  488  426 510  450  368  508  514  426 476  512  526  444  524  236
Por ej el estimador es 
```{r}
#x_2 = scan()
lambda_est_1ro = mean(x_2)
lambda_est_2do = (-1 + sqrt(1+4*mean(x_2^2)))/2 

```
```{r}
lambda_bootstrap_T1 = replicate(1000,{
  d_bootstrap = rpois(15,lambda_est_1ro)
  sd(d_bootstrap)
})
lambda_bootstrap_T2 = replicate(1000,{
  d_bootstrap = rpois(15,lambda_est_2do)
  sd(d_bootstrap)
})

hist(lambda_bootstrap_T1)
hist(lambda_bootstrap_T2)

```


## Including Plots

You can also embed plots, for example:

```{r}

```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
