## ----------------------------------------------------------------------------
## Ejemplo(s) con el paquete 'princurve'
## ----------------------------------------------------------------------------

## Ejemplo 1: de la ayuda
library(princurve)

x <- runif(100,-1,1)
x <- cbind(x, x ^ 2 + rnorm(100, sd = 0.1))
fit1 <- principal.curve(x, plot = TRUE)
fit2 <- principal.curve(x, plot = TRUE, smoother = "lowess")
whiskers <- function(from, to)
    segments(from[, 1], from[, 2], to[, 1], to[, 2])
par(mfrow = c(1, 2))
plot(fit1, xlim = c(-1,1), ylim = extendrange(x[,2]), main = "Splines")
points(x, col = "blue", pch = 19)
lines(fit1)
whiskers(x, fit1$s)
plot(fit2, xlim = c(-1,1), ylim = extendrange(x[,2]), main = "Lowess")
lines(fit2)
points(x, col = "blue", pch = 19)
points(fit2, col = 'orange')
whiskers(x, fit2$s)

## Ejemplo 2: circulo (presentacion de Graciela)
library(animation)
ani.options(interval = .1)

## Generamos datos bivariados con dependencia no lineal
set.seed(999)
nn <- 100
nu <- runif(nn, 0, pi * 2)
e1 <- rnorm(nn)
e2 <- rnorm(nn)
x1 <- 5 * sin(nu) + e1
x2 <- 5 * cos(nu) + e2
x  <- scale(cbind(x1, x2))
alim  <- extendrange(x, f = 0.1)
alim_ <- range(x)

saveHTML({
    ## Calculo la curva principal
    plot(x,pch=19, col = "green")
    par(pch = 19)
    pc <- principal.curve(x = as.matrix(x),             # x tiene que ser una matriz 
                          start = NULL,                 # si NULL usa la 1ra comp principal
                          thresh = 0.001,               # define la convergencia
                          plot.true = TRUE,             # plotea las iteraciones
                          maxit = 200,                  # maximo de iteraciones
                          stretch = 2,                  # agranda el dominio de la curva (creo)
                          smoother = 'periodic.lowess', # tipo de suavizado
                          trace = FALSE                 # informacion de cada iteracion
                          )
})
pc$nbrOfIterations

## Ejemplo 3: tortugas
library(princurve)
library(Flury)
data(turtles)

## Data
head(data)
summary(turtles)
datos  <- split(turtles[c('Length', 'Width')], turtles$Gender, drop = TRUE)
male   <- 10 * log(datos$Male)
female <- 10 * log(datos$Female)
matriz <- female

## Calculo la curva principal
pc <- principal.curve(x = as.matrix(matriz),      # x tiene que ser una matriz 
                      start = NULL,               # si NULL usa la 1ra comp principal
                      thresh = 0.001,             # define la convergencia
                      plot.true = TRUE,           # plotea las iteraciones
                      maxit = 100,                # maximo de iteraciones
                      stretch = 2,                # agranda el dominio de la curva (creo)
                      smoother = 'smooth.spline', # tipo de suavizado
                      trace = TRUE                # informacion de cada iteracion
                      )

plot(pc, pch = 19, main = "Curva principal")
points(matriz, pch = 19, col = 'blue')
points(pc, col = 'orange')
lines(pc, lwd = 2)

## Comparo con la primera componente principal (lineal)
sigma <- cov(matriz)
media <- colMeans(matriz)
pca   <- eigen(sigma)
val   <- pca$values
vec   <- pca$vectors
vec_1 <- pca$vectors[, 1]

## Plot comparativo
plot(pc, pch = 19)
points(matriz, pch = 19, col = 'blue')
points(pc, col = 'orange')
lines(pc, lwd = 2)
legend("topleft", legend = c("Curva principal", "Componente principal"),
       lty = c(1,2), lwd = 2, col = c("black", "red"))
segments(media[1]-10*vec_1[1], media[2]-10*vec_1[2],
         media[1]+10*vec_1[1], media[2]+10*vec_1[2],
         lwd = 2, col = "red", lty = 2)






