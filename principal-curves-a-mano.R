## -----------------------------------------------------------------------------
## Curvas principales
##
## Ejemplo extraido de:
## www.r-bloggers.com/principal-curves-example-elements-of-statistical-learning/
## -----------------------------------------------------------------------------

library(animation)
ani.options(interval = 3)

## Generamos datos bivariados con dependencia no lineal
set.seed(42)
x1 <- seq(1, 10, 0.3)
w  <-  0.6067;
a0 <-  1.6345;
a1 <- -0.6235;
b1 <- -1.3501;
a2 <- -1.1622;
b2 <- -0.9443;
ee <- rnorm(length(x1), 0, 3/4)
x2 <- a0 + a1 * cos(x1 * w) + b1 * sin(x1 * w) + a2 * cos(2 * x1 * w) +
    b2 * sin(2 * x1 * w) + ee
x     <- scale(cbind(x1, x2))
alim  <- extendrange(x, f = 0.1)
alim_ <- range(x)

saveVideo({
    ## Scatterplot
    plot(x, pch = 19, col = "blue",
         main = "Datos", xlab = expression(x[1]), ylab = expression(x[2]),
         xlim = alim, ylim = alim)
    ## Inicializamos con la primera componente principal
    plot(x, pch = 19, col = "blue",
         main = "Paso inicial", xlab = expression(x[1]), ylab = expression(x[2]),
         xlim = alim, ylim = alim)
    svdx <- svd(x)                                    # calculo SVD
    clip(alim_[1], alim_[2], alim_[1], alim_[2])      # recorto el grafico de la recta
    with(svdx, abline(a = 0, b = v[2, 1] / v[1, 1],
                      lwd = 3, col = "orange"))       # dibujo la recta de la componente
                      ## Proyecciones (ortogonales) a la curva
                      z1 <- with(svdx, x %*% v[,1] %*% t(v[,1]))
                      segments(x0 =  x[, 1], y0 =  x[, 2],
                               x1 = z1[, 1], y1 = z1[, 2], lty = 2, lwd = 2)
                      ## Lambdas iniciales (longitud de arco asociada a las proyecciones
                      ## ortogonales de los datos sobre la curva
                      lam <- with(svdx, as.numeric(u[,1] * d[1]))
                      ## PASOS ITERATIVOS
                      maxIter <- 10
                      for(itr in 1:maxIter) {
                          ## PASO (A): fijo lambda y calculo la curva
                          ##
                          ## ajusto splines para las coordenadas de la curva principal
                          ## usando los lambdas iniciales.
                          ## Primera coordenada de la curva principal (indexada en lambda)
                          fit1 <- smooth.spline(x = lam, y = x[, 1], df = 4)
                          ## Segunda coordenada de la curva principal (indexada en lambda)
                          fit2 <- smooth.spline(x = lam, y = x[, 2], df = 4)
                          ## Plot de los datos y la curva principal
                          plot(x,
                               main = "PASO (A)",
                               xlab = expression(x[1]), ylab = expression(x[2]),
                               xlim = alim, ylim = alim, pch = 19, col = "blue")
                          seq_lam <- seq(min(lam), max(lam), length = 100)
                          lines(predict(fit1, seq_lam)$y, predict(fit2, seq_lam)$y, lwd = 3, col = 'orange')
                          ## show points along curve corresponding to original lambdas
                          z1 <- cbind(predict(fit1, lam)$y, predict(fit2, lam)$y)
                          segments(x0 =  x[, 1], y0 =  x[, 2],
                                   x1 = z1[, 1], y1 = z1[, 2], lty = 2, lwd = 2)
                          ## PASO (B): fijo la curva y calculo los nuevos lambdas
                          ##
                          ## busco los lambdas correspondientes a la proyeccion ortogonal
                          ## de los datos a la curva
                          euc_dist <- function(l, x, f1, f2) {
                              ## Evaluo la curva en un lambda
                              punto <- c(predict(f1, l)$y, predict(f2, l)$y)
                              ## Miro la distancia euclidea entre el dato x
                              ## y el punto de la curva
                              sum((punto - x)^2)
                          }
                          ## Con la curva dada por f1 y f2 (fija), para cada observacion
                          ## busco el mejor 'l', ie, 
                          lam <- apply(x, 1, function (x0) {
                              optimize(euc_dist,
                                       interval = extendrange(lam, f = 0.50),
                                       x = x0, f1 = fit1, f2 = fit2)$minimum
                          })
                          ## Proyecto los datos a los nuevos lambdas
                          plot(x,
                               main = "PASO (B)",
                               xlab = expression(x[1]), ylab = expression(x[2]),
                               xlim = alim, ylim = alim, pch = 19, col = "blue")
                          seq_lam <- seq(min(lam), max(lam), length = 100)
                          lines(predict(fit1, seq_lam)$y, predict(fit2, seq_lam)$y, lwd = 3, col = 'orange')
                          z1 <- cbind(predict(fit1, lam)$y, predict(fit2, lam)$y)
                          segments(x0 =  x[, 1], y0 =  x[, 2],
                                   x1 = z1[, 1], y1 = z1[, 2], lty = 2, lwd = 2)
                      }
})
