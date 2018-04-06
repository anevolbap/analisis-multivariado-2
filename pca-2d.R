## -------------
## Ejemplo en 2d
## -------------

library(mvtnorm)
set.seed(999)

## Genero una normal bivariada con alta correlacion
n_obs <- 1e2
media <- c(1, 2)
sigma <- matrix(c(2,1.9,1.9,2),2,2)
datos <- rmvnorm(n = n_obs, mean = media, sigma = sigma)

## Alta correlacion
cor(datos)

## Centro los datos
datos_media <- colMeans(datos)
datos_cent <- scale(datos, scale = FALSE)

## Plot
dev.new()
alim  <- c(-6,6)
plot(datos, pch = 19, col = "blue",
     main = "Datos originales",
     xlab = "x", ylab = "y", xlim = alim, ylim = alim)
points(datos_media[1], datos_media[2], pch = 19, col = "orange")
abline(h = datos_media[2], lty = 3)
abline(v = datos_media[1], lty = 3)
abline(h = 0, lty = 1, lwd = 2)
abline(v = 0, lty = 1, lwd = 2)

## Autovectores
autovectores <- eigen(cov(datos))$vectors
datos_pca    <- prcomp(datos, scale = FALSE)

datos_pca$rotation
autovectores

## Plot: datos, datos centrados, scores
dev.new()
par(mfrow = c(1, 3))
plot(datos, pch = 19, col = "blue",
     main = "Datos originales",
     xlab = "x", ylab = "y", xlim = alim, ylim = alim)
abline(h = 0, lty = 2, lwd = 2)
abline(v = 0, lty = 2, lwd = 2)
plot(datos_cent, pch = 19, col = "blue",
     main = "Datos centrados",
     xlab = "x", ylab = "y", xlim = alim, ylim = alim)
abline(h = 0, lty = 2, lwd = 2)
abline(v = 0, lty = 2, lwd = 2)
plot(datos_cent %*% datos_pca$rotation,
     main = "Datos centrados y 'rotados'",
     pch = 19, col = "blue",
     xlab = "PCA1", ylab = "PCA2", xlim = alim, ylim = alim)
abline(h = 0, lty = 2, lwd = 2)
abline(v = 0, lty = 2, lwd = 2)

## Correlacion de las componentes principales
cor(datos_cent %*% datos_pca$rotation)

## Scree plot (varianza explicada)
dev.new()
plot(datos_pca)
screeplot(datos_pca)

## Me quedo solo con la primera coordenada
PCA1 <- (datos_cent %*% datos_pca$rotation)[, 1]
PCA2 <- (datos_cent %*% datos_pca$rotation)[, 2]

PCA1 == datos_pca$x[,1]
PCA2 == datos_pca$x[,2]

## Reconstruyo mis observaciones usando solo PCA1
datos_rec_PCA1 <- sapply(PCA1, function(x) -x * autovectores[, 1])
datos_rec_PCA2 <- sapply(PCA2, function(x) -x * autovectores[, 2])

## Plot: datos reconstruidos
dev.new()
par(mfrow = c(1, 2))
plot(datos, pch = 19, col = "blue",
     main = "Datos reconstruidos con PCA1",
     xlab = "x", ylab = "y", xlim = alim, ylim = alim)
points(t(datos_rec_PCA1 + datos_media), col = "magenta", pch = 19)
abline(h = 0, lty = 2, lwd = 2)
abline(v = 0, lty = 2, lwd = 2)
plot(datos, pch = 19, col = "blue",
     main = "Datos reconstruidos con PCA1 y PCA2",
     xlab = "x", ylab = "y", xlim = alim, ylim = alim)
points(t(datos_rec_PCA2 + datos_rec_PCA1 + datos_media), col = "magenta", pch = 18)
abline(h = 0, lty = 2, lwd = 2)
abline(v = 0, lty = 2, lwd = 2)
