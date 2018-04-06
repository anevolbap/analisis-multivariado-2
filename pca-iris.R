## ---------------------------------
## Ejemplo de PCA: iris - princomp()
## ---------------------------------

## Miro los datos
data(iris)
head(iris)
summary(iris)

## Varianzas
apply(iris[-5], 2, var)

## PCA
datos    <- iris[-c(1,51,101),-5]
log_iris <- log(datos)
iris_pca <- princomp(log_iris, cor = TRUE)
summary(iris_pca)

## Loadings
loadings <- iris_pca$loadings
par(mfrow = c(1,2))
barplot(loadings[,1], ylim = c(-1,0.6), main = "PCA1")
barplot(loadings[,2], ylim = c(-1,0.6), main = "PCA2")

## Scores
head(iris_pca$scores)
plot(iris_pca$scores[, 1:2], pch = 19, main = "Scores - iris")

## Scree plot
par(mfrow = c(1, 2))
screeplot(iris_pca, main = "Scree plot - iris")
plot(iris_pca, type = "l")

## Miro solo la PCA1
stripchart(iris_pca$scores[, 1], #method = "jitter",
           pch = 19,
           main = "Scores - iris", xlab = "PCA1")
legend("topleft", legend = levels(iris$Species),
       col = unique(iris$Species), pch = 19)

## Miro solo la PCA1 coloreada por especie
plot(iris_pca$scores[, 1:2], col = iris$Species,
     pch = 19, main = "Scores - iris")
legend("topleft", legend = levels(iris$Species),
       col = unique(iris$Species), pch = 19)

## Predecir nuevas observaciones
nuevas <- predict(iris_pca, newdata = log(iris[c(1,51,101),-5]))

plot(iris_pca$scores[, 1:2], col = iris$Species,
     pch = 19, main = "Scores - iris")
points(nuevas[, 1:2], col = "orange", pch = 10, cex = 2.5)
legend("topleft", legend = c(levels(iris$Species), "observacion nueva"),
       col = c(unique(iris$Species), "orange"),
       pch = c(19, 19, 19, 10))

## Biplot
biplot(iris_pca, main = "Biplot - iris")
