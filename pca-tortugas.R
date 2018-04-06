###
### PCA: turtles dataset
###

library(Flury)
data(turtles)

## Data
head(data)
summary(turtles)

## Data
datos  <- split(turtles[c('Length', 'Width')], turtles$Gender, drop = TRUE)
male   <- datos$Male
female <- datos$Female

## --- Analizamos primero el grupo 'male'

## Skewness
par(mfrow = c(1:2))
boxplot(male, main = "Male")
boxplot(female, main = "Female")

## Transformo los datos
male       <- 10 * log(male)
male_sigma <- cov(male)
male_media <- colMeans(male)

## Asimetria
boxplot(male, main = "Male")

## Plot
plot(male,
     main = "Male",
     xlab = "10 * log(Largo)", ylab = "10 * log(Ancho)",
     xlim = c(45, 50), ylim = c(42.5, 47.5))
points(male_media[1], male_media[2], pch = 19, col = "blue")

## PCA
male_pca <- eigen(male_sigma)
male_val <- round(male_pca$values, 4)
male_vec <- round(male_pca$vectors, 4)

## Autovectores (ordenados por autovalor mayor)
vec_1 <- male_pca$vectors[, 1]
vec_2 <- male_pca$vectors[, 2]

## Centro las observaciones
male_center <- sweep(male, 2, male_media)

## Calculo scores
escores_1 <- t(vec_1) %*% t(male_center) # cuanto en la dir 1
escores_2 <- t(vec_2) %*% t(male_center) # cuanto en la dir 2

## Calculo las proyecciones de los datos
male_proj_1 <- t(escores_1) %*% vec_1
male_proj_2 <- t(escores_2) %*% vec_2

## Reconstruyo los datos a partir de las proyecciones
male_aprox <- sweep(male_proj, 2, male_media, FUN = "+")

## Reconstruyo usando ambas direcciones principales
male_full <- sweep(male_proj_1 + male_proj_2, 2, male_media, FUN = "+")

## Scatter plot con la media muestral y los datos reconstruidos
dir1 = vec_1 + male_media
dir2 = vec_2 + male_media
plot(male,
     xlab = "10 * log(Largo)", ylab = "10 * log(Ancho)",
     xlim = c(45,52), ylim = c(42.5,47.5))
points(male_media[1], male_media[2], pch = 19 , col = "black")
#points(male_full, col = 'pink')
lines(male_aprox, lwd = 3, col = "orange")
lines(male_media + seq(0,100,0.1)*dir1, col = "red")
#lines(rbind(male_media, dir2), col = "black")

male_media + seq(0,100,0.1)*dir1

### Female

## Transformo los datos
female       <- 10 * log(female)
female_sigma <- cov(female)
female_media <- colMeans(female)

## Asimetria
boxplot(female, main = "Female")

## Plot
plot(female,
     main = "Female",
     xlab = "10 * log(Largo)", ylab = "10 * log(Ancho)",
     xlim = c(45, 50), ylim = c(42.5, 47.5))
points(female_media[1], female_media[2], pch = 19, col = "blue")

## PCA
female_pca <- eigen(female_sigma)
female_val <- round(female_pca$values, 4)
female_vec <- round(female_pca$vectors, 4)

## Autovectores (ordenados por autovalor mayor)
vec_1 <- female_pca$vectors[, 1]
vec_2 <- female_pca$vectors[, 2]

## Centro las observaciones
female_center <- sweep(female, 2, female_media)

## Calculo scores
escores_1 <- t(vec_1) %*% t(female_center) # cuanto en la dir 1
escores_2 <- t(vec_2) %*% t(female_center) # cuanto en la dir 2

## Calculo las proyecciones de los datos
female_proj_1 <- t(escores_1) %*% vec_1
female_proj_2 <- t(escores_2) %*% vec_2

## Reconstruyo los datos a partir de las proyecciones
female_aprox <- sweep(female_proj, 2, female_media, FUN = "+")

## Reconstruyo usando ambas direcciones principales
female_full <- sweep(female_proj_1 + female_proj_2, 2, female_media, FUN = "+")

## Scatter plot con la media muestral y los datos reconstruidos
dir1 = vec_1 + female_media
dir2 = vec_2 + female_media
plot(female,
     xlab = "10 * log(Largo)", ylab = "10 * log(Ancho)",
     xlim = c(45,52), ylim = c(42.5,47.5))
points(female_media[1], female_media[2], pch = 19 , col = "black")
#points(female_full, col = 'pink')
lines(female_aprox, lwd = 3, col = "orange")
lines(female_media + seq(0,100,0.1)*dir1, col = "red")
#lines(rbind(female_media, dir2), col = "black")

female_media + seq(0,100,0.1)*dir1
