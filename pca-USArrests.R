## --------------------------
## Ejemplo PCA: USArrests
##
## http://uc-r.github.io/pca
## --------------------------

library(tidyverse)  # data manipulation and visualization
library(gridExtra)  # plot arrangement

## --- A mano ---

## Datos
data("USArrests")
head(USArrests, 10)
help(USArrests)

## Varianza de cada variable
apply(USArrests, 2, var)

## Centramos las variables
scaled_df <- apply(USArrests, 2, scale)
head(scaled_df)

## Diagonalizamos
arrests_cov   <- cov(scaled_df)
arrests_eigen <- eigen(arrests_cov)
str(arrests_eigen)

## Loadings
(phi <- arrests_eigen$vectors[, 1:2])

phi <- -phi
row.names(phi) <- c("Murder", "Assault", "UrbanPop", "Rape")
colnames(phi)  <- c("PC1", "PC2")
phi

## Scores
PC1 <- as.matrix(scaled_df) %*% phi[, 1]
PC2 <- as.matrix(scaled_df) %*% phi[, 2]

## Data frame con los scores
PC <- data.frame(State = row.names(USArrests), PC1, PC2)
head(PC)
##        State        PC1        PC2
## 1    Alabama  0.9756604 -1.1220012
## 2     Alaska  1.9305379 -1.0624269
## 3    Arizona  1.7454429  0.7384595
## 4   Arkansas -0.1399989 -1.1085423
## 5 California  2.4986128  1.5274267
## 6   Colorado  1.4993407  0.9776297

## Plot Principal Components for each State
ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = State), size = 3) +
  xlab("Primera componente principal") + 
  ylab("Segunda componente principal") + 
  ggtitle("Primeras dos componentes principales - USArrests")

## Proporcion de varianza explicada
PVE <- arrests_eigen$values / sum(arrests_eigen$values)
round(PVE, 2)
## [1] 0.62 0.25 0.09 0.04

## PVE (aka scree) plot
PVEplot <- qplot(c(1:4), PVE) + 
  geom_line() + 
  xlab("Componentes principales") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

## Cumulative PVE plot
cumPVE <- qplot(c(1:4), cumsum(PVE)) + 
  geom_line() + 
  xlab("Componentes principales") + 
  ylab(NULL) + 
  ggtitle("Scree Plot acumulado") +
  ylim(0,1)

grid.arrange(PVEplot, cumPVE, ncol = 2)

## Now that we’ve calculated the first and second principal components for
## each US state, we can plot them against each other and produce a
## two-dimensional view of the data. The first principal component (x-axis)
## roughly corresponds to the rate of serious crimes. States such as
## California, Florida, and Nevada have high rates of serious crimes, while
## states such as North Dakota and Vermont have far lower rates. The second
## component (y-axis) is roughly explained as urbanization, which implies
## that states such as Hawaii and California are highly urbanized, while
## Mississippi and the Carolinas are far less so. A state close to the
## origin, such as Indiana or Virginia, is close to average in both
## categories.

## --- Funciones PCA en R ---

### Funciones built-in

pca_result <- prcomp(USArrests, scale = TRUE)
names(pca_result)

## means
pca_result$center
##   Murder  Assault UrbanPop     Rape 
##    7.788  170.760   65.540   21.232

## standard deviations
pca_result$scale
##    Murder   Assault  UrbanPop      Rape 
##  4.355510 83.337661 14.474763  9.366385

pca_result$rotation
##                 PC1        PC2        PC3         PC4
## Murder   -0.5358995  0.4181809 -0.3412327  0.64922780
## Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
## UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
## Rape     -0.5434321 -0.1673186  0.8177779  0.08902432

pca_result$rotation <- -pca_result$rotation
pca_result$rotation
##                PC1        PC2        PC3         PC4
## Murder   0.5358995 -0.4181809  0.3412327 -0.64922780
## Assault  0.5831836 -0.1879856  0.2681484  0.74340748
## UrbanPop 0.2781909  0.8728062  0.3780158 -0.13387773
## Rape     0.5434321  0.1673186 -0.8177779 -0.08902432

pca_result$x <- - pca_result$x
head(pca_result$x)
##                   PC1        PC2         PC3          PC4
## Alabama     0.9756604 -1.1220012  0.43980366 -0.154696581
## Alaska      1.9305379 -1.0624269 -2.01950027  0.434175454
## Arizona     1.7454429  0.7384595 -0.05423025  0.826264240
## Arkansas   -0.1399989 -1.1085423 -0.11342217  0.180973554
## California  2.4986128  1.5274267 -0.59254100  0.338559240
## Colorado    1.4993407  0.9776297 -1.08400162 -0.001450164


biplot(pca_result, scale = 0)

pca_result$sdev
## [1] 1.5748783 0.9948694 0.5971291 0.4164494

(VE <- pca_result$sdev^2)
## [1] 2.4802416 0.9897652 0.3565632 0.1734301

PVE <- VE / sum(VE)
round(PVE, 2)
## [1] 0.62 0.25 0.09 0.04
