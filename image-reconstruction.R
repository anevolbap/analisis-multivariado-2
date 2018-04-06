##------------------------------------------
## Vector quantization con PCA
##------------------------------------------

library(readbitmap)

## --- Lena ---

## Cargo la foto original
im <- read.bitmap("lenna.png")
n  <- dim(im)[1]
m  <- dim(im)[2]

## Foto original
plot(0:1, 0:1, type = "n", xlab = "", ylab = "", axes = F)
rasterImage(im, 0, 0, 1, 1)

## La paso a lista de vectores de R^3
im_df <- data.frame(apply(im, 3, rbind))
dim(im_df)

## PCA
im_pca <- prcomp(im_df)
im_red <- t(im_pca$center - sapply(im_pca$x[,1], function (x) -x * im_pca$rotation[,1]))
im_red[im_red > 1] <- 1
im_red[im_red < 0] <- 0

clcol <- rgb(im_red[ ,1], im_red[ ,2], im_red[ ,3])#, max = 255)
par(mfrow = c(1,2))
plot(0:1, 0:1, type = "n", xlab = "", ylab = "", axes = F, main = "Original")
rasterImage(im, 0, 0, 1, 1)
plot(0:1, 0:1, type = "n", xlab = "", ylab = "", axes = F, main = "Reconstruida")
rasterImage(matrix(clcol, n, m), 0, 0, 1, 1)

cl <- kmeans(im_df, 2)
plot(im_pca$x, pch = 19, col = cl$cluster)

## --- Imagen satelital ---

## Cargo la foto original
im <- read.bitmap("sat-data.png")
n  <- dim(im)[1]
m  <- dim(im)[2]

## Foto original
plot(0:1, 0:1, type = "n", xlab = "", ylab = "", axes = F)
rasterImage(im, 0, 0, 1, 1)

## La paso a lista de vectores de R^3
im_df <- data.frame(apply(im, 3, rbind))
dim(im_df)

## PCA
im_pca <- prcomp(im_df)
im_red <- t(im_pca$center - sapply(im_pca$x[,1], function (x) -x * im_pca$rotation[,1]))
im_red[im_red > 1] <- 1
im_red[im_red < 0] <- 0

clcol <- rgb(im_red[ ,1], im_red[ ,2], im_red[ ,3])#, max = 255)
par(mfrow = c(1,2))
plot(0:1, 0:1, type = "n", xlab = "", ylab = "", axes = F, main = "Original")
rasterImage(im, 0, 0, 1, 1)
plot(0:1, 0:1, type = "n", xlab = "", ylab = "", axes = F, main = "Reconstruida")
rasterImage(matrix(clcol, n, m), 0, 0, 1, 1)

cl <- kmeans(im_df, 2)
plot(im_pca$x, pch = 19, col = cl$cluster)

rand
