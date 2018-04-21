##
## Biplot
##

my_biplot <- function (data) {
    ## Centering
    data <- scale(data, scale = FALSE)
    
    ## PCA
    pca_data   <- eigen(cov(data))
    pca_vec    <- pca_data$vectors
    pca_val    <- sqrt(pca_data$values)
    pca_scores <- data %*% pca_vec
    
    ## Factorization
    H <- pca_vec %*% diag(pca_val)
    G <- pca_scores %*% diag(1 / pca_val)
    
    ## Low-rank approximation
    r  <- 2
    H2 <- H[, 1:r]
    G2 <- G[, 1:r]

    ## Plot: individuals
    par(mar = par("mar") + c(0, 0, 1, 0))
    glim1 <- extendrange(G2[, 1], f= 0.1)
    hlim1 <- extendrange(H2[, 1], f= 0.1)
    glim2 <- extendrange(G2[, 2], f= 0.1)
    hlim2 <- extendrange(H2[, 2], f= 0.1)
    plot(G2, pch = 19, col = 'blue',
         xlim = glim1, ylim = glim2,
         xlab = "PCA1", ylab = "PCA2",
         main = "Biplot") 
    text(G2, labels = rownames(data), pos = 1)
    
    ## Creo un nuevo plot, mantengo la ventana
    par(new = TRUE)
    dev.hold()
    on.exit(dev.flush(), add = TRUE)
    
    ## Ajusto los limites para que coincidan los origenes
    expand <- 1
    ratio <- max(hlim1 / glim1, hlim2 / glim2) / expand

    ## Plot: variables
    colores <- 1:(ncol(data))+10
    plot(H2, axes = FALSE, type = "n",
         xlab = "", ylab = "",
         xlim = glim1 * ratio, ylim = glim2 * ratio)
    arrows(0, 0, angle = 15, length = 0.08,
           x1 = H2[, 1], y1 = H2[, 2],
           lwd = 2,
           col = colores)
    axis(3)
    axis(4)
    text(H2, labels = colnames(data), pos = 2)
    legend("bottomleft", legend = colnames(data), col = colores, pch = 20)
    abline(h=0, lty = 3)
    abline(v=0, lty = 3)
}


library(Flury)
data(microtus)
my_biplot(microtus[1:43, -1]/10)
