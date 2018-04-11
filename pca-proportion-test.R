##
## Test para la proporcion de varianza explicada
## por las componentes principales
##

pca_proportion_test <- function (X, q, p0 = 0.9,
                                 bilateral = TRUE,
                                 conf = 0.05) {
    
    ## Sample size and dimension
    n <- nrow(X)
    p <- ncol(X)
    
    ## Spectral decomposition
    s2_val <- eigen(cov(X))$values
    
    ## Test statistic
    sums  <- cumsum(s2_val)
    theta <- sums[q] - p0 * sums[p]
    B     <- sum((s2_val^2)[(q + 1):p])
    A     <- sum((s2_val^2)[1:q])
    sigma <- sqrt(2 * ((1 - p0)^2 * A + p0^2 * B))
    
    ## p-value
    if (bilateral) {
        T     <- sqrt(n) * abs(theta) / sigma
        p_val <- pnorm(T, lower.tail = FALSE) * 2
        ## Confidence interval
        margin   <- sigma / sqrt(n) * pnorm(1 - conf / 2)
        conf_int <-  theta + c(-1,1) * margin
    } else{
        T        <- sqrt(n) * theta / sigma
        p_val    <- pnorm(T, lower.tail = FALSE)
        conf_int <- NA
    }
   
    return (list(p = p_val, T = T,
                 conf_int = conf_int,
                 l_hat = s2_val))   
}

## Ejemplo de la presentacion de Graciela
library(Flury)
data(microtus)
X <- microtus[1:43, -1] / 10
pca_proportion_test(X, 2, 0.90, bilateral = TRUE)
