##
## Test de cociente de maxima verosimilitud
## para igualdad de componentes principales
##

pca_equality_test <- function(X, r, h) {
    ## H_0: \lambda_{r+1} = ... = \lambda_{r+h}
    ## H_1: \lambda_{r+1} > ... > \lambda_{r+h}
    
    ## Spectral decomposition
    idx    <- (r + 1):(r + h)
    s2_val <- eigen(cov(X))$values[idx]
    
    ## Test statistic
    num <- prod(s2_val)
    den <- mean(s2_val)
    T   <- nrow(X) * (h * log(den) - log(num))

    ## p-value
    df    <- h * (h + 1) / 2 - 1
    p_val <- pchisq(T, df, lower.tail = FALSE)
    
    return (list(p = p_val, df = df, T = T,
                 lambda_hat = s2_val))
}

## Ejemplo de jueguete
library(mvtnorm)
n  <- 1000                     # comparar el rechazo con n=100
mu <- rep(1, 5)
s2 <- diag(c(1, 2, 3, 1, 1))
X  <- rmvnorm(n, mu, s2)
pca_equality_test(X, 2, 2)

## Ejemplo de la presentacion de Graciela
library(Flury)
data(microtus)
X <- microtus[1:43, -1] / 10
pca_equality_test(X, 2, 2)

