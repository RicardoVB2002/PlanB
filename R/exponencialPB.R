#' Generador Exponencial(lambda) por método de la inversa
#'
#' @param n Número de observaciones.
#' @param lambda Tasa de la exponencial (lambda > 0).
#' @return Vector de n observaciones Exponencial(lambda).
#' @examples
#' rexponencialPB(1000, lambda = 2)
#' @export
rexponencialPB <- function(n, lambda){
  u <- runif(n)
  x <- -log(1 - u) / lambda
  return(x)
}

#' Densidad Exponencial(lambda)
#' @param x Vector de valores.
#' @param lambda Tasa de la exponencial.
#' @return Vector con f(x).
#' @export
dexponencialPB <- function(x, lambda){
  ifelse(x >= 0, lambda * exp(-lambda * x), 0)
}

#' Acumulada Exponencial(lambda)
#' @param x Vector de valores.
#' @param lambda Tasa de la exponencial.
#' @return Vector con F(x).
#' @export
pexponencialPB <- function(x, lambda){
  ifelse(x >= 0, 1 - exp(-lambda * x), 0)
}

#' Cuantil Exponencial(lambda)
#' @param p Vector de probabilidades.
#' @param lambda Tasa de la exponencial.
#' @return Vector con F^{-1}(p).
#' @export
qexponencialPB <- function(p, lambda){
  -log(1 - p) / lambda
}


