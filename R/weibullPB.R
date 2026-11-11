#' Generador Weibull(k, lambda) por método de la inversa
#'
#' Genera n observaciones de una Weibull con parámetro de forma k
#' y parámetro de escala lambda, usando el método de la inversa.
#'
#' @param n Número de observaciones.
#' @param k Parámetro de forma.
#' @param lambda Parámetro de escala.
#' @return Vector de n observaciones Weibull(k, lambda).
#' @examples
#' rweibullPB(1000, k = 3, lambda = 0.5)
#' @export
rweibullPB <- function(n, k, lambda){
  u <- runif(n)
  x <- lambda * (-log(1 - u))^(1/k)
  return(x)
}

#' Densidad Weibull(k, lambda)
#' @param x Vector de valores.
#' @param k Parámetro de forma.
#' @param lambda Parámetro de escala.
#' @return Vector con la densidad evaluada en x.
#' @export
dweibullPB <- function(x, k, lambda){
  ifelse(x >= 0,
         (k/lambda) * (x/lambda)^(k - 1) * exp(-(x/lambda)^k),
         0)
}

#' Acumulada Weibull(k, lambda)
#' @param x Vector de valores.
#' @param k Parámetro de forma.
#' @param lambda Parámetro de escala.
#' @return Vector con la acumulada evaluada en x.
#' @export
pweibullPB <- function(x, k, lambda){
  ifelse(x >= 0, 1 - exp(-(x/lambda)^k), 0)
}

#' Cuantil Weibull(k, lambda)
#' @param p Vector de probabilidades.
#' @param k Parámetro de forma.
#' @param lambda Parámetro de escala.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qweibullPB <- function(p, k, lambda){
  lambda * (-log(1 - p))^(1/k)
}
