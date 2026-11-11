#' Generador Beta(alpha, beta) usando qbeta
#'
#' La Beta no tiene inversa algebraica simple para alpha y beta
#' arbitrarios. Se usa qbeta() de R, que aplica el método de
#' la inversa numéricamente.
#'
#' @param n Número de observaciones.
#' @param alpha Parámetro de forma 1.
#' @param beta Parámetro de forma 2.
#' @return Vector de n observaciones Beta(alpha, beta).
#' @examples
#' rbetaPB(1000, alpha = 2, beta = 5)
#' @export
rbetaPB <- function(n, alpha, beta){
  u <- runif(n)
  qbeta(u, shape1 = alpha, shape2 = beta)
}

#' Densidad Beta(alpha, beta)
#' @param x Vector de valores.
#' @param alpha Parámetro de forma 1.
#' @param beta Parámetro de forma 2.
#' @return Vector con la densidad evaluada en x.
#' @export
dbetaPB <- function(x, alpha, beta){
  dbeta(x, shape1 = alpha, shape2 = beta)
}

#' Acumulada Beta(alpha, beta)
#' @param x Vector de valores.
#' @param alpha Parámetro de forma 1.
#' @param beta Parámetro de forma 2.
#' @return Vector con la acumulada evaluada en x.
#' @export
pbetaPB <- function(x, alpha, beta){
  pbeta(x, shape1 = alpha, shape2 = beta)
}

#' Cuantil Beta(alpha, beta)
#' @param p Vector de probabilidades.
#' @param alpha Parámetro de forma 1.
#' @param beta Parámetro de forma 2.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qbetaPB <- function(p, alpha, beta){
  qbeta(p, shape1 = alpha, shape2 = beta)
}
