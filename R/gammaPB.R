#' Generador Gamma(k, theta) por suma de exponenciales
#'
#' Para k entero, una Gamma(k, theta) es la suma de k
#' exponenciales independientes con tasa lambda = 1/theta.
#'
#' @param n Número de observaciones.
#' @param k Parámetro de forma (entero).
#' @param theta Parámetro de escala.
#' @return Vector de n observaciones Gamma(k, theta).
#' @examples
#' rgammaPB(1000, k = 10, theta = 0.5)
#' @export
rgammaPB <- function(n, k, theta){
  lambda <- 1 / theta
  replicate(n, sum(rexp(k, rate = lambda)))
}

#' Densidad Gamma(k, theta)
#' @param x Vector de valores.
#' @param k Parámetro de forma.
#' @param theta Parámetro de escala.
#' @return Vector con la densidad evaluada en x.
#' @export
dgammaPB <- function(x, k, theta){
  dgamma(x, shape = k, scale = theta)
}

#' Acumulada Gamma(k, theta)
#' @param x Vector de valores.
#' @param k Parámetro de forma.
#' @param theta Parámetro de escala.
#' @return Vector con la acumulada evaluada en x.
#' @export
pgammaPB <- function(x, k, theta){
  pgamma(x, shape = k, scale = theta)
}

#' Cuantil Gamma(k, theta)
#' @param p Vector de probabilidades.
#' @param k Parámetro de forma.
#' @param theta Parámetro de escala.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qgammaPB <- function(p, k, theta){
  qgamma(p, shape = k, scale = theta)
}
