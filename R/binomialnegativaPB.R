#' Generador Binomial Negativa(size, prob) por método de la inversa
#'
#' Cuenta el número de fracasos antes de obtener size éxitos.
#'
#' @param n Número de observaciones.
#' @param size Número objetivo de éxitos.
#' @param prob Probabilidad de éxito.
#' @return Vector de n observaciones Binomial Negativa(size, prob).
#' @examples
#' rbnegativaPB(1000, size = 5, prob = 0.3)
#' @export
rbnegativaPB <- function(n, size, prob){
  u <- runif(n)
  qnbinom(u, size = size, prob = prob)
}

#' Densidad (masa) Binomial Negativa(size, prob)
#' @param x Vector de valores.
#' @param size Número objetivo de éxitos.
#' @param prob Probabilidad de éxito.
#' @return Vector con la masa evaluada en x.
#' @export
dbnegativaPB <- function(x, size, prob){
  dnbinom(x, size = size, prob = prob)
}

#' Acumulada Binomial Negativa(size, prob)
#' @param q Vector de valores.
#' @param size Número objetivo de éxitos.
#' @param prob Probabilidad de éxito.
#' @return Vector con la acumulada evaluada en q.
#' @export
pbnegativaPB <- function(q, size, prob){
  pnbinom(q, size = size, prob = prob)
}

#' Cuantil Binomial Negativa(size, prob)
#' @param p Vector de probabilidades.
#' @param size Número objetivo de éxitos.
#' @param prob Probabilidad de éxito.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qbnegativaPB <- function(p, size, prob){
  qnbinom(p, size = size, prob = prob)
}
