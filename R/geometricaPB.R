#' Generador Geométrica(prob) por método de la inversa
#'
#' Cuenta el número de fracasos antes del primer éxito.
#'
#' @param n Número de observaciones.
#' @param prob Probabilidad de éxito (0 < prob < 1).
#' @return Vector de n observaciones Geométrica(prob).
#' @examples
#' rgeometricaPB(1000, prob = 0.3)
#' @export
rgeometricaPB <- function(n, prob){
  u <- runif(n)
  qgeom(u, prob = prob)
}

#' Densidad (masa) Geométrica(prob)
#' @param x Vector de valores.
#' @param prob Probabilidad de éxito.
#' @return Vector con la masa evaluada en x.
#' @export
dgeometricaPB <- function(x, prob){
  dgeom(x, prob = prob)
}

#' Acumulada Geométrica(prob)
#' @param q Vector de valores.
#' @param prob Probabilidad de éxito.
#' @return Vector con la acumulada evaluada en q.
#' @export
pgeometricaPB <- function(q, prob){
  pgeom(q, prob = prob)
}

#' Cuantil Geométrica(prob)
#' @param p Vector de probabilidades.
#' @param prob Probabilidad de éxito.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qgeometricaPB <- function(p, prob){
  qgeom(p, prob = prob)
}
