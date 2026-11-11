#' Generador Poisson(lambda) por método de la inversa
#'
#' @param n Número de observaciones.
#' @param lambda Tasa media (lambda > 0).
#' @return Vector de n observaciones Poisson(lambda).
#' @examples
#' rpoissonPB(1000, lambda = 3)
#' @export
rpoissonPB <- function(n, lambda){
  u <- runif(n)
  qpois(u, lambda = lambda)
}

#' Densidad (masa) Poisson(lambda)
#' @param x Vector de valores.
#' @param lambda Tasa media.
#' @return Vector con la masa evaluada en x.
#' @export
dpoissonPB <- function(x, lambda){
  dpois(x, lambda = lambda)
}

#' Acumulada Poisson(lambda)
#' @param q Vector de valores.
#' @param lambda Tasa media.
#' @return Vector con la acumulada evaluada en q.
#' @export
ppoissonPB <- function(q, lambda){
  ppois(q, lambda = lambda)
}

#' Cuantil Poisson(lambda)
#' @param p Vector de probabilidades.
#' @param lambda Tasa media.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qpoissonPB <- function(p, lambda){
  qpois(p, lambda = lambda)
}
