#' Generador Binomial(size, prob) por método de la inversa
#'
#' @param n Número de observaciones.
#' @param size Número de ensayos.
#' @param prob Probabilidad de éxito en cada ensayo.
#' @return Vector de n observaciones Binomial(size, prob).
#' @examples
#' rbinomialPB(1000, size = 10, prob = 0.3)
#' @export
rbinomialPB <- function(n, size, prob){
  u <- runif(n)
  qbinom(u, size = size, prob = prob)
}

#' Densidad (masa) Binomial(size, prob)
#' @param x Vector de valores.
#' @param size Número de ensayos.
#' @param prob Probabilidad de éxito.
#' @return Vector con la masa evaluada en x.
#' @export
dbinomialPB <- function(x, size, prob){
  dbinom(x, size = size, prob = prob)
}

#' Acumulada Binomial(size, prob)
#' @param q Vector de valores.
#' @param size Número de ensayos.
#' @param prob Probabilidad de éxito.
#' @return Vector con la acumulada evaluada en q.
#' @export
pbinomialPB <- function(q, size, prob){
  pbinom(q, size = size, prob = prob)
}

#' Cuantil Binomial(size, prob)
#' @param p Vector de probabilidades.
#' @param size Número de ensayos.
#' @param prob Probabilidad de éxito.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qbinomialPB <- function(p, size, prob){
  qbinom(p, size = size, prob = prob)
}
