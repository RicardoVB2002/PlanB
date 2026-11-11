#' Generador Bernoulli(prob) por método de la inversa
#'
#' @param n Número de observaciones.
#' @param prob Probabilidad de éxito (0 < prob < 1).
#' @return Vector de n observaciones Bernoulli(prob).
#' @examples
#' rbernoulliPB(1000, prob = 0.3)
#' @export
rbernoulliPB <- function(n, prob){
  u <- runif(n)
  as.integer(u > 1 - prob)
}

#' Densidad (masa) Bernoulli(prob)
#' @param x Vector de valores (0 o 1).
#' @param prob Probabilidad de éxito.
#' @return Vector con la masa evaluada en x.
#' @export
dbernoulliPB <- function(x, prob){
  ifelse(x == 1, prob, ifelse(x == 0, 1 - prob, 0))
}

#' Acumulada Bernoulli(prob)
#' @param q Vector de valores.
#' @param prob Probabilidad de éxito.
#' @return Vector con la acumulada evaluada en q.
#' @export
pbernoulliPB <- function(q, prob){
  ifelse(q < 0, 0, ifelse(q < 1, 1 - prob, 1))
}

#' Cuantil Bernoulli(prob)
#' @param p Vector de probabilidades.
#' @param prob Probabilidad de éxito.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qbernoulliPB <- function(p, prob){
  ifelse(p <= 1 - prob, 0, 1)
}
