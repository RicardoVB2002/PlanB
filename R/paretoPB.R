#' Generador Pareto(alpha, xm) por método de la inversa
#'
#' Genera n observaciones de una Pareto con parámetro de forma
#' alpha y valor mínimo xm, usando el método de la inversa.
#'
#' @param n Número de observaciones.
#' @param alpha Parámetro de forma.
#' @param xm Valor mínimo del soporte.
#' @return Vector de n observaciones Pareto(alpha, xm).
#' @examples
#' rparetoPB(1000, alpha = 3, xm = 1)
#' @export
rparetoPB <- function(n, alpha, xm){
  u <- runif(n)
  x <- xm / (1 - u)^(1/alpha)
  return(x)
}

#' Densidad Pareto(alpha, xm)
#' @param x Vector de valores.
#' @param alpha Parámetro de forma.
#' @param xm Valor mínimo del soporte.
#' @return Vector con la densidad evaluada en x.
#' @export
dparetoPB <- function(x, alpha, xm){
  ifelse(x >= xm, alpha * xm^alpha / x^(alpha + 1), 0)
}

#' Acumulada Pareto(alpha, xm)
#' @param x Vector de valores.
#' @param alpha Parámetro de forma.
#' @param xm Valor mínimo del soporte.
#' @return Vector con la acumulada evaluada en x.
#' @export
pparetoPB <- function(x, alpha, xm){
  ifelse(x <= xm, 0, 1 - (xm/x)^alpha)
}

#' Cuantil Pareto(alpha, xm)
#' @param p Vector de probabilidades.
#' @param alpha Parámetro de forma.
#' @param xm Valor mínimo del soporte.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qparetoPB <- function(p, alpha, xm){
  xm / (1 - p)^(1/alpha)
}
