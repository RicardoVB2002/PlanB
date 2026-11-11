#' Generador Triangular(a, b) por método de la inversa
#'
#' Genera n observaciones de una distribución triangular con
#' soporte (a, b), usando el método de la inversa.
#'
#' @param n Número de observaciones.
#' @param a Límite inferior.
#' @param b Límite superior.
#' @return Vector de n observaciones Triangular(a, b).
#' @examples
#' rtriangularPB(1000, a = 1, b = 10)
#' @export
rtriangularPB <- function(n, a, b){
  u <- runif(n)
  x <- a + (b - a) * sqrt(u)
  return(x)
}

#' Densidad Triangular(a, b)
#' @param x Vector de valores.
#' @param a Límite inferior.
#' @param b Límite superior.
#' @return Vector con la densidad evaluada en x.
#' @export
dtriangularPB <- function(x, a, b){
  ifelse(x > a & x < b, 2*(x - a)/(b - a)^2, 0)
}

#' Acumulada Triangular(a, b)
#' @param x Vector de valores.
#' @param a Límite inferior.
#' @param b Límite superior.
#' @return Vector con la acumulada evaluada en x.
#' @export
ptriangularPB <- function(x, a, b){
  ifelse(x <= a, 0,
         ifelse(x < b, ((x - a)/(b - a))^2, 1))
}

#' Cuantil Triangular(a, b)
#' @param p Vector de probabilidades.
#' @param a Límite inferior.
#' @param b Límite superior.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qtriangularPB <- function(p, a, b){
  a + (b - a) * sqrt(p)
}
