#' Generador Normal(mu, sigma) por Box-Muller
#'
#' La Normal no tiene inversa algebraica simple. Se genera usando
#' el método de Box-Muller, que transforma dos uniformes en una
#' normal estándar y luego se estandariza con mu y sigma.
#'
#' @param n Número de observaciones.
#' @param mu Media.
#' @param sigma Desviación estándar.
#' @return Vector de n observaciones Normal(mu, sigma).
#' @examples
#' rnormalPB(1000, mu = 0, sigma = 1)
#' @export
rnormalPB <- function(n, mu = 0, sigma = 1){
  u1 <- runif(n)
  u2 <- runif(n)
  z  <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  x  <- mu + sigma * z
  return(x)
}

#' Densidad Normal(mu, sigma)
#' @param x Vector de valores.
#' @param mu Media.
#' @param sigma Desviación estándar.
#' @return Vector con la densidad evaluada en x.
#' @export
dnormalPB <- function(x, mu = 0, sigma = 1){
  dnorm(x, mean = mu, sd = sigma)
}

#' Acumulada Normal(mu, sigma)
#' @param x Vector de valores.
#' @param mu Media.
#' @param sigma Desviación estándar.
#' @return Vector con la acumulada evaluada en x.
#' @export
pnormalPB <- function(x, mu = 0, sigma = 1){
  pnorm(x, mean = mu, sd = sigma)
}

#' Cuantil Normal(mu, sigma)
#' @param p Vector de probabilidades.
#' @param mu Media.
#' @param sigma Desviación estándar.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qnormalPB <- function(p, mu = 0, sigma = 1){
  qnorm(p, mean = mu, sd = sigma)
}
