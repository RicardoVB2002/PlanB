#' Generador Chi-cuadrado(df) usando qchisq
#'
#' La Chi-cuadrado con df grados de libertad es un caso particular
#' de la Gamma con forma df/2 y escala 2. Se usa qchisq() de R.
#'
#' @param n Número de observaciones.
#' @param df Grados de libertad.
#' @return Vector de n observaciones Chi-cuadrado(df).
#' @examples
#' rchisqPB(1000, df = 5)
#' @export
rchisqPB <- function(n, df){
  u <- runif(n)
  qchisq(u, df = df)
}

#' Densidad Chi-cuadrado(df)
#' @param x Vector de valores.
#' @param df Grados de libertad.
#' @return Vector con la densidad evaluada en x.
#' @export
dchisqPB <- function(x, df){
  dchisq(x, df = df)
}

#' Acumulada Chi-cuadrado(df)
#' @param x Vector de valores.
#' @param df Grados de libertad.
#' @return Vector con la acumulada evaluada en x.
#' @export
pchisqPB <- function(x, df){
  pchisq(x, df = df)
}

#' Cuantil Chi-cuadrado(df)
#' @param p Vector de probabilidades.
#' @param df Grados de libertad.
#' @return Vector con la inversa de F evaluada en p.
#' @export
qchisqPB <- function(p, df){
  qchisq(p, df = df)
}
