#' Moyenne mobile d'ordre N
#'
#' @param var Nom du vecteur sur lequel calculé la moyenne mobile
#' @param n Ordre de la moyenne mobile
#'
#' @return Permet de lisser une série de données par la méthode des moyennes mobiles
#' @importFrom stats filter
#' @export
#'
moy_mob <- function(var, n=5){
  stats::filter(x = var, filter = rep(1/n,n))
}
