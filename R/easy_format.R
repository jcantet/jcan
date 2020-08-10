#' Fonction de formatage d'un nombre pour affichage
#'
#' @param variable Variable à formater
#' @param type_out Type de nombre en sortie : "pourcent", "milliers"
#' @param decimal Nombre de décimales attendues. 0 Par défaut
#' @param suffix Symbole éventiel à ajouter. AUcun par défaut
#'
#' @return Renvoit une variable formatée en pourcentage ou milliers, avec les bonnes règles typographiques
#' @export
#'
easy_format <- function(variable, type_out, decimal = 0, suffix = NULL){

  # Format pourcent
  if (type_out == "pourcent"){
    variable = paste0(format(x = round(variable*100, decimal)),"%")

    # Format milliers
  } else if (type_out == "milliers"){
    variable = paste0(format(x = round(variable, decimal), big.mark = " ", justify = "right"), suffix)
  }

  # Gestion des erreurs : si l'utilisateur rentre un paramètre qui n'est pas prévu
  if ((type_out %in% c("pourcent","milliers")) == FALSE){
    print(paste0("type_out ", type_out," n'existe pas"))
  } else {
    return(variable)
  }

}
