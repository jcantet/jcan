#' Marimekko graphique avec ggplot2 ersion fonctionnelle avec transformation en facteur si nécessaire ====
#'
#' @param dataset Source de données
#' @param group Variable de l'axe X
#' @param fill Découpage sur l'axe Y
#' @param switch Pour inverser l'axe d'affichage des étiquettes
#' @param nb_car Nombre de caractères par ligne dans les strip
#' @param ... Arguments complémentaires
#'
#' @return Génère un graphique type Marimekko, avec une variable de type facteur sur l'axe X, une varaible de type facteur pour décomposer chaque groupe. La largeur de la colonne est définie par les effectifs du groupe
#' @import dplyr ggplot2
#' @importFrom stringr str_wrap
#' @importFrom scales percent
#' @export
#'
marimekko_plot <- function(dataset, group, fill, switch = TRUE, nb_car = 15,  ...){

  group <- enquo(group)
  fill <- enquo(fill)

  # Transformation en facteur de la variable de remplissage
  dataset[,quo_name(fill)] <- as.factor(dataset[,quo_name(fill)])
  # Transformation en facteur de la variable de l'axe X
  dataset[,quo_name(group)] <- as.factor(dataset[,quo_name(group)])

  # Préparation des données ====
  input <- dataset %>%
    group_by(!!group , !!fill) %>%
    summarize(nb = n()) %>%
    group_by(!!group) %>%
    mutate(taille = sum(nb),
           prop = nb / sum(nb)) %>%
    ungroup() %>%
    mutate(taille2 = taille / sum(nb),
           !!group := stringr::str_wrap(!!group,width = nb_car)) # Le nom de la variable se met à jour en fonction de l'input


  # Graphique ====
  g1 <-
    ggplot(data = input, aes(width = taille2))+
    geom_col(aes(x = !!group, y = nb, fill = !!fill), position = "fill", color = "white")+
    geom_label(aes(x = !!group, y = prop, label = scales::percent(prop,accuracy = 0.1), fill = !!fill),
               position = position_stack(vjust = 0.5),
               color = "white", size = 3, fontface = "bold", show.legend = FALSE)+
    scale_x_discrete(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0.03))+

    # Pour choisir sur quel axe afficher les étiquettes de l'axe X
    if (switch == TRUE){
      facet_grid(cols = vars(!!group), scales = "free_x", space = "free_x")
    } else {
      facet_grid(cols = vars(!!group), scales = "free_x", space = "free_x", switch = "x")
    }

  g1 +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.spacing.x = unit(0.002, "npc"),
          strip.background = element_rect(colour = "white"))

}
