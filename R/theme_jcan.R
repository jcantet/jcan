#' Thème personnalisé
#'
#' @return La fonction applique un thème à un graphique ggplot2
#' @import ggplot2
#' @export
theme_jcan <-function(){

  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = unit(c(0.7,0.7,0.7,0.7), "cm"),
        plot.background = element_rect(fill = "#FEFBF8"),
        # Paneau principal
        panel.border = element_rect(colour = NA, fill = NA),
        panel.background = element_rect(fill = "#FEFBF8"),
        # Legende
        legend.background = element_rect(fill = "#FEFBF8"),
        legend.key = element_rect(fill = "#FEFBF8"),
        # Grilles
        panel.grid.major.x = element_line(colour = "steelblue2", linetype = 3, size = 0.1),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y =  element_line(colour = "steelblue2", linetype = 3, size = 0.1),
        panel.grid.minor.y = element_blank(),
        # Facette
        strip.background = element_rect(fill = "#736364"),
        strip.text = element_text(color = "white", face = "bold", size = 9),
        # Modification du texte
        axis.text = element_text(colour = "steelblue4", face = "italic", size = 12),
        axis.text.y = element_text(vjust = 0.35, margin = margin(r = 5)),
        axis.title = element_text(colour = "steelblue4", size = 13, face = "bold"),
        axis.title.x = element_text(vjust = 0, hjust = 0.98),
        axis.title.y = element_text(vjust = 2, hjust = 0.98),
        axis.ticks = element_line(colour = "steelblue4"),
        plot.title = element_text(size = 15, face = 'bold', hjust = 0, color = "#361F27"),
        plot.subtitle = element_text(size = 12, colour = "grey35"),
        plot.caption = element_text(size = 12, face = "italic", vjust = -1, color = "#361F27"),
        legend.title = element_text(size = 12, face = "bold", color = "#361F27"),
        legend.text = element_text(color = "#361F27", size = 12))
}




#' Palette de couleurs personnalisées adaptées au thème personnalisé
#'
#' @return La fonction permet d'utiliser la palette de couleurs personnalisées
#' @export
# Set de couleurs
jcan_colors <- c(
  `bleu`  = "#004385",
  `bleu clair` = "#3F88C5",
  `rouge` = "#CD342F",
  `vert` = "#078339",
  `vert clair` = "#5FAD41",
  `orange` = "#FF8C42",
  `bleu gris` = "#4F5D75",
  `jaune` = "#FFBA08",
  `violet` = "#7F2982",
  `blanc` = "#EDF4F9",
  `bleu tres clair` = "#DFEBF5",
  `rouge tres clair` = "#F7DDDC",
  `vert tres clair` = "#D6EADE",
  `violet tres clair` = "#EADBEA",
  `violet grad` = "#432371",
  `orange grad` = "#FAAE7B")




#' Fonction pour extraire mes couleurs comme des hex codes
#'
#' @param ... Noms en toutes lettes pour les couleurs de ma palette custom jcan_colors
#'
#' @return La fonctionne transforme les noms de couleurs en HEX code
#' @export
#'
jcan_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (jcan_colors)

  jcan_colors[cols]
}


#' Multiples palettes avec les couleurs personnalisées :
#' principale, progressive bleue, progressive bleue2,progressive rouge,progressive rouge2,progressive vert,progressive vert2,progressive violet,progressive violet2,VioletOrange,divergente
#' @return On définit les différentes palettes
#' @export
# Palette

jcan_palettes <- list(
  `principale` = jcan_cols("bleu clair", "vert clair","jaune","orange","rouge"),
  `bleue` = jcan_cols("bleu clair", "blanc"),
  `bleue2` = jcan_cols("bleu clair", "bleu tres clair"),
  `rouge` = jcan_cols("rouge", "blanc"),
  `rouge2` = jcan_cols("rouge", "rouge tres clair"),
  `vert` = jcan_cols("vert","blanc"),
  `vert2` = jcan_cols("vert","vert tres clair"),
  `violet` = jcan_cols("violet","white"),
  `violet2` = jcan_cols("violet","violet tres clair"),
  `VioletOrange` = jcan_cols("violet grad", "orange grad"),
  `div2` = jcan_cols("bleu gris" ,"jaune")
)


#' Fonction pour interpoler une palette custom
#'
#' @param palette Nom de la palette dans jcan_palettes
#' @param reverse Boolean pour inverser ou non la palette sélectionnée
#' @param ... Arguements additionnels à passer dans colorRampPalette()
#'
#' @return Renvoi la palette sélectionné, et l'interpole pour avoir plus de couleurs si nécessaire
#' @importFrom grDevices colorRampPalette
#' @export
#'
jcan_pal <- function(palette = "principale", reverse = FALSE, ...) {
  pal <- jcan_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}





#' Constructeur de scale_color pour ggplot avec les palettes custom du packages
#'
#' @param palette Nom de la palette dans jcan_palettes
#' @param discrete Boolean pour indiquer si la palette sélectionnée s'applique à des données discrètes ou non
#' @param reverse Boolean pour inverser ou non la palette sélectionnée
#' @param ... Additional arguments passed to discrete_scale() or scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @return Applique la palette custom sur un graphique ggplot
#' @export
#'
scale_color_jcan <- function(palette = "principale", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jcan_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("jcan_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}




#' Constructeur de scale_fill pour ggplot avec les palettes custom du packages
#'
#' @param palette Nom de la palette dans jcan_palettes
#' @param discrete Boolean pour indiquer si la palette sélectionnée s'applique à des données discrètes ou non
#' @param reverse Boolean pour inverser ou non la palette sélectionnée
#' @param ... Additional arguments passed to discrete_scale() or scale_dill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @return Applique la palette custom sur un graphique ggplot
#' @export
#'
scale_fill_jcan <- function(palette = "principale", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jcan_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("jcan_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
