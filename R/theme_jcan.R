#' Thème personnalisé
#'
#' @return La fonction applique un thème à un graphique ggplot2
#' @import ggplot2
#' @export
theme_jcan <-function(){

  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        plot.background = element_rect(fill = "#FEFBF8"),
        # Paneau principal
        panel.border = element_rect(colour = NA, fill = NA),
        panel.background = element_rect(fill = "#FEFBF8"),
        # Grilles
        panel.grid.major.x = element_line(colour = "steelblue2", linetype = 3, size = 0.1),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y =  element_line(colour = "steelblue2", linetype = 3, size = 0.1),
        panel.grid.minor.y = element_blank(),
        # Facette
        strip.background = element_rect(fill = "#736364"),
        strip.text = element_text(color = "white", face = "bold", size = 9),
        # Modification du texte
        axis.text = element_text(colour = "steelblue4", face = "italic", size = 10),
        axis.text.y = element_text(vjust = 0.35, margin = margin(r = 5)),
        axis.title = element_text(colour = "steelblue4", face = "bold", size = 12),
        axis.title.x = element_text(vjust = 0, hjust = 0.98),
        axis.title.y = element_text(vjust = 2, hjust = 0.98),
        axis.ticks = element_line(colour = "steelblue4"),
        plot.title = element_text(size = 15, face = 'bold', hjust = 0, vjust = 1),
        plot.subtitle = element_text(size = 12, face = 'italic'),
        plot.caption = element_text(size = 9, face = "italic", vjust = -1),
        legend.title = element_text(size = 11, face = "bold"))
}







#' Palette de couleurs personnalisées (adaptées au thème personnalisé
#'
#' @return La fonction permet d'utiliser la palette de couleurs personnalisées
#' @export



# Set de couleurs
jcan_colors <- c(
  `bleu`  = "#235789",
  `bleu clair` = "#3F88C5",
  `rouge` = "#DB3A34",
  `vert` = "#078339",
  `vert clair` = "#5FAD41",
  `orange` = "#FF8C42",
  `bleu gris` = "#4F5D75",
  `jaune` = "#FFBA08",
  `violet` = "#7F2982",
  `blanc` = "#EDF4F9")



#' Fonction pour extraire mes couleurs comme des hex codes
#'
#' @param ... Character names of jcan_colors
#'
jcan_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (jcan_colors)

  jcan_colors[cols]
}
jcan_cols()



# Palette
jcan_palettes <- list(
  `principale` = jcan_cols("bleu clair", "rouge","vert","orange","violet","jaune","bleu gris","vert clair"),
  `progressive bleue` = jcan_cols("bleu clair", "blanc"),
  `progressive rouge` = jcan_cols("rouge", "blanc"),
  `divergente` = jcan_cols("bleu clair" ,"blanc","rouge")
)



#' Fonction pour interpoler une palette custom
#'
#' @param palette Character name of palette in jcan_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette() comme l'inversion de la palette
#' @importFrom grDevices colorRampPalette
#'
jcan_pal <- function(palette = "principale", reverse = FALSE, ...) {
  pal <- jcan_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}





#' Color scale constructor for jcan colors
#'
#' @param palette Character name of palette in jcan_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_jcan <- function(palette = "principale", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jcan_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("jcan_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in jcan_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_jcan <- function(palette = "principale", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jcan_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("jcan_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
