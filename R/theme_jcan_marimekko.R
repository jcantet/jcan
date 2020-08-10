#' Thème personnalisé
#'
#' @return La fonction applique un thème à un graphique ggplot2
#' @import ggplot2
#' @export
theme_jcan_marimekko <-function(){

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
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y =  element_line(colour = "steelblue2", linetype = 3, size = 0.1),
        panel.grid.minor.y = element_blank(),
        # Facette
        strip.text = element_text(color = "white", face = "bold", size = 9),
        # Modification du texte
        axis.text = element_text(colour = "steelblue4", face = "italic", size = 12),
        axis.title = element_text(colour = "steelblue4", size = 13, face = "bold"),
        axis.title.x = element_text(vjust = 0, hjust = 0.98),
        axis.title.y = element_text(vjust = 2, hjust = 0.95),
        plot.title = element_text(size = 15, face = 'bold', hjust = 0, color = "#361F27"),
        plot.subtitle = element_text(size = 12, colour = "grey35"),
        plot.caption = element_text(size = 12, face = "italic", vjust = -1, color = "#361F27"),
        legend.title = element_text(size = 12, face = "bold", color = "#361F27"),
        legend.text = element_text(color = "#361F27", size = 12),
        # Spécial marimekko
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing.x = unit(0.002, "npc"),
        strip.background = element_rect(colour = "white", fill = "grey20"))
}
