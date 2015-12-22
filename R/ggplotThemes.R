
#' @import ggplot2
#' @export
theme_larger <- function(magnify = 2, base_theme = theme_gray)
  base_theme() +
  theme(axis.title.x=element_text(size=rel(magnify),vjust=-.65),
        axis.title.y=element_text(size=rel(magnify),vjust=1.5),
        legend.title = element_text(size=rel(magnify)),
        legend.text = element_text(size=rel(magnify*.833)),
        axis.text.x = element_text(size=rel(magnify *.75)),
        axis.text.y = element_text(size=rel(magnify*.75)),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = rel(magnify)),
        strip.text.y = element_text(size = rel(magnify)),
        plot.title = element_text(size=rel(magnify*.833)),
        legend.key.height = unit(2,"line"),
        legend.key.width = unit(2,"line"))
