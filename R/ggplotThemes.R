
#' @import ggplot2
#' @import grid
#' @export
theme_larger <- function()
  theme_grey() +
  theme(axis.title.x=element_text(size=rel(2),vjust=-.65),
        axis.title.y=element_text(size=rel(2),vjust=1.5),
        legend.title = element_text(size=rel(1.6)),
        legend.text = element_text(size=rel(1.35)),
        axis.text.x = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2)),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=24))

#' @import ggplot2
#' @import grid
theme_presentation <- function() {
  theme_grey() +
  theme(axis.title.x=element_text(size=rel(3),vjust=-.5),
        axis.title.y=element_text(size=rel(3),vjust=1.5),
        legend.title = element_text(size=rel(3)),
        legend.text = element_text(size=rel(3)),
        axis.text.x = element_text(size=rel(3)),
        axis.text.y = element_text(size=rel(3)),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size=36),
        legend.key.height=unit(2,"line"),
        legend.key.width=unit(2,"line"))
}
