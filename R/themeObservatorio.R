#' Theme to use following Observatorio COVID BR website standards
#'
#' @export
#'
themeObservatorio <- function() {
  theme_bw() +
    theme(axis.text = element_text(size = 10, face = "plain"),
          axis.title = element_text(size = 10, face = "plain"),
          legend.text = element_text(size = 12),
          title = element_text(size = 12),
          plot.margin = margin(0, 0, 0, 0, "pt"),
          panel.border = element_blank(),
          panel.grid = element_line(size = 0.25),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
}
