#' Annie's Quick ggplot Settings
#'
#' Applies formatting for  publication-ready figs
#' @param base_size Base font size
#' @param base_family Base font family
#'
#' @return A ggplot2 object
#' @export

ez_gg <- function(base_size = 12, base_family = "Times New Roman") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = base_size + 4, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = base_size + 2, hjust = 0.5),
      plot.caption = ggplot2::element_text(size = base_size - 1, hjust = 1),
      axis.title = ggplot2::element_text(size = base_size + 1, face = "bold"),
      axis.text = ggplot2::element_text(size = base_size),
      legend.title = ggplot2::element_text(size = base_size, face = "bold"),
      legend.text = ggplot2::element_text(size = base_size - 1),
      strip.text = ggplot2::element_text(size = base_size, face = "bold"),
      panel.grid.major = ggplot2::element_line(color = "gray80", size = 0.3),
      panel.grid.minor = ggplot2::element_blank()
    )
}

