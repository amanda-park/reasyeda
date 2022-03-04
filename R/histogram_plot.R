#' @importFrom magrittr "%>%"
#'

histogram_plot <- function(data,
                           value = input$numVar,
                           num_text = numText,
                           title = base::paste0("Original Data for ",
                                                num_text)) {
  data %>%
    dplyr::select(.data[[value]]) %>%
    ggplot2::ggplot(ggplot2::aes(.data[[value]])) +
    ggplot2::geom_histogram(stat = "bin",
                            bins = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = -45,
        vjust = 1,
        hjust = 0),
      legend.position = "none") +
    ggplot2::labs(x = num_text, y = "Count", title = title)
}
