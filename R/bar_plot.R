#' @importFrom magrittr "%>%"
#'

bar_plot <- function(data,
                     value = input$pred,
                     color = input$set_color,
                     var_text = predText,
                     num_text = numText) {
  data %>%
    dplyr::select(.data[[value]]) %>%
    dplyr::group_by(.data[[value]]) %>%
    dplyr::summarize(n = dplyr:: n()) %>%
    dplyr::mutate(pct = base::round(n / sum(n) * 100, 1)) %>%
    ggplot2::ggplot(ggplot2::aes(.data[[value]])) +
    ggplot2::geom_col(ggplot2::aes(y = .data[["n"]],
                                   fill=.data[[value]]),
                      position = "dodge") +
    ggthemes::scale_fill_tableau(palette = color) +
    ggplot2::labs(x = var_text, y = "Count") +
    ggplot2::ggtitle(paste0("Distribution of ", var_text)) +
    ggplot2::coord_flip() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0))

}
