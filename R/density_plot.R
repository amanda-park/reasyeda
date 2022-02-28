#' @importFrom magrittr "%>%"
#'

density_plot <- function(data,
                     value = input$pred,
                     var_text = predText) {

  data %>%
    dplyr::select(.data[[value]]) %>%
    ggplot2::ggplot(ggplot2::aes(.data[[value]])) +
    ggplot2::geom_density(ggplot2::aes(fill = "#5778a4", alpha = .7)) +
    ggplot2::ggtitle(paste0("Distribution of ", var_text)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0),
                   legend.position = "none") +
    ggplot2::labs(x = var_text, y = "Density")

}
