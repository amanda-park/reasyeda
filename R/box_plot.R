#' @importFrom magrittr "%>%"
#'

box_plot <- function(data,
                     value_pred = input$pred,
                     value_resp = input$resp,
                     color = input$set_color,
                     var_text_pred = predText,
                     var_text_resp = respText) {

  data %>%
    dplyr::select(.data[[value_pred]], .data[[value_resp]]) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[value_pred]], y = .data[[value_resp]], fill = .data[[value_pred]])) +
    ggplot2::geom_boxplot(alpha = .7) +
    ggthemes::scale_fill_tableau(palette = color) +
    ggplot2::ggtitle(paste0("Boxplot of ", var_text_resp, " Split On ", var_text_pred)) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = var_text_pred, y = var_text_resp)

}
