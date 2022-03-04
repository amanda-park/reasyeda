#' @importFrom magrittr "%>%"
#'

scatter_plot <- function(data,
                         value_pred = input$pred,
                         value_resp = input$resp,
                         var_text_pred = pred_text,
                         var_text_resp = resp_text) {

  data %>%
    dplyr::select(.data[[value_pred]], .data[[value_resp]]) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[[value_pred]],
                   y = .data[[value_resp]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth() +
    ggplot2::ggtitle(paste0("Scatterplot of ",
                            var_text_pred, " and ", var_text_resp)) +
    ggplot2::labs(x = var_text_pred, y = var_text_resp)

}
