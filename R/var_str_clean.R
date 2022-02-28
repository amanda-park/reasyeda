#' @importFrom magrittr "%>%"
#'

var_str_clean <- function(value = input$pred) {
  value %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_to_title()
}
