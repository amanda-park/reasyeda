#' @importFrom magrittr "%>%"

datatable_var_output <- function(data)  {

DT::datatable(
  data = data,
  rownames = NULL,
  extensions = "Buttons",
  options = list(
    paging = FALSE,
    searching = FALSE,
    dom = "Bfrtip",
    buttons = list("copy", "csv", "excel"),
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css(
      {'background-color': '#007ba7', 'color': '#ffffff'});",
      "}")
  )
)

}
