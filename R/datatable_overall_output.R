#' @importFrom magrittr "%>%"

datatable_overall_output <- function(data)  {

DT::datatable(
  data = data,
  rownames = NULL,
  filter = "top",
  extensions = "Buttons",
  options = list(
    pageLength = 25,
    paging = TRUE,
    searching = TRUE,
    dom = 'Bfrtip',
    buttons = list('copy', 'csv', 'excel')
  )
)

}
