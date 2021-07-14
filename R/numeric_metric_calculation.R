#' @importFrom magrittr "%>%"

numeric_metric_calculations <- function(df = data) {

  output <- df %>%
    summarize(`Minimum Value` = base::min(df, na.rm = TRUE),
              `25% Percentile` = stats::quantile(df, 0.25, na.rm = TRUE),
              `Mean Value` = base::round(mean(df %>% dplyr::pull(), na.rm = TRUE), 2),
              `Median Value` = stats::median(df %>% dplyr::pull(), na.rm = TRUE),
              `75% Percentile` = stats::quantile(df, 0.75, na.rm = TRUE),
              `Maximum Value` = base::max(df, na.rm = TRUE),
              `Missing Count` = base::sum(is.na(df)),
              `Total Observations` = dplyr::n(),
              `Zero Value Count` = base::sum(df == 0, na.rm = TRUE),
              `Negative Value Count` = base::sum(df < 0, na.rm = TRUE),
              `Outlier Count` = base::length(grDevices::boxplot.stats(df)$out)) %>%
    base::t() %>%
    base::as.data.frame() %>%
    tibble::rownames_to_column("row_names") %>%
    purrr::set_names("Numeric Metric", "Value")

  return(output)
}

