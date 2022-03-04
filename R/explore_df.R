#' Shiny EDA App
#'
#' @param data A data frame to feed into the Shiny app
#'
#' @return
#' @export
#'
#' @examples
#' #data(penguins, package = "modeldata")
#' #explore_df(penguins)
#'
#' @importFrom magrittr "%>%"

color_sets <- c("Tableau 10", "Tableau 20", "Color Blind", "Seattle Grays",
               "Traffic", "Miller Stone", "Superfishel Stone", "Nuriel Stone",
               "Jewel Bright", "Summer", "Winter", "Green-Orange-Teal",
               "Red-Blue-Brown", "Purple-Pink-Gray", "Hue Circle", "Classic 10",
               "Classic 10 Medium", "Classic 10 Light", "Classic 20",
               "Classic Gray 5", "Classic Color Blind", "Classic Traffic Light",
               "Classic Purple-Gray 6", "Classic Purple-Gray 12",
               "Classic Green-Orange 6", "Classic Green-Orange 12",
               "Classic Blue-Red 6", "Classic Blue-Red 12", "Classic Cyclic")

explore_df <- function(data) {
  # Check data
  assertthat::assert_that(!missing(data),
                          msg = "Expect a data frame")
  assertthat::assert_that(is.data.frame(data),
                          msg = "Expect a data frame")
  assertthat::assert_that(nrow(data) > 0,
                          msg = "Data frame has no observations")

  # Define UI for application that draws a histogram
  ui <- shinydashboard::dashboardPage(
    skin = "blue",
    # Application title
    shinydashboard::dashboardHeader(title = "DashboardExploreR"),
    #Sidebar
    shinydashboard::dashboardSidebar(

      shinydashboard::sidebarMenu(

        shinydashboard::menuItem("Visuals",
                                 tabName = "ggplot"),

        shinydashboard::menuItem("Transformations",
                                 tabName = "transform"),

        shinydashboard::menuItem("Data Frame",
                                 tabName = "dt"),

        shinydashboard::menuItem("Correlation",
                                 tabName = "corr")
      )
    ),

    shinydashboard::dashboardBody(

      shinydashboard::tabItems(

        shinydashboard::tabItem(
          title = "Visualizations",
          tabName = "ggplot",
          solidHeader = TRUE,

          shinydashboard::box(
            shiny::uiOutput("pred"),
            width = 4
          ),

          shinydashboard::box(
            shiny::uiOutput("resp"),
            width = 4
          ),

          shinydashboard::box(
            shiny::uiOutput("set_color"),
            width = 4
          ),

          shinydashboard::box(
            shiny::textOutput("pps_text"),
            width = 12
          ),

          shinydashboard::box(
            shinycssloaders::withSpinner(
               plotly::plotlyOutput("predictor_plot"),
               type = 3,
               color = "#5778a4",
               color.background = "#ffffff"),
            width = 6
          ),

          shinydashboard::box(
            shinycssloaders::withSpinner(
               plotly::plotlyOutput("response_plot"),
               type = 3,
               color = "#5778a4",
               color.background = "#ffffff"),
            width = 6
          ),

          shinydashboard::box(
            shinycssloaders::withSpinner(
               plotly::plotlyOutput("bivariate_plot"),
               type = 3,
               color = "#5778a4",
               color.background = "#ffffff"),
            width = 6
          ),

          shinydashboard::box(
            shinycssloaders::withSpinner(
               plotly::plotlyOutput("bivariate_plot_alt"),
               type = 3,
               color = "#5778a4",
               color.background = "#ffffff"),
            width = 6
          ),

          shinydashboard::tabBox(
            title = "Table Outputs",
            shiny::tabPanel(
              "Predictor Table",
              DT::dataTableOutput("predictor_summary_table")
              ),
            shiny::tabPanel(
              "Response Table",
              DT::dataTableOutput("response_summary_table")
              ),
            width = 12
          ),
        ),

        shinydashboard::tabItem(
          tabName = "transform",
          shinydashboard::box(
            shiny::uiOutput("num_var"),
            shiny::textOutput("shapiro_original_dist"),
            shiny::textOutput("shapiro_log_transform"),
            shiny::textOutput("shapiro_sqrt_transform"),
            shiny::textOutput("shapiro_yj_transform"),
            width = 6
          ),

          shinydashboard::box(
            plotly::plotlyOutput("original_distribution"),
            width = 6
          ),

          shinydashboard::box(
            plotly::plotlyOutput("log_transform"), width = 6
          ),

          shinydashboard::box(
            plotly::plotlyOutput("sqrt_transform"), width = 6
          ),

          shinydashboard::box(
            plotly::plotlyOutput("yj_transform"), width = 6
          ),

        ),

        shinydashboard::tabItem(

          tabName = "dt",
          shinydashboard::box(
            title = "Data Table",
            DT::dataTableOutput("all_data_datatable"),
            width = 12
          )
        ),

        shinydashboard::tabItem(
          tabName = "corr",
            shinydashboard::box(
              title = "Predictive Power Score Plot",
              shinycssloaders::withSpinner(shiny::plotOutput("pps_plot"),
                                           type = 3,
                                           color = "#5778a4",
                                           color.background = "#ffffff"),
              width = 12
            ),

          shinydashboard::box(
            title = "Pearson Correlation Plot",
            shinycssloaders::withSpinner(shiny::plotOutput("corr_plot"),
                                         type = 3,
                                         color = "#5778a4",
                                         color.background = "#ffffff"),
            width = 12
          ),

          shinydashboard::tabBox(
            title = "Table Outputs",
            shiny::tabPanel("PPS", DT::dataTableOutput("pps_table")),
            shiny::tabPanel("Corr", DT::dataTableOutput("corr_table")),
            width = 12
          )

        )
      )
    )

  )

  # Define server logic required to draw a histogram
#' Title
#'
#' @param input
#' @param output
#'
#' @return
#' @export
#'
#' @examples
  server <- function(input, output) {

    #Define Variables

    output$pred <- shiny::renderUI({
      shiny::selectInput(
        inputId = "pred",
        label = "Predictor",
        choices = data[, order(colnames(data), decreasing = TRUE)]
          %>% colnames()
      )
    })

    output$resp <- shiny::renderUI({
      shiny::selectInput(
        inputId = "resp",
        label = "Response",
        choices = data[, order(colnames(data), decreasing = FALSE)]
          %>% colnames()
      )
    })

    output$num_var <- shiny::renderUI({
      shiny::selectInput(
        inputId = "num_var",
        label = "Numeric Variable",
        choices = data %>%
          dplyr::select_if(is.numeric) %>%
          colnames()
      )
    })

    #Transformation Plots

    ##Computes Shapiro-Wilk Score
    output$shapiro_original_dist <- shiny::renderText({

      sw_orig_score <- data %>%
        dplyr::pull(.data[[input$num_var]]) %>%
        stats::shapiro.test()

      base::paste0("Untransformed Shapiro-Wilk p-value: ",
                   base::round(sw_orig_score$p.value, 2))
    })

    output$shapiro_log_transform <- shiny::renderText({
      #Box Cox
      bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_log(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(bc_recipe)

      sw_log_score <- data %>%
        dplyr::pull(.data[[input$num_var]]) %>%
        stats::shapiro.test()

      base::paste0("Log Shapiro-Wilk p-value: ",
                   base::round(sw_log_score$p.value, 2))
    })

    output$shapiro_sqrt_transform <- shiny::renderText({

      #Sqrt
      bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_sqrt(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(bc_recipe)

      sw_sqrt_score <- data %>%
        dplyr::pull(.data[[input$num_var]]) %>%
        shapiro.test()

      base::paste0("Square Root Shapiro-Wilk p-value: ",
                   base::round(sw_sqrt_score$p.value, 2))
    })

    output$shapiro_yj_transform <- shiny::renderText({
      #Box Cox
      bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_YeoJohnson(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(bc_recipe)

      sw_yj_score <- data %>%
        dplyr::pull(.data[[input$num_var]]) %>%
        stats::shapiro.test()

      base::paste0("Yeo-Johnson Shapiro-Wilk p-value: ",
                   base::round(sw_yj_score$p.value, 2))
    })

    ##Create Plotly Graphs
    output$original_distribution <- plotly::renderPlotly({

      num_text <- var_str_clean(input$num_var)

      plot <- histogram_plot(
        data,
        value = input$num_var,
        num_text = num_text,
        title = base::paste0("Original Data for ", num_text))

      plotly::ggplotly(plot)

    })

    output$log_transform <- plotly::renderPlotly({

      num_text <- var_str_clean(input$num_var)

      log_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_log(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(log_recipe)

      plot <- histogram_plot(
        data,
        value = input$num_var,
        num_text = num_text,
        title = base::paste0("Log Transformation of ", num_text))

      plotly::ggplotly(plot)

    })

    output$sqrt_transform <- plotly::renderPlotly({

      num_text <- var_str_clean(input$num_var)

      sqrt_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_sqrt(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(sqrt_recipe)

      plot <- histogram_plot(
        data,
        value = input$num_var,
        num_text = num_text,
        title = base::paste0("Square Root Transformation of ", num_text))

      plotly::ggplotly(plot)

    })

    output$yj_transform <- plotly::renderPlotly({

      num_text <- var_str_clean(input$num_var)

      yj_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_YeoJohnson(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(yj_recipe)

      plot <- histogram_plot(
        data,
        value = input$num_var,
        num_text = num_text,
        title = base::paste0("Yeo-Johnson Transformation of ", num_text))

      plotly::ggplotly(plot)

    })

    #Visualization Tab Plots

    output$pps_text <- shiny::renderText({
      pps_calc <- data %>%
        dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
        ppsr::score_matrix()

      print(paste0("The Predictive Power Score Between ",
                   input$pred, " and ", input$resp, " is: ",
                   round(pps_calc[2], 2)))
    })

    output$set_color <- shiny::renderUI({
      selectInput(
        inputId = "set_color",
        label = "Color Palette",
        choices = color_sets
      )
    })

    output$predictor_plot <- plotly::renderPlotly({

      pred_text <- var_str_clean(input$pred)

      if (guess_cat_num(data[[input$pred]]) == "cat") {

        plot <- bar_plot(data,
                         value = input$pred,
                         color = input$set_color,
                         var_text = pred_text,
                         num_text = num_text)

        plotly::ggplotly(plot)
      }

      else if (guess_cat_num(data[[input$pred]]) == "num") {

        plot <- density_plot(data,
                         value = input$pred,
                         var_text = pred_text)

        plotly::ggplotly(plot)
      }
    })

    output$response_plot <- plotly::renderPlotly({

      resp_text <- var_str_clean(input$resp)

      if (guess_cat_num(data[[input$resp]]) == "cat") {

        plot <- bar_plot(data,
                         value = input$resp,
                         color = input$set_color,
                         var_text = resp_text,
                         num_text = num_text)

        plotly::ggplotly(plot)

      }

      else if (guess_cat_num(data[[input$resp]]) == "num") {

        plot <- density_plot(data,
                             value = input$resp,
                             var_text = resp_text)

        plotly::ggplotly(plot)

      }

    })

    output$bivariate_plot <- plotly::renderPlotly({

      pred_text <- var_str_clean(input$pred)
      resp_text <- var_str_clean(input$resp)

      if (guess_cat_num(data[[input$pred]]) == "num" &
         guess_cat_num(data[[input$resp]]) == "num") {
        plot <- scatter_plot(data,
                             value_pred = input$pred,
                             value_resp = input$resp,
                             var_text_pred = pred_text,
                             var_text_resp = resp_text)

        plotly::ggplotly(plot)
      }

      else if (guess_cat_num(data[[input$pred]]) == "cat" &
              guess_cat_num(data[[input$resp]]) == "num") {
        #If Categorical is datetype, plot a line chart
        if (inherits(data[[input$pred]], "Date") == TRUE) {
          plot <- data %>%
            dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
            ggplot2::ggplot(
              ggplot2::aes(x = .data[[input$pred]],
                           y = .data[[input$resp]])) +
            ggplot2::geom_line() +
            ggplot2::ggtitle(paste0("Time Series of ", resp_text))
        }
        #Otherwise, show a boxplot
        else {
          plot <- box_plot(data,
                           value_pred = input$pred,
                           value_resp = input$resp,
                           color = input$set_color,
                           var_text_pred = pred_text,
                           var_text_resp = resp_text)

        }

        plotly::ggplotly(plot)
      }

      else if (guess_cat_num(data[[input$pred]]) == "num" &
              guess_cat_num(data[[input$resp]]) == "cat") {

        plot <- box_plot(data,
                         value_pred = input$resp,
                         value_resp = input$pred,
                         color = input$set_color,
                         var_text_pred = resp_text,
                         var_text_resp = pred_text)

        plotly::ggplotly(plot)
      }

      else {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          dplyr::group_by(.data[[input$pred]], .data[[input$resp]]) %>%
          dplyr::summarize(n = dplyr::n()) %>%
          dplyr::mutate(pct = round(n / sum(n) * 100, 1)) %>%
          ggplot2::ggplot(
            ggplot2::aes(x = .data[[input$resp]],
                         y = .data[["n"]])) +
          ggplot2::geom_col(ggplot2::aes(fill = .data[[input$pred]])) +
          ggplot2::labs(x = pred_text, y = "Count") +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Stacked Bar Plot of ",
                                  pred_text, " Split On ",
                                  resp_text)) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = -45,
                                                vjust = 1,
                                                hjust = 0)) +
          ggplot2::guides(fill = ggplot2::guide_legend(title = resp_text))

        plotly::ggplotly(plot)
      }

    })

    output$bivariate_plot_alt <- plotly::renderPlotly({
      pred_text <- var_str_clean(input$pred)
      resp_text <- var_str_clean(input$resp)

      if (guess_cat_num(data[[input$pred]]) == "num" &
         guess_cat_num(data[[input$resp]]) == "num") {

        df <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          tidyr::drop_na()

        form <- as.formula(paste0("~", input$pred, " + ", input$resp))

        df$pc <- predict(prcomp(form, df, scale = TRUE))[, 1]

        plot <- df %>%
          ggplot2::ggplot(
            ggplot2::aes(x = .data[[input$pred]],
                         y = .data[[input$resp]],
                         color = pc)) +
          ggplot2::geom_point(shape = 16,
                              size = 3,
                              alpha = .7,
                              show.legend = FALSE) +
          ggplot2::scale_color_gradient(low = "#0091ff", high = "#f0650e") +
          ggplot2::ggtitle(paste0("Scatterplot of ",
                                  pred_text, " and ", resp_text)) +
          ggplot2::labs(x = pred_text, y = resp_text)

        plotly::ggplotly(plot)
      }

      else if (guess_cat_num(data[[input$pred]]) == "cat" &
              guess_cat_num(data[[input$resp]]) == "num") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          ggplot2::ggplot(
            ggplot2::aes(x = .data[[input$resp]],
                         fill = .data[[input$pred]])) +
          ggplot2::geom_density(alpha = .7) +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Density Plot of ",
                                  resp_text, " Split On ", pred_text)) +
          ggplot2::labs(x = resp_text, y = "Density")

        plotly::ggplotly(plot)
      }

      else if (guess_cat_num(data[[input$pred]]) == "num" &
              guess_cat_num(data[[input$resp]]) == "cat") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          ggplot2::ggplot(
            ggplot2::aes(x = .data[[input$pred]],
                         fill = .data[[input$resp]])) +
          ggplot2::geom_density(alpha = .7) +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Density Plot of ",
                                  resp_text, " Split On ", pred_text)) +
          ggplot2::labs(x = pred_text, y = "Density")

        plotly::ggplotly(plot)
      }

      else {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          dplyr::group_by(.data[[input$pred]], .data[[input$resp]]) %>%
          dplyr::summarize(n = dplyr::n()) %>%
          dplyr::mutate(pct = round(n / sum(n) * 100, 1)) %>%
          ggplot2::ggplot(
            ggplot2::aes(x = .data[[input$pred]],
                         y = .data[["n"]])) +
          ggplot2::geom_col(
            ggplot2::aes(
              fill = .data[[input$resp]]),
              position = "dodge") +
          ggplot2::labs(x = pred_text, y = "Count") +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Grouped Bar Plot of ",
                                  pred_text, " Split On ", resp_text)) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = -45,
              vjust = 1,
              hjust = 0)) +
          ggplot2::guides(fill = ggplot2::guide_legend(title = resp_text))

        plotly::ggplotly(plot)
      }

    })

    ## Descriptions for Predictor and Response Variable

    output$predictor_summary_table <- DT::renderDataTable({
      if (guess_cat_num(data[[input$pred]]) == "num") {
        numeric_metric_calculations(df = data[, input$pred]) %>%
          datatable_var_output()

      }
      else{
        data %>%
          dplyr::select(.data[[input$pred]]) %>%
          dplyr::group_by(.data[[input$pred]]) %>%
          dplyr::summarize(count = dplyr::n()) %>%
          dplyr::mutate(percent = base::round(count / sum(count) * 100, 1)) %>%
          dplyr::rename_with(stringr::str_to_title, dplyr::everything()) %>%
          datatable_var_output()
      }

    })

    output$response_summary_table <- DT::renderDataTable({

      if (guess_cat_num(data[[input$resp]]) == "num") {
        numeric_metric_calculations(df = data[, input$resp]) %>%
          datatable_var_output()
      }
      else {
        data %>%
          dplyr::select(.data[[input$resp]]) %>%
          dplyr::group_by(.data[[input$resp]]) %>%
          dplyr::summarize(count = dplyr::n()) %>%
          dplyr::mutate(percent = base::round(count / sum(count) * 100, 1)) %>%
          dplyr::rename_with(stringr::str_to_title, dplyr::everything()) %>%
          datatable_var_output()
      }

    })

    ## Basic interactive data table output of all data

    output$all_data_datatable <- DT::renderDataTable({
      datatable_overall_output(
        data %>%
          dplyr::rename_with(stringr::str_to_title, dplyr::everything()))
    })

    # Predictive Power Score

    pps_matrix <- reactive(
      ppsr::score_matrix(data)
    )

    output$pps_table <- DT::renderDataTable({
      datatable_overall_output(as.data.frame(pps_matrix()) %>%
                                 round(2) %>%
                                 tibble::rownames_to_column(var = "Variable"))
    })

    output$pps_plot <- shiny::renderPlot({
      ggcorr_plot::ggcorr_plot(pps_matrix(),
                             title = "Predictive Power Score Plot of Data",
                             show.legend = FALSE,
                             type = "upper",
                             lab = TRUE,
                             lab_size = 3.5)
    })

    # Correlation Graphs

    corr_matrix <- reactive(
      rstatix::cor_mat(data, names(dplyr::select_if(data, is.numeric)))
    )

    output$corr_plot <- shiny::renderPlot({
      ggcorr_plot::ggcorr_plot(corr_matrix(),
                             title = "Correlation Plot of Data",
                             colors = c("blue", "white", "orange"),
                             hc.order = TRUE,
                             type = "upper",
                             lab = TRUE,
                             insig = "blank",
                             lab_size = 3.5)

    })

    output$corr_table <- DT::renderDataTable({
      datatable_overall_output(corr_matrix())
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
