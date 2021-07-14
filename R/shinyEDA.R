#' Shiny EDA App
#'
#' @param data A data frame to feed into the Shiny app
#'
#' @return
#' @export
#'
#' @examples
#' #data(penguins, package = "modeldata")
#' #shinyEDA(penguins)
#'
#' @importFrom magrittr "%>%"

colorSets <- c("Tableau 10", "Tableau 20", "Color Blind", "Seattle Grays",
               "Traffic", "Miller Stone", "Superfishel Stone", "Nuriel Stone",
               "Jewel Bright", "Summer", "Winter", "Green-Orange-Teal",
               "Red-Blue-Brown", "Purple-Pink-Gray", "Hue Circle", "Classic 10",
               "Classic 10 Medium", "Classic 10 Light", "Classic 20",
               "Classic Gray 5", "Classic Color Blind", "Classic Traffic Light",
               "Classic Purple-Gray 6", "Classic Purple-Gray 12",
               "Classic Green-Orange 6", "Classic Green-Orange 12",
               "Classic Blue-Red 6", "Classic Blue-Red 12", "Classic Cyclic")

shinyEDA <- function(data) {
  # Check data
  assertthat::assert_that(!missing(data), msg = "Expect a data frame")
  assertthat::assert_that(is.data.frame(data), msg = "Expect a data frame")
  assertthat::assert_that(nrow(data) > 0, msg = "Data frame doesn't have any observations")

  # Define UI for application that draws a histogram
  ui <- shinydashboard::dashboardPage(
    skin = "blue",
    # Application title
    shinydashboard::dashboardHeader(title = "Quick and Pretty EDA Plots"),

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
            shiny::textOutput("ppsText"),
            width = 12
          ),

          shinydashboard::box(
            shinycssloaders::withSpinner(plotly::plotlyOutput("predPlot"),
                                         type = 3,
                                         color = "#5778a4",
                                         color.background = "#ffffff"),
            width = 6
          ),
          shinydashboard::box(
            shinycssloaders::withSpinner(plotly::plotlyOutput("respPlot"),
                                         type = 3,
                                         color = "#5778a4",
                                         color.background = "#ffffff"),
            width = 6
          ),

          shinydashboard::box(
            shinycssloaders::withSpinner(plotly::plotlyOutput("bivarPlot"),
                                         type = 3,
                                         color = "#5778a4",
                                         color.background = "#ffffff"),
            width = 6
          ),
          shinydashboard::box(
            shinycssloaders::withSpinner(plotly::plotlyOutput("bivarPlotAlt"),
                                         type = 3,
                                         color = "#5778a4",
                                         color.background = "#ffffff"),
            width = 6
          ),

          shinydashboard::tabBox(
            title = "Table Outputs",
            shiny::tabPanel("Predictor Table", DT::dataTableOutput("predDesc")),
            shiny::tabPanel("Response Table", DT::dataTableOutput("respDesc")),
            width = 12
          ),
        ),

        shinydashboard::tabItem(
          tabName = "transform",
          shinydashboard::box(
            shiny::uiOutput("numVar"),
            shiny::textOutput("shapiroOrig"),
            shiny::textOutput("shapiroBox"),
            shiny::textOutput("shapiroLog"),
            shiny::textOutput("shapiroSqrt"),
            shiny::textOutput("shapiroYJ"),
            width = 6
          ),

          shinydashboard::box(
            plotly::plotlyOutput("origDist"),
            width = 6
          ),

          shinydashboard::box(
            plotly::plotlyOutput("boxCox"), width = 6
          ),
          shinydashboard::box(
            plotly::plotlyOutput("log"), width = 6
          ),
          shinydashboard::box(
            plotly::plotlyOutput("sqrt"), width = 6
          ),
          shinydashboard::box(
            plotly::plotlyOutput("yj"), width = 6
          ),
        ),

        shinydashboard::tabItem(

          tabName = "dt",
          shinydashboard::box(
            title = "Data Table",
            DT::dataTableOutput("dataTable"),
            width = 12
          )
        ),

        shinydashboard::tabItem(
          tabName = "corr",
            shinydashboard::box(
              title = "Predictive Power Score Plot",
              shinycssloaders::withSpinner(shiny::plotOutput("ppsPlot"),
                                           type = 3,
                                           color = "#5778a4",
                                           color.background = "#ffffff"),
              width = 12
            ),


          shinydashboard::box(
            title = "Pearson Correlation Plot",
            shinycssloaders::withSpinner(shiny::plotOutput("corrPlot"),
                                         type = 3,
                                         color = "#5778a4",
                                         color.background = "#ffffff"),
            width = 12
          ),

          shinydashboard::tabBox(
            title = "Table Outputs",
            shiny::tabPanel("PPS", DT::dataTableOutput("ppsTable")),
            shiny::tabPanel("Corr", DT::dataTableOutput("corrTable")),
            width = 12
          )

        )
      )
    )

  )




  # Define server logic required to draw a histogram
  server <- function(input, output) {

    #Define Variables

    output$pred <- shiny::renderUI({
      shiny::selectInput(
        inputId = "pred",
        label = "Predictor",
        choices = data[,order(colnames(data), decreasing = TRUE)] %>% colnames()
      )
    })

    output$resp <- shiny::renderUI({
      shiny::selectInput(
        inputId = "resp",
        label = "Response",
        choices = data[,order(colnames(data), decreasing = TRUE)] %>% colnames()
      )
    })

    output$numVar <- shiny::renderUI({
      shiny::selectInput(
        inputId = "numVar",
        label = "Numeric Variable",
        choices = data %>% dplyr::select_if(is.numeric) %>% colnames()
      )
    })

    #Transformation Plots

    ##Computes Shapiro-Wilk Score
    output$shapiroOrig <- shiny::renderText({

      swOrig <- data %>%
        dplyr::pull(.data[[input$numVar]]) %>%
        stats::shapiro.test()

      base::paste0("Untransformed Shapiro-Wilk p-value: ", base::round(swOrig$p.value, 2))
    })

    output$shapiroBox <- shiny::renderText({
      #Box Cox
      bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_BoxCox(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(bc_recipe)

      swBox <- data %>%
        dplyr::pull(.data[[input$numVar]]) %>%
        stats::shapiro.test()

      base::paste0("Box-Cox Shapiro-Wilk p-value: ", base::round(swBox$p.value, 2))
    })

    output$shapiroLog <- shiny::renderText({
      #Box Cox
      bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_log(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(bc_recipe)

      swBox <- data %>%
        dplyr::pull(.data[[input$numVar]]) %>%
        stats::shapiro.test()

      base::paste0("Log Shapiro-Wilk p-value: ", base::round(swBox$p.value, 2))
    })

    output$shapiroSqrt <- shiny::renderText({
      #Box Cox
      bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_sqrt(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(bc_recipe)

      swBox <- data %>%
        dplyr::pull(.data[[input$numVar]]) %>%
        shapiro.test()

      base::paste0("Square Root Shapiro-Wilk p-value: ", base::round(swBox$p.value, 2))
    })

    output$shapiroYJ <- shiny::renderText({
      #Box Cox
      bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_YeoJohnson(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(bc_recipe)

      swBox <- data %>%
        dplyr::pull(.data[[input$numVar]]) %>%
        stats::shapiro.test()

      base::paste0("Yeo-Johnson Shapiro-Wilk p-value: ", base::round(swBox$p.value, 2))
    })

    ##Create Plotly Graphs
    output$origDist <- plotly::renderPlotly({

      numText <- input$numVar %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

      plot <- data %>%
        dplyr::select(.data[[input$numVar]]) %>%
        ggplot2::ggplot(ggplot2::aes(.data[[input$numVar]])) +
        ggplot2::geom_histogram(stat = "bin",
                       bins = 10) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
        ggplot2::labs(x = numText, y = "Count", title = base::paste0("Original Data for ", numText))

      plotly::ggplotly(plot)

    })

    output$boxCox <- plotly::renderPlotly({

      numText <- input$numVar %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

      formula <- as.formula(paste(input$numVar , ".", sep = "~"))

      bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_BoxCox(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(bc_recipe)

      bc_val <- recipes::tidy(bc_recipe, number = 1) %>%
        dplyr::filter(terms == input$numVar) %>%
        dplyr::select(value) %>%
        base::as.numeric() %>%
        base::round(2)

      sw <- data %>%
        dplyr::pull(.data[[input$numVar]]) %>%
        stats::shapiro.test()

      plot <- data %>%
        dplyr::select(.data[[input$numVar]]) %>%
        ggplot2::ggplot(ggplot2::aes(.data[[input$numVar]])) +
        ggplot2::geom_histogram(stat = "bin",
                       bins = 10) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
        ggplot2::labs(x = numText, y = "Count", title = paste0("Box-Cox Transformation of ", numText, " with Lambda = ", bc_val))

      plotly::ggplotly(plot)
    })

    output$log <- plotly::renderPlotly({

      numText <- input$numVar %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

      formula <- as.formula(paste(input$numVar , ".", sep = "~"))

      bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_log(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(bc_recipe)


      plot <- data %>%
        dplyr::select(.data[[input$numVar]]) %>%
        ggplot2::ggplot(ggplot2::aes(.data[[input$numVar]])) +
        ggplot2::geom_histogram(bins = 10) +
        ggplot2::ggtitle(paste0("Log Transformation of ", numText)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
        ggplot2::labs(x = numText, y = "Count")

      plotly::ggplotly(plot)


    })

    output$sqrt <- plotly::renderPlotly({

      numText <- input$numVar %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

      bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_sqrt(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(bc_recipe)

      plot <- data %>%
        dplyr::select(.data[[input$numVar]]) %>%
        ggplot2::ggplot(ggplot2::aes(.data[[input$numVar]])) +
        ggplot2::geom_histogram(bins = 10) +
        ggplot2::ggtitle(paste0("Square Root Transformation of ", numText)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
        ggplot2::labs(x = numText, y = "Count")

      plotly::ggplotly(plot)


    })

    output$yj <- plotly::renderPlotly({

      numText <- input$numVar %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

      formula <- as.formula(paste(input$numVar , ".", sep = "~"))

      bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
        recipes::step_YeoJohnson(recipes::all_numeric()) %>%
        recipes::prep(data, retain = TRUE)

      data <- recipes::juice(bc_recipe)

      plot <- data %>%
        dplyr::select(.data[[input$numVar]]) %>%
        ggplot2::ggplot(ggplot2::aes(.data[[input$numVar]])) +
        ggplot2::geom_histogram(bins = 10) +
        ggplot2::ggtitle(paste0("Yeo-Johnson Transformation of ", numText)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
        ggplot2::labs(x = numText, y = "Count")

      plotly::ggplotly(plot)


    })

    #Visualization Tab Plots

    output$ppsText <- shiny::renderText({
      ppsCalc <- data %>%
        dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
        ppsr::score_matrix()
      print(paste0("The Predictive Power Score Between ",
                   input$pred, " and ", input$resp, " is: ",
                   round(ppsCalc[2], 2)))
    })

    output$set_color <- shiny::renderUI({
      selectInput(
        inputId = "set_color",
        label = "Color Palette",
        choices = colorSets
      )
    })

    output$predPlot <- plotly::renderPlotly({

      predText <- input$pred %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

      if(guess_cat_num(data[[input$pred]]) == "cat") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]]) %>%
          dplyr::group_by(.data[[input$pred]]) %>%
          dplyr::summarize(n =dplyr:: n()) %>%
          dplyr::mutate(pct = base::round(n / sum(n) * 100, 1)) %>%
          ggplot2::ggplot(ggplot2::aes(.data[[input$pred]])) +
          ggplot2::geom_col(ggplot2::aes(y = .data[["n"]],
                                         fill=.data[[input$pred]]),
                   position = "dodge") +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::labs(x = predText, y = "Count") +
          ggplot2::ggtitle(paste0("Distribution of ", predText)) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0))

        plotly::ggplotly(plot)


      }

      else if(guess_cat_num(data[[input$pred]]) == "num") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]]) %>%
          ggplot2::ggplot(ggplot2::aes(.data[[input$pred]])) +
          ggplot2::geom_density(ggplot2::aes(fill = "#5778a4", alpha = .7)) +
          ggplot2::ggtitle(paste0("Distribution of ", predText)) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0),
                         legend.position = "none") +
          ggplot2::labs(x = predText, y = "Density")

        plotly::ggplotly(plot)

      }

    })

    output$respPlot <- plotly::renderPlotly({

      respText <- input$resp %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

      if(guess_cat_num(data[[input$resp]]) == "cat") {
        plot <- data %>%
          dplyr::select(.data[[input$resp]]) %>%
          dplyr::group_by(.data[[input$resp]]) %>%
          dplyr::summarize(n = dplyr::n()) %>%
          dplyr::mutate(pct = round(n / sum(n) * 100, 1)) %>%
          ggplot2::ggplot(ggplot2::aes(.data[[input$resp]])) +
          ggplot2::geom_col(ggplot2::aes(y = .data[["n"]],
                            fill=.data[[input$resp]]),
                            position = "dodge") +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::labs(x = respText, y = "Count") +
          ggplot2::ggtitle(paste0("Distribution of ", respText)) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0))

        plotly::ggplotly(plot)


      }

      else if(guess_cat_num(data[[input$resp]]) == "num") {
        plot <- data %>%
          dplyr::select(.data[[input$resp]]) %>%
          ggplot2::ggplot(ggplot2::aes(.data[[input$resp]])) +
          ggplot2::geom_density(ggplot2::aes(fill = "#5778a4", alpha = .7)) +
          ggplot2::ggtitle(paste0("Distribution of ", respText)) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0),
                         legend.position = "none") +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::labs(x = respText, y = "Density")

        plotly::ggplotly(plot)

      }

    })

    output$bivarPlot <- plotly::renderPlotly({
      predText <- input$pred %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

      respText <- input$resp %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

      if(guess_cat_num(data[[input$pred]]) == "num" & guess_cat_num(data[[input$resp]]) == "num") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          ggplot2::ggplot(ggplot2::aes(x = .data[[input$pred]], y = .data[[input$resp]])) +
          ggplot2::geom_point() +
          ggplot2::geom_smooth() +
          ggplot2::ggtitle(paste0("Scatterplot of ", predText, " and ", respText)) +
          ggplot2::labs(x = predText, y = respText)

        plotly::ggplotly(plot)
      }

      else if(guess_cat_num(data[[input$pred]]) == "cat" & guess_cat_num(data[[input$resp]]) == "num") {
        #If Categorical is datetype, plot a line chart
        if(inherits(data[[input$pred]], "Date") == TRUE) {
          plot <- data %>%
            dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
            ggplot2::ggplot(ggplot2::aes(x = .data[[input$pred]], y = .data[[input$resp]])) +
            ggplot2::geom_line() +
            ggplot2::ggtitle(paste0("Time Series of ", respText))
        }
        #Otherwise, show a boxplot
        else {
          plot <- data %>%
            dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
            ggplot2::ggplot(ggplot2::aes(x = .data[[input$pred]], y = .data[[input$resp]], fill = .data[[input$pred]])) +
            ggplot2::geom_boxplot(alpha = .7) +
            ggthemes::scale_fill_tableau(palette = input$set_color) +
            ggplot2::ggtitle(paste0("Boxplot of ", respText, " Split On ", predText)) +
            ggplot2::coord_flip()
        }

        plotly::ggplotly(plot)
      }

      else if(guess_cat_num(data[[input$pred]]) == "num" & guess_cat_num(data[[input$resp]]) == "cat") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          ggplot2::ggplot(ggplot2::aes(x = .data[[input$resp]], y = .data[[input$pred]], fill = .data[[input$resp]])) +
          ggplot2::geom_boxplot(alpha = .7) +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Boxplot of ", respText, " Split On ", predText)) +
          ggplot2::coord_flip()

        plotly::ggplotly(plot)
      }

      else {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          dplyr::group_by(.data[[input$pred]], .data[[input$resp]]) %>%
          dplyr::summarize(n = dplyr::n()) %>%
          dplyr::mutate(pct = round(n / sum(n) * 100, 1)) %>%
          ggplot2::ggplot(ggplot2::aes(x = .data[[input$resp]], y = .data[["n"]])) +
          ggplot2::geom_col(ggplot2::aes(fill = .data[[input$pred]])) +
          ggplot2::labs(x = predText, y = "Count") +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Stacked Bar Plot of ", predText, " Split On ", respText)) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0)) +
          ggplot2::guides(fill = ggplot2::guide_legend(title = respText))

        plotly::ggplotly(plot)
      }

    })

    output$bivarPlotAlt <- plotly::renderPlotly({
      predText <- input$pred %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

      respText <- input$resp %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title()

      if(guess_cat_num(data[[input$pred]]) == "num" & guess_cat_num(data[[input$resp]]) == "num") {

        df <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          tidyr::drop_na()

        form <- as.formula(paste0("~", input$pred, " + ", input$resp))

        df$pc <- predict(prcomp(form, df, scale = TRUE))[,1]

        plot <- df %>%
          ggplot2::ggplot(ggplot2::aes(x = .data[[input$pred]], y = .data[[input$resp]], color = pc)) +
          ggplot2::geom_point(shape = 16, size = 3, alpha = .7, show.legend = FALSE) +
          ggplot2::scale_color_gradient(low = "#0091ff", high = "#f0650e") +
          ggplot2::ggtitle(paste0("Scatterplot of ", predText, " and ", respText)) +
          ggplot2::labs(x = predText, y = respText)

        plotly::ggplotly(plot)
      }

      else if(guess_cat_num(data[[input$pred]]) == "cat" & guess_cat_num(data[[input$resp]]) == "num") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          ggplot2::ggplot(ggplot2::aes(x = .data[[input$resp]], fill = .data[[input$pred]])) +
          ggplot2::geom_density(alpha = .7) +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Density Plot of ", respText, " Split On ", predText)) +
          ggplot2::labs(x = respText, y = "Density")

        plotly::ggplotly(plot)
      }

      else if(guess_cat_num(data[[input$pred]]) == "num" & guess_cat_num(data[[input$resp]]) == "cat") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          ggplot2::ggplot(ggplot2::aes(x = .data[[input$pred]], fill = .data[[input$resp]])) +
          ggplot2::geom_density(alpha = .7) +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Density Plot of ", respText, " Split On ", predText)) +
          ggplot2::labs(x = predText, y = "Density")

        plotly::ggplotly(plot)
      }

      else {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          dplyr::group_by(.data[[input$pred]], .data[[input$resp]]) %>%
          dplyr::summarize(n = dplyr::n()) %>%
          dplyr::mutate(pct = round(n / sum(n) * 100, 1)) %>%
          ggplot2::ggplot(ggplot2::aes(x = .data[[input$pred]], y = .data[["n"]])) +
          ggplot2::geom_col(ggplot2::aes(fill = .data[[input$resp]]), position = "dodge") +
          ggplot2::labs(x = predText, y = "Count") +
          ggthemes::scale_fill_tableau(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Grouped Bar Plot of ", predText, " Split On ", respText)) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0)) +
          ggplot2::guides(fill = ggplot2::guide_legend(title = respText))

        plotly::ggplotly(plot)
      }

    })

    ## Descriptions for Predictor and Response Variable

    output$predDesc <- DT::renderDataTable({
      if(guess_cat_num(data[[input$pred]]) == "num") {
        numeric_metric_calculations(df = data[,input$pred]) %>%
          datatable_var_output()

      }
      else{
        data %>%
          dplyr::select(.data[[input$pred]]) %>%
          dplyr::group_by(.data[[input$pred]]) %>%
          dplyr::summarize(n = dplyr::n()) %>%
          dplyr::mutate(pct = base::round(n / sum(n) * 100, 1)) %>%
          datatable_var_output()
      }

    })

    output$respDesc <- DT::renderDataTable({

      if(guess_cat_num(data[[input$resp]]) == "num") {
        numeric_metric_calculations(df = data[,input$resp]) %>%
          datatable_var_output()
      }
      else{
        data %>%
          dplyr::select(.data[[input$resp]]) %>%
          dplyr::group_by(.data[[input$resp]]) %>%
          dplyr::summarize(n = dplyr::n()) %>%
          dplyr::mutate(pct = base::round(n / sum(n) * 100, 1)) %>%
          datatable_var_output()
      }

    })

    ## Basic interactive data table output of all data

    output$dataTable <- DT::renderDataTable({
      datatable_overall_output(data)
    })

    # Predictive Power Score

    ppsMat <- reactive(
      ppsr::score_matrix(data)
    )

    output$ppsTable <- DT::renderDataTable({
      datatable_overall_output(as.data.frame(ppsMat()) %>%
                                 round(2) %>%
                                 tibble::rownames_to_column(var = "Variable"))
    })

    output$ppsPlot <- shiny::renderPlot({
      ggcorrplot::ggcorrplot(ppsMat(),
                             title = "Predictive Power Score Plot of Data",
                             show.legend = FALSE,
                             type = "upper",
                             lab = TRUE,
                             lab_size = 3.5)
    })

    # Correlation Graphs

    corMat <- reactive(
      rstatix::cor_mat(data, names(dplyr::select_if(data, is.numeric)))
    )

    output$corrPlot <- shiny::renderPlot({
      ggcorrplot::ggcorrplot(corMat(),
                             title = "Correlation Plot of Data",
                             colors = c("blue", "white", "orange"),
                             hc.order = TRUE,
                             type = "upper",
                             lab = TRUE,
                             insig = "blank",
                             lab_size = 3.5)

    })

    output$corrTable <- DT::renderDataTable({
      datatable_overall_output(corMat())
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
