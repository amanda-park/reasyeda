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

shinyEDA <- function(data) {
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
        shinydashboard::menuItem("Table",
                                 tabName = "dt"),
        shinydashboard::menuItem("Predictive Power Score",
                                 tabName = "pps"),
        shinydashboard::menuItem("Pearson Correlation",
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
            shiny::uiOutput("color"),
            shiny::uiOutput("set_color"),
            width = 4
          ),

          shinydashboard::box(
            shiny::textOutput("ppsText"),
            width = 8
          ),

          shinydashboard::box(
            plotly::plotlyOutput("predPlot"),
            width = 6
          ),
          shinydashboard::box(
            plotly::plotlyOutput("respPlot"),
            width = 6
          ),

          shinydashboard::box(
            plotly::plotlyOutput("bivarPlot"),
            width = 6
          ),
          shinydashboard::box(
            plotly::plotlyOutput("bivarPlotAlt"),
            width = 6
          ),
          shinydashboard::box(
            #DT::dataTableOutput("predDesc"), width = 6
            DT::dataTableOutput("predDesc"), width = 6
          ),

          shinydashboard::box(
            DT::dataTableOutput("respDesc"), width = 6
          ),
        ),

        shinydashboard::tabItem(
          tabName = "transform",
          shinydashboard::box(
            shiny::uiOutput("numVar"),
            shiny::uiOutput("tf_color"),
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

          tabName = "pps",
          shinydashboard::box(
            title = "Predictive Power Score",
            shiny::plotOutput("ppsPlot"),
            DT::dataTableOutput("ppsTable"),
            width = 12
          )

        ),


        shinydashboard::tabItem(

          tabName = "corr",
          shinydashboard::box(
            title = "Pearson Correlations",
            shiny::plotOutput("corrPlot"),
            DT::dataTableOutput("corrTable"),
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
        choices = data %>% colnames()
      )
    })

    output$resp <- shiny::renderUI({
      shiny::selectInput(
        inputId = "resp",
        label = "Response",
        choices = data %>% colnames()
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
        ggplot2::ggplot(aes(.data[[input$numVar]])) +
        ggplot2::geom_histogram(stat = "bin",
                       bins = 10,
                       aes(fill = input$tf_color)) +
        ggplot2::theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
        ggplot2::labs(x = numText, y = "Count", title = base::paste0("Original Data for ", numText)) +
        ggplot2::scale_fill_manual(values = input$tf_color)

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
        ggplot2::ggplot(aes(.data[[input$numVar]])) +
        ggplot2::geom_histogram(stat = "bin",
                       bins = 10, aes(fill = input$tf_color)) +
        ggplot2::theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
        ggplot2::labs(x = numText, y = "Count", title = paste0("Box-Cox Transformation of ", numText, " with Lambda = ", bc_val)) +
        ggplot2::scale_fill_manual(values = input$tf_color)

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
        ggplot2::ggplot(aes(.data[[input$numVar]])) +
        ggplot2::geom_histogram(bins = 10, aes(fill = input$tf_color)) +
        ggplot2::ggtitle(paste0("Log Transformation of ", numText)) +
        ggplot2::theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
        ggplot2::labs(x = numText, y = "Count") +
        ggplot2::scale_fill_manual(values = input$tf_color)

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
        ggplot2::ggplot(aes(.data[[input$numVar]])) +
        ggplot2::geom_histogram(bins = 10, aes(fill = input$tf_color)) +
        ggplot2::ggtitle(paste0("Square Root Transformation of ", numText)) +
        ggplot2::theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
        ggplot2::labs(x = numText, y = "Count") +
        ggplot2::scale_fill_manual(values = input$tf_color)

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
        ggplot2::ggplot(aes(.data[[input$numVar]])) +
        ggplot2::geom_histogram(bins = 10, aes(fill = input$tf_color)) +
        ggplot2::ggtitle(paste0("Yeo-Johnson Transformation of ", numText)) +
        ggplot2::theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
        ggplot2::labs(x = numText, y = "Count") +
        ggplot2::scale_fill_manual(values = input$tf_color)

      plotly::ggplotly(plot)


    })

    output$tf_color <- shiny::renderUI({
      selectInput(
        inputId = "tf_color",
        label = "Color",
        choices = c("royalblue", "tomato4", "darkorange", "chocolate",
                    "darkgoldenrod", "yellow4",
                    "green4","darkorchid4", "lightpink4", "darkgray",
                    "black", "gray", "white", "#1f5448", "#5f846b",
                    "#96ad92", "#96ad92", "#b5c762",
                    "#c7d7ca",
                    "#d3e0d5",
                    "#326f48",
                    "#699269",
                    "#89b284",
                    "#a9cd9c",
                    "#bed9ac",
                    "#cde0b8",
                    "#909c4d",
                    "#a1c47c",
                    "#c1d77e",
                    "#cbe670",
                    "#cedc00",
                    "#e0e721",
                    "#fece37",
                    "#ffda67",
                    "#e79218",
                    "#f58f0e",
                    "#6c3279",
                    "#981d97",
                    "#c73c46",
                    "#d05b56",
                    "#f25240",
                    "#8c7d58",
                    "#b9aa7e",
                    "#d6cda6",
                    "#313c44",
                    "#6a6f71",
                    "#8e8e90",
                    "#b3b7ad",
                    "#ced0ca",
                    "#e0e0dc",
                    "#4f6e84",
                    "#6e889b",
                    "#899daa",
                    "#bac2c4",
                    "#cbd1d2",
                    "#081a42",
                    "#05206f",
                    "#224d8c",
                    "#2655b0",
                    "#8190c8",
                    "#b6bfda",
                    "#00708e",
                    "#00737f",
                    "#00a0aa",
                    "#5aa096",
                    "#00557f",
                    "#00aa7f",
                    "#1d7576",
                    "#009a44"
        )
      )
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

    output$color <- shiny::renderUI({
      selectInput(
        inputId = "color",
        label = "Bar Color",
        choices = c("royalblue", "tomato4", "darkorange", "chocolate", "darkgoldenrod", "yellow4",
                    "green4","darkorchid4", "lightpink4", "darkgray",
                    "black", "gray", "white", "#1f5448", "#5f846b",
                    "#96ad92", "#96ad92", "#b5c762",
                    "#c7d7ca",
                    "#d3e0d5",
                    "#326f48",
                    "#699269",
                    "#89b284",
                    "#a9cd9c",
                    "#bed9ac",
                    "#cde0b8",
                    "#909c4d",
                    "#a1c47c",
                    "#c1d77e",
                    "#cbe670",
                    "#cedc00",
                    "#e0e721",
                    "#fece37",
                    "#ffda67",
                    "#e79218",
                    "#f58f0e",
                    "#6c3279",
                    "#981d97",
                    "#c73c46",
                    "#d05b56",
                    "#f25240",
                    "#8c7d58",
                    "#b9aa7e",
                    "#d6cda6",
                    "#313c44",
                    "#6a6f71",
                    "#8e8e90",
                    "#b3b7ad",
                    "#ced0ca",
                    "#e0e0dc",
                    "#4f6e84",
                    "#6e889b",
                    "#899daa",
                    "#bac2c4",
                    "#cbd1d2",
                    "#081a42",
                    "#05206f",
                    "#224d8c",
                    "#2655b0",
                    "#8190c8",
                    "#b6bfda",
                    "#00708e",
                    "#00737f",
                    "#00a0aa",
                    "#5aa096",
                    "#00557f",
                    "#00aa7f",
                    "#1d7576",
                    "#009a44")
      )
    })

    output$set_color <- shiny::renderUI({
      selectInput(
        inputId = "set_color",
        label = "Stacked Bar Color",
        choices = c("Set2", "Set1", "Set3",
                    "Pastel2", "Pastel3", "Dark2", "Paired",
                    "RdYlBu", "PuOr", "PRGn", "PiYG", "BrGB",
                    "Spectral", "YlGnBu", "YlGn", "RdPu", "Reds",
                    "PuBuGn", "Oranges", "OrRd", "Greys", "Greens",
                    "GnBu")
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
          ggplot2::ggplot(aes(.data[[input$pred]])) +
          ggplot2::geom_col(aes(y = .data[["n"]]),
                   position = "dodge",
                   fill = input$color) +
          ggplot2::labs(x = predText, y = "Count") +
          ggplot2::ggtitle(paste0("Distribution of ", predText)) +
          ggplot2::theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0))

        plotly::ggplotly(plot)


      }

      else if(guess_cat_num(data[[input$pred]]) == "num") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]]) %>%
          ggplot2::ggplot(aes(.data[[input$pred]])) +
          ggplot2::geom_density(aes(fill = input$color, alpha = .7)) +
          ggplot2::ggtitle(paste0("Distribution of ", predText)) +
          ggplot2::theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0)) +
          ggplot2::scale_fill_manual(values = input$color) +
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
          ggplot2::ggplot(aes(.data[[input$resp]])) +
          ggplot2::geom_col(aes(y = .data[["n"]]),
                   position = "dodge",
                   fill = input$color) +
          ggplot2::labs(x = respText, y = "Count") +
          ggplot2::ggtitle(paste0("Distribution of ", respText)) +
          ggplot2::theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0))

        plotly::ggplotly(plot)


      }

      else if(guess_cat_num(data[[input$resp]]) == "num") {
        #Some other graphical component will go here
        plot <- data %>%
          dplyr::select(.data[[input$resp]]) %>%
          ggplot2::ggplot(aes(.data[[input$resp]])) +
          ggplot2::geom_density(aes(fill = input$color, alpha = .7)) +
          ggplot2::ggtitle(paste0("Distribution of ", respText)) +
          ggplot2::theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0)) +
          ggplot2::scale_fill_manual(values = input$color) +
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
          ggplot2::ggplot(aes(x = .data[[input$pred]], y = .data[[input$resp]])) +
          ggplot2::geom_point() +
          ggplot2::geom_smooth() +
          ggplot2::ggtitle(paste0("Scatterplot of ", predText, " and ", respText)) +
          ggplot2::labs(x = predText, y = respText)

        plotly::ggplotly(plot)
      }

      else if(guess_cat_num(data[[input$pred]]) == "cat" & guess_cat_num(data[[input$resp]]) == "num") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          ggplot2::ggplot(aes(x = .data[[input$pred]], y = .data[[input$resp]])) +
          ggplot2::geom_boxplot(fill = input$color, alpha = .7) +
          ggplot2::ggtitle(paste0("Boxplot of ", respText, " Split On ", predText))


        plotly::ggplotly(plot)
      }

      else if(guess_cat_num(data[[input$pred]]) == "num" & guess_cat_num(data[[input$resp]]) == "cat") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          ggplot2::ggplot(aes(x = .data[[input$resp]], y = .data[[input$pred]] )) +
          ggplot2::geom_boxplot(fill = input$color, alpha = .7) +
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
          ggplot2::ggplot(aes(x = .data[[input$resp]], y = .data[["n"]])) +
          ggplot2::geom_col(aes(fill = .data[[input$pred]])) +
          ggplot2::labs(x = predText, y = "Count") +
          ggplot2::scale_fill_brewer(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Stacked Bar Plot of ", predText, " Split On ", respText)) +
          ggplot2::theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0)) +
          ggplot2::guides(fill = guide_legend(title = respText))

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
          ggplot2::ggplot(aes(x = .data[[input$pred]], y = .data[[input$resp]], color = pc)) +
          ggplot2::geom_point(shape = 16, size = 3, alpha = .7, show.legend = FALSE) +
          ggplot2::scale_color_gradient(low = "#0091ff", high = "#f0650e") +
          ggplot2::ggtitle(paste0("Scatterplot of ", predText, " and ", respText)) +
          ggplot2::labs(x = predText, y = respText)

        plotly::ggplotly(plot)
      }

      else if(guess_cat_num(data[[input$pred]]) == "cat" & guess_cat_num(data[[input$resp]]) == "num") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          ggplot2::ggplot(aes(x = .data[[input$resp]], fill = .data[[input$pred]])) +
          ggplot2::geom_density(alpha = .7) +
          ggplot2::scale_fill_brewer(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Density Plot of ", respText, " Split On ", predText)) +
          ggplot2::labs(x = respText, y = "Density")

        plotly::ggplotly(plot)
      }

      else if(guess_cat_num(data[[input$pred]]) == "num" & guess_cat_num(data[[input$resp]]) == "cat") {
        plot <- data %>%
          dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
          ggplot2::ggplot(aes(x = .data[[input$pred]], fill = .data[[input$resp]])) +
          ggplot2::geom_density(alpha = .7) +
          ggplot2::scale_fill_brewer(palette = input$set_color) +
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
          ggplot2::ggplot(aes(x = .data[[input$pred]], y = .data[["n"]])) +
          ggplot2::geom_col(aes(fill = .data[[input$resp]]), position = "dodge") +
          ggplot2::labs(x = predText, y = "Count") +
          ggplot2::scale_fill_brewer(palette = input$set_color) +
          ggplot2::ggtitle(paste0("Grouped Bar Plot of ", predText, " Split On ", respText)) +
          ggplot2::theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0)) +
          ggplot2::guides(fill = guide_legend(title = respText))

        plotly::ggplotly(plot)
      }

    })

    ## Descriptions for Predictor and Response Variable

    output$predDesc <- DT::renderDataTable({
      if(guess_cat_num(data[[input$pred]]) == "num") {
        data %>%
          dplyr::select(.data[[input$pred]]) %>%
          dlookr::describe() %>%
          dplyr::select(variable, na, mean, sd, se_mean, IQR, skewness, kurtosis, p00, p25, p50, p75, p100) %>%
          purrr::set_names("Predictor", "Missing", "Mean", "SD", "SE Mean", "IQR", "Skewness", "Kurtosis",
                    "Min", "25th Percentile", "Median", "75th Percentile", "Max") %>%
          base::t() %>%
          base::as.data.frame() %>%
          tibble::rownames_to_column("row_names") %>%
          purrr::set_names("Metric", "Value") %>%
          DT::datatable(options = list(pageLength = 15), rownames = NULL)

      }
      else{
        data %>%
          dplyr::select(.data[[input$pred]]) %>%
          dlookr::diagnose_category() %>%
          DT::datatable(options = list(pageLength = 15), rownames = NULL)
      }

    })

    output$respDesc <- DT::renderDataTable({
      if(guess_cat_num(data[[input$resp]]) == "num") {
        data %>%
          dplyr::select(.data[[input$resp]]) %>%
          dlookr::describe() %>%
          dplyr::select(variable, na, mean, sd, se_mean, IQR, skewness, kurtosis, p00, p25, p50, p75, p100) %>%
          purrr::set_names("Predictor", "Missing", "Mean", "SD", "SE Mean", "IQR", "Skewness", "Kurtosis",
                    "Min", "25th Percentile", "Median", "75th Percentile", "Max") %>%
          base::t() %>%
          base::as.data.frame() %>%
          tibble::rownames_to_column("row_names") %>%
          purrr::set_names("Metric", "Value") %>%
          DT::datatable(options = list(pageLength = 15), rownames = NULL)

      }
      else{
        data %>%
          dplyr::select(.data[[input$resp]]) %>%
          dlookr::diagnose_category() %>%
          DT::datatable(options = list(pageLength = 15), rownames = NULL)
      }

    })

    ##Currently not using compareTable

    # output$compareTable <- renderTable({
    #     data %>%
    #         dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
    #         group_by(.data[[input$pred]], .data[[input$resp]]) %>%
    #         summarize(Count = n()) %>%
    #         mutate(Percent = round(Count / sum(Count) * 100, 1))
    # })

    ## Basic interactive data table output of all data

    output$dataTable <- DT::renderDataTable({
      DT::datatable(data = data, filter = "top")
    })

    # Predictive Power Score

    output$ppsTable <- DT::renderDataTable({
      ppsMat <- ppsr::score_matrix(data)
      DT::datatable(round(ppsMat,2), filter = "top", rownames = NULL)
    })

    output$ppsPlot <- shiny::renderPlot({
      ppsMat <- ppsr::score_matrix(data)
      ggcorrplot::ggcorrplot(ppsMat,
                             title = "Predictive Power Score Plot of Data",
                             show.legend = FALSE,
                             type = "upper",
                             lab = TRUE,
                             lab_size = 3.5)
    })

    # Correlation Graphs

    output$corrPlot <- shiny::renderPlot({
      corMat <- rstatix::cor_mat(data, names(dplyr::select_if(data, is.numeric)))
      ggcorrplot::ggcorrplot(corMat,
                             title = "Correlation Plot of Data",
                             #colors = c("blue", "white", "orange"),
                             hc.order = TRUE,
                             type = "upper",
                             lab = TRUE,
                             insig = "blank",
                             lab_size = 3.5)

    })

    output$corrTable <- DT::renderDataTable({
      corMat <- rstatix::cor_mat(data, names(dplyr::select_if(data, is.numeric)))
      DT::datatable(corMat, filter = "top", rownames = NULL)
    })

    # df_products_upload <- reactive({
    #   inFile <- input$target_upload
    #   if (is.null(inFile))
    #     return(NULL)
    #   data <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
    #   return(data)
    # })

    # # Downloadable csv of selected dataset ----
    # output$downloadData <- downloadHandler(
    #     filename = function() {
    #         paste("PPSTable", ".csv", sep = "")
    #     },
    #     content = function(file) {
    #         write.csv(ppsTable(), file, row.names = FALSE)
    #     }
    # )
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
