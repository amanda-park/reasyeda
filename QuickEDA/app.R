#Load packages
require(pacman)

# Load in predictive power score package
# install.packages('devtools')
#devtools::install_github('https://github.com/paulvanderlaken/ppsr')

p_load(shiny,
       shinydashboard,
       ggplot2,
       tidyverse,
       plotly,
       DT,
       janitor,
       RODBC,
       modeldata,
       ppsr,
       corrr,
       tidymodels,
       rstatix)

source("edaFunctions.R")

#Load in data
data(penguins, package = "modeldata")
data <- penguins

#Run calculations for correlations and predictive power score here
#These can be rerun outside this script and loaded in as well if the run time is long
corMat <- rstatix::cor_mat(data, names(select_if(data, is.numeric)))
ppsMat <- ppsr::score_matrix(data) 

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "blue",
    # Application title
    dashboardHeader(title = "Quick and Pretty EDA Plots"),

    #Sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Visuals",
                     tabName = "ggplot"),
            menuItem("Transformations",
                     tabName = "transform"),
            menuItem("Table",
                     tabName = "dt"),
            menuItem("Predictive Power Score",
                     tabName = "pps"),
            menuItem("Pearson Correlation",
                     tabName = "corr")
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(
                title = "Visualizations",
                tabName = "ggplot",
                solidHeader = TRUE,
                
                box(
                    uiOutput("pred"),
                    width = 4
                ),
                
                box(
                    uiOutput("resp"),
                    width = 4
                ),
                
                box(
                    uiOutput("color"),
                    uiOutput("set_color"),
                    width = 4
                ),
                
                box(
                    textOutput("ppsText"),
                    width = 8
                ),
                
                box(
                    plotlyOutput("predPlot"),
                    width = 6
                    ),
                box(
                    plotlyOutput("respPlot"),
                    width = 6
                 ),
                
                box(
                    plotlyOutput("bivarPlot"),
                    width = 6
                ),
                box(
                    plotlyOutput("bivarPlotAlt"),
                    width = 6
                ),
                box(
                    dataTableOutput("predDesc"), width = 6
                    ),
                
                box(
                    dataTableOutput("respDesc"), width = 6
                ),
            ),
            
            tabItem(
                tabName = "transform",
                box(
                    uiOutput("numVar"),
                    uiOutput("tf_color"),
                    textOutput("shapiroOrig"),
                    textOutput("shapiroBox"),
                    textOutput("shapiroLog"),
                    textOutput("shapiroSqrt"),
                    textOutput("shapiroYJ"),
                    width = 6
                ),
                
                box(
                    plotlyOutput("origDist"),
                    width = 6
                ),
                
                box(
                    plotlyOutput("boxCox"), width = 6
                ),
                box(
                    plotlyOutput("log"), width = 6
                ),
                box(
                    plotlyOutput("sqrt"), width = 6
                ),
                box(
                    plotlyOutput("yj"), width = 6
                ),
            ),
            
            tabItem(
                
                tabName = "dt",
                box(
                    title = "Data Table",
                    dataTableOutput("dataTable"),
                    width = 12
                )
            ),
            
            tabItem(
                
                tabName = "pps",
                box(
                    title = "Predictive Power Score",
                    plotOutput("ppsPlot"),
                    dataTableOutput("ppsTable"),
                    width = 12
                )
                
            ),
        
            
            tabItem(
                
                tabName = "corr",
                box(
                    title = "Pearson Correlations",
                    plotOutput("corrPlot"),
                    dataTableOutput("corrTable"),
                    width = 12
                )
            )
        )
    )
    
)
    


# Define server logic required to draw a histogram
server <- function(input, output) {

    #Define Variables
    
    output$pred <- renderUI({
        selectInput(
            inputId = "pred",
            label = "Predictor",
            choices = data %>% colnames() 
        )
    })

    output$resp <- renderUI({
        selectInput(
            inputId = "resp",
            label = "Response",
            choices = data %>% colnames() 
        )
    })
    
    output$numVar <- renderUI({
        selectInput(
            inputId = "numVar",
            label = "Numeric Variable",
            choices = data %>% dplyr::select_if(is.numeric) %>% colnames() 
        )
    })
    
    #Transformation Plots
    
    ##Computes Shapiro-Wilk Score
    output$shapiroOrig <- renderText({

        swOrig <- data %>%
                pull(.data[[input$numVar]]) %>%
                shapiro.test()

        paste0("Untransformed Shapiro-Wilk p-value: ", round(swOrig$p.value, 2))
    })
    
    output$shapiroBox <- renderText({
        #Box Cox
        bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
            recipes::step_BoxCox(all_numeric()) %>%
            recipes::prep(data, retain = TRUE)

        data <- juice(bc_recipe)

        swBox <- data %>%
            pull(.data[[input$numVar]]) %>%
            shapiro.test()
        
        paste0("Box-Cox Shapiro-Wilk p-value: ", round(swBox$p.value, 2))
    })
        
    output$shapiroLog <- renderText({
        #Box Cox
        bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
            recipes::step_log(all_numeric()) %>%
            recipes::prep(data, retain = TRUE)
        
        data <- juice(bc_recipe)
        
        swBox <- data %>%
            pull(.data[[input$numVar]]) %>%
            shapiro.test()
        
        paste0("Log Shapiro-Wilk p-value: ", round(swBox$p.value, 2))
    })
    
    output$shapiroSqrt <- renderText({
        #Box Cox
        bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
            recipes::step_sqrt(all_numeric()) %>%
            recipes::prep(data, retain = TRUE)
        
        data <- juice(bc_recipe)
        
        swBox <- data %>%
            pull(.data[[input$numVar]]) %>%
            shapiro.test()
        
        paste0("Square Root Shapiro-Wilk p-value: ", round(swBox$p.value, 2))
    })
    
    output$shapiroYJ <- renderText({
        #Box Cox
        bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
            recipes::step_YeoJohnson(all_numeric()) %>%
            recipes::prep(data, retain = TRUE)
        
        data <- juice(bc_recipe)
        
        swBox <- data %>%
            pull(.data[[input$numVar]]) %>%
            shapiro.test()
        
        paste0("Yeo-Johnson Shapiro-Wilk p-value: ", round(swBox$p.value, 2))
    })
    
    ##Create Plotly Graphs
    output$origDist <- renderPlotly({
        
        numText <- input$numVar %>%
            stringr::str_replace_all("_", " ") %>%
            stringr::str_to_title()
        
        plot <- data %>% 
            dplyr::select(.data[[input$numVar]]) %>%
            ggplot(aes(.data[[input$numVar]])) +
            geom_histogram(stat = "bin",
                           bins = 10,
                           aes(fill = input$tf_color)) + 
            theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
            labs(x = numText, y = "Count", title = paste0("Original Data for ", numText)) +
            scale_fill_manual(values = input$tf_color)
        
        ggplotly(plot) 

    })
    
    output$boxCox <- renderPlotly({
        
        numText <- input$numVar %>%
            stringr::str_replace_all("_", " ") %>%
            stringr::str_to_title()

        formula <- as.formula(paste(input$numVar , ".", sep = "~"))

        bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
            recipes::step_BoxCox(all_numeric()) %>%
            recipes::prep(data, retain = TRUE)

        data <- juice(bc_recipe)

        bc_val <- tidy(bc_recipe, number = 1) %>% 
            filter(terms == input$numVar) %>%
            select(value) %>%
            as.numeric() %>%
            round(2)
        
        sw <- data %>% 
            pull(.data[[input$numVar]]) %>%
            shapiro.test()
        
        plot <- data %>% 
            dplyr::select(.data[[input$numVar]]) %>%
            ggplot(aes(.data[[input$numVar]])) +
            geom_histogram(stat = "bin",
                           bins = 10, aes(fill = input$tf_color)) + 
            theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
            labs(x = numText, y = "Count", title = paste0("Box-Cox Transformation of ", numText, " with Lambda = ", bc_val)) +
            scale_fill_manual(values = input$tf_color)
        
        ggplotly(plot) 
    })
    
    output$log <- renderPlotly({
        
        numText <- input$numVar %>%
            stringr::str_replace_all("_", " ") %>%
            stringr::str_to_title()
        
        formula <- as.formula(paste(input$numVar , ".", sep = "~"))
        
        bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
            recipes::step_log(all_numeric()) %>%
            recipes::prep(data, retain = TRUE)
        
        data <- juice(bc_recipe)

        
        plot <- data %>% 
            dplyr::select(.data[[input$numVar]]) %>%
            ggplot(aes(.data[[input$numVar]])) +
            geom_histogram(bins = 10, aes(fill = input$tf_color)) + 
            ggtitle(paste0("Log Transformation of ", numText)) + 
            theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
            labs(x = numText, y = "Count") +
            scale_fill_manual(values = input$tf_color)
        
        ggplotly(plot)
        
        
    })
    
    output$sqrt <- renderPlotly({
        
        numText <- input$numVar %>%
            stringr::str_replace_all("_", " ") %>%
            stringr::str_to_title()
        
        bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
            recipes::step_sqrt(all_numeric()) %>%
            recipes::prep(data, retain = TRUE)
        
        data <- juice(bc_recipe)
        
        plot <- data %>% 
            dplyr::select(.data[[input$numVar]]) %>%
            ggplot(aes(.data[[input$numVar]])) +
            geom_histogram(bins = 10, aes(fill = input$tf_color)) +  
            ggtitle(paste0("Square Root Transformation of ", numText)) + 
            theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
            labs(x = numText, y = "Count") +
            scale_fill_manual(values = input$tf_color)
        
        ggplotly(plot)
        
        
    })
    
    output$yj <- renderPlotly({
        
        numText <- input$numVar %>%
            stringr::str_replace_all("_", " ") %>%
            stringr::str_to_title()
        
        formula <- as.formula(paste(input$numVar , ".", sep = "~"))
        
        bc_recipe <- recipes::recipe(x = data, formula = NULL) %>%
            recipes::step_YeoJohnson(all_numeric()) %>%
            recipes::prep(data, retain = TRUE)
        
        data <- juice(bc_recipe)
        
        plot <- data %>% 
            dplyr::select(.data[[input$numVar]]) %>%
            ggplot(aes(.data[[input$numVar]])) +
            geom_histogram(bins = 10, aes(fill = input$tf_color)) + 
            ggtitle(paste0("Yeo-Johnson Transformation of ", numText)) + 
            theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
            labs(x = numText, y = "Count") +
            scale_fill_manual(values = input$tf_color)
        
        ggplotly(plot)
        
        
    })
    
    output$tf_color <- renderUI({
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
    
    output$ppsText <- renderText({
        ppsCalc <- data %>%
            dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
            ppsr::score_matrix()
        print(paste0("The Predictive Power Score Between ", 
                     input$pred, " and ", input$resp, " is: ", 
                     round(ppsCalc[2], 2)))
    })
    
    output$color <- renderUI({
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
    
    output$set_color <- renderUI({
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
    
    output$predPlot <- renderPlotly({
        
        predText <- input$pred %>%
            stringr::str_replace_all("_", " ") %>%
            stringr::str_to_title()
        
        if(guess_cat_num(data[[input$pred]]) == "cat") {
            plot <- data %>%
                dplyr::select(.data[[input$pred]]) %>%
                group_by(.data[[input$pred]]) %>%
                summarize(n = n()) %>%
                mutate(pct = round(n / sum(n) * 100, 1)) %>%
                ggplot(aes(.data[[input$pred]])) +
                geom_col(aes(y = .data[["n"]]),
                         position = "dodge",
                         fill = input$color) +
                labs(x = predText, y = "Count") +
                ggtitle(paste0("Distribution of ", predText)) +
                theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") 
                
            ggplotly(plot)
            
            
        }
        
        else if(guess_cat_num(data[[input$pred]]) == "num") {
            plot <- data %>%
                dplyr::select(.data[[input$pred]]) %>%
                ggplot(aes(.data[[input$pred]])) +
                geom_density(aes(fill = input$color, alpha = .7)) + 
                ggtitle(paste0("Distribution of ", predText)) + 
                theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
                scale_fill_manual(values = input$color) + 
                labs(x = predText, y = "Density")
            
            ggplotly(plot)
            
        }
        
    })
    
    output$respPlot <- renderPlotly({
        
        respText <- input$resp %>%
            stringr::str_replace_all("_", " ") %>%
            stringr::str_to_title()
        
        if(guess_cat_num(data[[input$resp]]) == "cat") {
            plot <- data %>%
                dplyr::select(.data[[input$resp]]) %>%
                group_by(.data[[input$resp]]) %>%
                summarize(n = n()) %>%
                mutate(pct = round(n / sum(n) * 100, 1)) %>%
                ggplot(aes(.data[[input$resp]])) +
                geom_col(aes(y = .data[["n"]]),
                         position = "dodge",
                         fill = input$color) +
                labs(x = respText, y = "Count") +
                ggtitle(paste0("Distribution of ", respText)) +
                theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") 
            
            ggplotly(plot)
            
            
        }
        
        else if(guess_cat_num(data[[input$resp]]) == "num") {
            #Some other graphical component will go here
            plot <- data %>%
                dplyr::select(.data[[input$resp]]) %>%
                ggplot(aes(.data[[input$resp]])) +
                geom_density(aes(fill = input$color, alpha = .7)) + 
                ggtitle(paste0("Distribution of ", respText)) + 
                theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0), legend.position = "none") +
                scale_fill_manual(values = input$color) +
                labs(x = respText, y = "Density")
            
            ggplotly(plot)
            
        }
        
    })
    
    output$bivarPlot <- renderPlotly({
        predText <- input$pred %>%
            stringr::str_replace_all("_", " ") %>%
            stringr::str_to_title()
        
        respText <- input$resp %>%
            stringr::str_replace_all("_", " ") %>%
            stringr::str_to_title()
        
        if(guess_cat_num(data[[input$pred]]) == "num" & guess_cat_num(data[[input$resp]]) == "num") {
            plot <- data %>%
                dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
                ggplot(aes(x = .data[[input$pred]], y = .data[[input$resp]])) +
                geom_point() +
                geom_smooth() +
                ggtitle(paste0("Scatterplot of ", predText, " and ", respText)) +
                labs(x = predText, y = respText) 
            
            ggplotly(plot)
        }
        
        else if(guess_cat_num(data[[input$pred]]) == "cat" & guess_cat_num(data[[input$resp]]) == "num") {
            plot <- data %>%
                dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
                ggplot(aes(x = .data[[input$pred]], y = .data[[input$resp]])) +
                geom_boxplot(fill = input$color, alpha = .7) +
                ggtitle(paste0("Boxplot of ", respText, " Split On ", predText)) 


            ggplotly(plot)
        }
        
        else if(guess_cat_num(data[[input$pred]]) == "num" & guess_cat_num(data[[input$resp]]) == "cat") {
            plot <- data %>%
                dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
                ggplot(aes(x = .data[[input$resp]], y = .data[[input$pred]] )) +
                geom_boxplot(fill = input$color, alpha = .7) +
                ggtitle(paste0("Boxplot of ", respText, " Split On ", predText)) +
                coord_flip() 
            
            ggplotly(plot)
        }
        
        else {
            plot <- data %>%
                dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
                group_by(.data[[input$pred]], .data[[input$resp]]) %>%
                summarize(n = n()) %>%
                mutate(pct = round(n / sum(n) * 100, 1)) %>%
                ggplot(aes(x = .data[[input$resp]], y = .data[["n"]])) +
                geom_col(aes(fill = .data[[input$pred]])) +
                labs(x = predText, y = "Count") +
                scale_fill_brewer(palette = input$set_color) +
                ggtitle(paste0("Stacked Bar Plot of ", predText, " Split On ", respText)) +
                theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0)) +
                guides(fill = guide_legend(title = respText))
            
            ggplotly(plot) 
        }
        
    })

    output$bivarPlotAlt <- renderPlotly({
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
                ggplot(aes(x = .data[[input$pred]], y = .data[[input$resp]], color = pc)) +
                geom_point(shape = 16, size = 3, alpha = .7, show.legend = FALSE) +
                scale_color_gradient(low = "#0091ff", high = "#f0650e") +
                ggtitle(paste0("Scatterplot of ", predText, " and ", respText)) +
                labs(x = predText, y = respText)
            
            ggplotly(plot)
        }
        
        else if(guess_cat_num(data[[input$pred]]) == "cat" & guess_cat_num(data[[input$resp]]) == "num") {
            plot <- data %>%
                dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
                ggplot(aes(x = .data[[input$resp]], fill = .data[[input$pred]])) +
                geom_density(alpha = .7) +
                scale_fill_brewer(palette = input$set_color) +
                ggtitle(paste0("Density Plot of ", respText, " Split On ", predText)) +
                labs(x = respText, y = "Density")
            
            ggplotly(plot)
        }
        
        else if(guess_cat_num(data[[input$pred]]) == "num" & guess_cat_num(data[[input$resp]]) == "cat") {
            plot <- data %>%
                dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
                ggplot(aes(x = .data[[input$pred]], fill = .data[[input$resp]])) +
                geom_density(alpha = .7) +
                scale_fill_brewer(palette = input$set_color) +
                ggtitle(paste0("Density Plot of ", respText, " Split On ", predText)) + 
                labs(x = predText, y = "Density")
            
            ggplotly(plot)
        }
        
        else {
            plot <- data %>%
                dplyr::select(.data[[input$pred]], .data[[input$resp]]) %>%
                group_by(.data[[input$pred]], .data[[input$resp]]) %>%
                summarize(n = n()) %>%
                mutate(pct = round(n / sum(n) * 100, 1)) %>%
                ggplot(aes(x = .data[[input$pred]], y = .data[["n"]])) +
                geom_col(aes(fill = .data[[input$resp]]), position = "dodge") +
                labs(x = predText, y = "Count") +
                scale_fill_brewer(palette = input$set_color) +
                ggtitle(paste0("Grouped Bar Plot of ", predText, " Split On ", respText)) +
                theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0)) +
                guides(fill = guide_legend(title = respText))
            
            ggplotly(plot) 
        }
        
    })
    
    ## Descriptions for Predictor and Response Variable
    
    output$predDesc <- renderDataTable({
        if(guess_cat_num(data[[input$pred]]) == "num") {
            data %>%
                dplyr::select(.data[[input$pred]]) %>%
                dlookr::describe() %>%
                dplyr::select(variable, na, mean, sd, se_mean, IQR, skewness, kurtosis, p00, p25, p50, p75, p100) %>%
                set_names("Predictor", "Missing", "Mean", "SD", "SE Mean", "IQR", "Skewness", "Kurtosis",
                          "Min", "25th Percentile", "Median", "75th Percentile", "Max") %>%
                t() %>%
                as.data.frame() %>%
                rownames_to_column("row_names") %>% 
                set_names("Metric", "Value") %>%
                datatable(options = list(pageLength = 15))
                
        }
        else{
            data %>%
                dplyr::select(.data[[input$pred]]) %>%
                dlookr::diagnose_category() %>%
                datatable(options = list(pageLength = 15))
        }
         
    })
    
    output$respDesc <- renderDataTable({
        if(guess_cat_num(data[[input$resp]]) == "num") {
            data %>%
                dplyr::select(.data[[input$resp]]) %>%
                dlookr::describe() %>%
                dplyr::select(variable, na, mean, sd, se_mean, IQR, skewness, kurtosis, p00, p25, p50, p75, p100) %>%
                set_names("Predictor", "Missing", "Mean", "SD", "SE Mean", "IQR", "Skewness", "Kurtosis",
                          "Min", "25th Percentile", "Median", "75th Percentile", "Max") %>%
                t() %>%
                as.data.frame() %>%
                rownames_to_column("row_names") %>% 
                set_names("Metric", "Value") %>%
                datatable(options = list(pageLength = 15))
            
        }
        else{
            data %>%
                dplyr::select(.data[[input$resp]]) %>%
                dlookr::diagnose_category() %>%
                datatable(options = list(pageLength = 15))
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
    
    output$dataTable <- renderDataTable({
        DT::datatable(data = data, filter = "top")
    })
    
    # Predictive Power Score 
    
    output$ppsTable <- renderDataTable({
        DT::datatable(round(ppsMat,2), filter = "top")
    })
    
    output$ppsPlot <- renderPlot({
        ggcorrplot::ggcorrplot(ppsMat,
                               title = "Predictive Power Score Plot of Data",
                               show.legend = FALSE,
                               type = "upper",
                               lab = TRUE,
                               lab_size = 3.5) 
    })
    
    # Correlation Graphs
    
    output$corrPlot <- renderPlot({

        
        ggcorrplot::ggcorrplot(corMat,
                               title = "Correlation Plot of Data",
                               colors = c("blue", "white", "orange"),
                               hc.order = TRUE,
                               type = "upper",
                               lab = TRUE,
                               insig = "blank", 
                               lab_size = 3.5) 

    })
    
    output$corrTable <- renderDataTable({
        DT::datatable(corMat, filter = "top")
    })
    
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
shinyApp(ui = ui, server = server)
