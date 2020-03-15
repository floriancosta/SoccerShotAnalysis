library(shiny)
library(tidyverse)
library(rintrojs)
library(plotly)
library(modelr)
library(infer)

soccer <- read_csv("soccer.csv", col_names = c("id", "event_type", "player", "placement", "is_goal", "bodypart"))
soccer_clean <- soccer %>%
    select(
        player, placement, bodypart, is_goal
    ) %>% 
    drop_na() %>% 
    mutate(
        player = as.factor(player),
        placement = as.factor(placement),
        bodypart = as.factor(bodypart),
        is_goal = as.double(is_goal)
    )

i <- sample(x = 1:nrow(soccer_clean), size = floor(.06*nrow(soccer_clean)), replace = FALSE)
soccer_shots <- soccer_clean[i, ]

soccer_shots <- soccer_shots %>% 
    select(
        placement, bodypart, is_goal
    ) %>% 
    mutate(
        is_goal = as.factor(is_goal)
    )

shots1 <- soccer_shots %>% 
    filter(
        placement == "1"
    )
shots1["placement_x"] <- runif(nrow(shots1), 8, 31)
shots1["placement_y"] <- runif(nrow(shots1), 13, 16)

shots2 <- soccer_shots %>% 
    filter(
        placement == "2"
    )
shots2["placement_x"] <- runif(nrow(shots2), 11, 28)
shots2["placement_y"] <- runif(nrow(shots2), 0, 10)

shots3 <- soccer_shots %>% 
    filter(
        placement == "3"
    )
shots3["placement_x"] <- runif(nrow(shots3), 9, 15)
shots3["placement_y"] <- runif(nrow(shots3), 0, 6)

shots4 <- soccer_shots %>% 
    filter(
        placement == "4"
    )
shots4["placement_x"] <- runif(nrow(shots4), 24, 30)
shots4["placement_y"] <- runif(nrow(shots4), 0, 6)

shots5 <- soccer_shots %>% 
    filter(
        placement == "5"
    )
shots5["placement_x"] <- runif(nrow(shots5), 15, 24)
shots5["placement_y"] <- runif(nrow(shots5), 0, 8)

shots6 <- soccer_shots %>% 
    filter(
        placement == "6"
    )
shots6["placement_x"] <- sample(c(runif(nrow(shots6), 0, 8), runif(nrow(shots6), 31, 39)), size = nrow(shots6))
shots6["placement_y"] <- runif(nrow(shots6), 12, 20)

shots7 <- soccer_shots %>% 
    filter(
        placement == "7"
    )
sample7 <- sample(x = 1:nrow(shots7), size = floor(.4*nrow(shots7)), replace = FALSE)
crossbar7 <- shots7[sample7, ]
crossbar7 ["placement_x"] <- runif(nrow(crossbar7), 8, 31)
crossbar7["placement_y"] <- runif(nrow(crossbar7), 12, 13)
post7 <- shots7[-sample7, ]
post7["placement_x"] <- sample(c(runif(nrow(post7), 8, 9), runif(nrow(post7), 30, 31)), size = nrow(post7))
post7["placement_y"] <- runif(nrow(post7), 0, 13)

shots8 <- soccer_shots %>% 
    filter(
        placement == "8"
    )
shots8["placement_x"] <- runif(nrow(shots8), 0, 8)
shots8["placement_y"] <- runif(nrow(shots8), 0, 12)

shots9 <- soccer_shots %>% 
    filter(
        placement == "9"
    )
shots9["placement_x"] <- runif(nrow(shots9), 31, 39)
shots9["placement_y"] <- runif(nrow(shots9), 0, 12)

shots10 <- soccer_shots %>% 
    filter(
        placement == "10"
    )
shots10["placement_x"] <- runif(nrow(shots10), 8, 31)
shots10["placement_y"] <- runif(nrow(shots10), 16, 20)

shots11 <- soccer_shots %>% 
    filter(
        placement == "11"
    )
shots11["placement_x"] <- runif(nrow(shots11), 15, 24)
shots11["placement_y"] <- runif(nrow(shots11), 7, 12)

shots12 <- soccer_shots %>% 
    filter(
        placement == "12"
    )
shots12["placement_x"] <- runif(nrow(shots12), 9, 18)
shots12["placement_y"] <- runif(nrow(shots12), 6, 12)

shots13 <- soccer_shots %>% 
    filter(
        placement == "13"
    )
shots13["placement_x"] <- runif(nrow(shots13), 21, 30)
shots13["placement_y"] <- runif(nrow(shots13), 6, 12)

shot_placements <- union_all(union_all(union_all(union_all(union_all(union_all(union_all(union_all(union_all(union_all(union_all(union_all(union_all(shots1, shots2),shots3),shots4),shots5),shots6),crossbar7),post7),shots8),shots9),shots10),shots11),shots12),shots13)

ui <- fluidPage(

        titlePanel(tagList(
        img(src = "soccer-logo.png", height = 80, width = 80),
        
        span("Soccer Shot Analysis", 
             span(
                 introBox(
                     actionButton("github",
                                  label = "Data",
                                  icon = icon("kaggle"),
                                  width = "80px",
                                  onclick ="window.open(`https://www.kaggle.com/secareanualin/football-events`, '_blank')",
                                  style="color: #fff; background-color: #008ABC; border-color: #000000"),
                     actionButton("github",
                                  label = "Code",
                                  icon = icon("github"),
                                  width = "80px",
                                  onclick ="window.open(`https://github.com/floriancosta/SoccerShotAnalysis`, '_blank')",
                                  style="color: #fff; background-color: #767676; border-color: #000000"),
                     data.step = 1,
                     data.intro = "View Code"),
                 style = "position:absolute;right:2em;")
             ),
        span(h5("Using data from European soccer league games from 2011-2017"))
        ),
        windowTitle = "Soccer Shot Analysis"),
    
    hr(),
    
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "outcome",
                      label = "Select an outcome:",
                      choiceValues = c("goal", "miss"),
                      choiceNames = c("goal scored", "shot missed"),
                      selected = c("goal", "miss")),
            hr(),
            checkboxGroupInput(inputId = "method",
                               label = "Select a shot method:",
                               choiceValues = c("left", "right"),
                               choiceNames = c("left foot", "right foot"),
                               selected = c("left", "right")),
            hr(),
            selectInput(inputId = "confidence",
                        label = "Efficiency Confidence Level:",
                        
                        choices = c("95% confidence", 
                                        "90% confidence", 
                                        "80% confidence"),
                        selected = "95% confidence"
                        ),
            width = 3
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Shot Placement Plot", h3("Distribution of Shot Placements"), plotlyOutput(outputId = "shotPlot")), 
                tabPanel("Top Scorers", br(), tableOutput("scorerTable"), align = "center"), 
                tabPanel("Most Efficient Players", br(), tableOutput("efficientTable"), align = "center")
            ),
            hr(),
            h4("Bootstrapped Confidence Intervals"),
            h5("Approximating True Mean Efficiency:"),
            tableOutput(outputId = "meanIntervalTable"),
            h5("Approximating True Median Efficiency:"),
            tableOutput(outputId = "medianIntervalTable"),
            align = "center",
        )
    )
    
)

server <- function(input, output) {
    
    output$shotPlot <- renderPlot({
        hist(rnorm(100))
    })
    
    scorer_Data <- reactive({
        if ("left" %in% input$method && "right" %in% input$method) {
            soccer_clean %>% 
                select(player, is_goal) %>%
                filter(
                    is_goal == 1
                ) %>% 
                group_by(player) %>% 
                count() %>% 
                top_n(wt = n, n = 10) %>% 
                arrange(desc(n)) %>% 
                rename(
                    goals = n
                )
        } else if ("left" %in% input$method && !("right" %in% input$method)) {
            soccer_clean %>% 
                select(player, is_goal, bodypart) %>%
                filter(
                    bodypart ==2,
                    is_goal == 1
                ) %>% 
                group_by(player) %>% 
                count() %>% 
                top_n(wt = n, n = 10) %>% 
                arrange(desc(n)) %>% 
                rename(
                    goals = n
                )
        } else if (!("left" %in% input$method) && "right" %in% input$method) {
            soccer_clean %>% 
                select(player, is_goal, bodypart) %>%
                filter(
                    bodypart == 1,
                    is_goal == 1
                ) %>% 
                group_by(player) %>% 
                count() %>% 
                top_n(wt = n, n = 10) %>% 
                arrange(desc(n)) %>% 
                rename(
                    goals = n
                )
        } else {
            data.frame(player = character(),
                       goals = double())
        }
    })
    output$scorerTable <- renderTable(
        expr = head({scorer_Data()}, n = 10),
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        rownames = TRUE,
        width = '75%',
        align = 'c'
    )
    
    efficient_Data <- reactive({
        if ("left" %in% input$method && "right" %in% input$method) {
            soccer_clean %>% 
                select(player, is_goal) %>%
                group_by(player, is_goal) %>%
                count() %>% 
                ungroup() %>% 
                pivot_wider(id_cols = player, values_from = n, names_from = is_goal, values_fill = list(n=0)) %>% 
                filter((`0` + `1`) >= 100) %>% 
                mutate(
                    efficiency = `1` / (`0` + `1`)
                ) %>% 
                select(
                    player, efficiency
                ) %>% 
                arrange(desc(efficiency))
        } else if ("left" %in% input$method && !("right" %in% input$method)) {
            soccer_clean %>% 
                select(player, is_goal, bodypart) %>%
                filter(
                    bodypart == 2
                ) %>% group_by(player, is_goal) %>%
                count() %>% 
                ungroup() %>% 
                pivot_wider(id_cols = player, values_from = n, names_from = is_goal, values_fill = list(n=0)) %>% 
                filter((`0` + `1`) >= 50) %>% 
                mutate(
                    efficiency = `1` / (`0` + `1`)
                ) %>% 
                select(
                    player, efficiency
                ) %>% 
                arrange(desc(efficiency))
        } else if (!("left" %in% input$method) && "right" %in% input$method) {
            soccer_clean %>% 
                select(player, is_goal, bodypart) %>%
                filter(
                    bodypart == 1
                ) %>% group_by(player, is_goal) %>%
                count() %>% 
                ungroup() %>% 
                pivot_wider(id_cols = player, values_from = n, names_from = is_goal, values_fill = list(n=0)) %>% 
                filter((`0` + `1`) >= 50) %>% 
                mutate(
                    efficiency = `1` / (`0` + `1`)
                ) %>% 
                select(
                    player, efficiency
                ) %>% 
                arrange(desc(efficiency))
        } else {
            data.frame(player = character(),
                       efficiency = double())
        }
    })
    output$efficientTable <- renderTable(
        expr = head({efficient_Data()}, n = 10),
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        rownames = TRUE,
        width = '75%',
        align = 'c'
    )
    
    meanInterval <- reactive({
        if (input$confidence == "95% confidence") {
            confidence_level <- 0.95
        } else if (input$confidence == "90% confidence") {
            confidence_level <- 0.9
        } else {
            confidence_level <- 0.8
        }
        
        if ("left" %in% input$method && "right" %in% input$method) {
            overall_efficiency <- soccer_clean %>% 
                select(player, is_goal) %>%
                group_by(player, is_goal) %>%
                count() %>% 
                ungroup() %>% 
                pivot_wider(id_cols = player, values_from = n, names_from = is_goal, values_fill = list(n=0)) %>% 
                mutate(
                    efficiency = `1` / (`0` + `1`)
                ) %>% 
                select(
                    player, efficiency
                )
            boots <- bootstrap(data = overall_efficiency, n = 150) %>% 
                mutate(
                    boot_mean = map_dbl(strap, ~mean(data.frame(.x)$efficiency)),
                )
            boots %>% t_test(response = boot_mean, conf_level = confidence_level) %>%
                select(lower_ci, upper_ci) %>% 
                rename(
                    lower_bound = lower_ci,
                    upper_bound = upper_ci
                )
        } else if ("left" %in% input$method && !("right" %in% input$method)) {
            overall_efficiency <- soccer_clean %>% 
                select(player, is_goal,bodypart) %>%
                filter(
                    bodypart == 2
                ) %>% 
                group_by(player, is_goal) %>%
                count() %>% 
                ungroup() %>% 
                pivot_wider(id_cols = player, values_from = n, names_from = is_goal, values_fill = list(n=0)) %>% 
                mutate(
                    efficiency = `1` / (`0` + `1`)
                ) %>% 
                select(
                    player, efficiency
                )
            boots <- bootstrap(data = overall_efficiency, n = 150) %>% 
                mutate(
                    boot_mean = map_dbl(strap, ~mean(data.frame(.x)$efficiency)),
                )
            boots %>% t_test(response = boot_mean, conf_level = confidence_level) %>%
                select(lower_ci, upper_ci) %>% 
                rename(
                    lower_bound = lower_ci,
                    upper_bound = upper_ci
                )
        } else if (!("left" %in% input$method) && "right" %in% input$method) {
            overall_efficiency <- soccer_clean %>% 
                select(player, is_goal, bodypart) %>%
                filter(
                    bodypart == 1
                ) %>% 
                group_by(player, is_goal) %>%
                count() %>% 
                ungroup() %>% 
                pivot_wider(id_cols = player, values_from = n, names_from = is_goal, values_fill = list(n=0)) %>% 
                mutate(
                    efficiency = `1` / (`0` + `1`)
                ) %>% 
                select(
                    player, efficiency
                )
            boots <- bootstrap(data = overall_efficiency, n = 150) %>% 
                mutate(
                    boot_mean = map_dbl(strap, ~mean(data.frame(.x)$efficiency)),
                )
            boots %>% t_test(response = boot_mean, conf_level = confidence_level) %>%
                select(lower_ci, upper_ci) %>% 
                rename(
                    lower_bound = lower_ci,
                    upper_bound = upper_ci
                )
        } else {
            data.frame(player = character(),
                       lower_bound = double(),
                       upper_bound = double())
        }
    })
    output$meanIntervalTable <- renderTable(
        expr = {
            meanInterval()
        },
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        digits = 4,
        width = '75%',
        align = 'c'
    )
    
    medianInterval <- reactive({
        if (input$confidence == "95% confidence") {
            confidence_level <- 0.95
        } else if (input$confidence == "90% confidence") {
            confidence_level <- 0.9
        } else {
            confidence_level <- 0.8
        }
        
        if ("left" %in% input$method && "right" %in% input$method) {
            overall_efficiency <- soccer_clean %>% 
                select(player, is_goal) %>%
                group_by(player, is_goal) %>%
                count() %>% 
                ungroup() %>% 
                pivot_wider(id_cols = player, values_from = n, names_from = is_goal, values_fill = list(n=0)) %>% 
                filter(
                    (`0` + `1`) >= 10
                ) %>% 
                mutate(
                    efficiency = `1` / (`0` + `1`)
                ) %>% 
                select(
                    player, efficiency
                )
            boots <- bootstrap(data = overall_efficiency, n = 150) %>% 
                mutate(
                    boot_median = map_dbl(strap, ~median(data.frame(.x)$efficiency)),
                )
            boots %>% t_test(response = boot_median, conf_level = confidence_level) %>%
                select(lower_ci, upper_ci) %>% 
                rename(
                    lower_bound = lower_ci,
                    upper_bound = upper_ci
                )
        } else if ("left" %in% input$method && !("right" %in% input$method)) {
            overall_efficiency <- soccer_clean %>% 
                select(player, is_goal,bodypart) %>%
                filter(
                    bodypart == 2
                ) %>% 
                group_by(player, is_goal) %>%
                count() %>% 
                ungroup() %>% 
                pivot_wider(id_cols = player, values_from = n, names_from = is_goal, values_fill = list(n=0)) %>% 
                filter(
                    (`0` + `1`) >= 10
                ) %>% 
                mutate(
                    efficiency = `1` / (`0` + `1`)
                ) %>% 
                select(
                    player, efficiency
                )
            boots <- bootstrap(data = overall_efficiency, n = 150) %>% 
                mutate(
                    boot_median = map_dbl(strap, ~median(data.frame(.x)$efficiency)),
                )
            boots %>% t_test(response = boot_median, conf_level = confidence_level) %>%
                select(lower_ci, upper_ci) %>% 
                rename(
                    lower_bound = lower_ci,
                    upper_bound = upper_ci
                )
        } else if (!("left" %in% input$method) && "right" %in% input$method) {
            overall_efficiency <- soccer_clean %>% 
                select(player, is_goal, bodypart) %>%
                filter(
                    bodypart == 1
                ) %>% 
                group_by(player, is_goal) %>%
                count() %>% 
                ungroup() %>% 
                pivot_wider(id_cols = player, values_from = n, names_from = is_goal, values_fill = list(n=0)) %>% 
                filter(
                    (`0` + `1`) >= 10
                ) %>% 
                mutate(
                    efficiency = `1` / (`0` + `1`)
                ) %>% 
                select(
                    player, efficiency
                )
            boots <- bootstrap(data = overall_efficiency, n = 150) %>% 
                mutate(
                    boot_median = map_dbl(strap, ~median(data.frame(.x)$efficiency)),
                )
            boots %>% t_test(response = boot_median, conf_level = confidence_level) %>%
                select(lower_ci, upper_ci) %>% 
                rename(
                    lower_bound = lower_ci,
                    upper_bound = upper_ci
                )
        } else {
            data.frame(player = character(),
                       lower_bound = double(),
                       upper_bound = double())
        }
    })
    output$medianIntervalTable <- renderTable(
        expr = {
            medianInterval()
        },
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        digits = 4,
        width = '75%',
        align = 'c',
        na = 0
    )
    
    shotPlacementPlot <- reactive({
        if ("goal" %in% input$outcome && "miss" %in% input$outcome) {
            if ("left" %in% input$method && "right" %in% input$method) {
                shot_placements
            } else if ("left" %in% input$method && !("right" %in% input$method)) {
                shot_placements %>% 
                    filter(
                        bodypart == "2"
                    )
            } else if (!("left" %in% input$method) && "right" %in% input$method) {
                shot_placements %>% 
                    filter(
                        bodypart == "1"
                    )
            } else {
                data.frame(placement_x = double(),
                           placement_y = double(),
                           is_goal = factor())
            }
        } else if ("goal" %in% input$outcome && !("miss" %in% input$outcome)) {
            if ("left" %in% input$method && "right" %in% input$method) {
                shot_placements %>% 
                    filter(
                        is_goal == "1"
                    )
            } else if ("left" %in% input$method && !("right" %in% input$method)) {
                shot_placements %>% 
                    filter(
                        is_goal == "1",
                        bodypart == "2"
                    )
            } else if (!("left" %in% input$method) && "right" %in% input$method) {
                shot_placements %>% 
                    filter(
                        is_goal == "1",
                        bodypart == "1"
                    )
            } else {
                data.frame(placement_x = double(),
                           placement_y = double(),
                           is_goal = factor())
            }
        } else if (!("goal" %in% input$outcome) && ("miss" %in% input$outcome)) {
            if ("left" %in% input$method && "right" %in% input$method) {
                shot_placements %>% 
                    filter(
                        is_goal == "0"
                    )
            } else if ("left" %in% input$method && !("right" %in% input$method)) {
                shot_placements %>% 
                    filter(
                        is_goal == "0",
                        bodypart == "2"
                    )
            } else if (!("left" %in% input$method) && "right" %in% input$method) {
                shot_placements %>% 
                    filter(
                        is_goal == "0",
                        bodypart == "1"
                    )
            } else {
                data.frame(placement_x = double(),
                           placement_y = double(),
                           is_goal = factor())
            }
        } else {
            data.frame(placement_x = double(),
                       placement_y = double(),
                       is_goal = factor())
        }
        
        
    })
    output$shotPlot <- renderPlotly(
         expr = {
             pal <- c("red", "green")
             data <- shotPlacementPlot()
             plot_ly(data = data, x = ~placement_x, y = ~placement_y, color = ~is_goal, colors = pal, alpha = 0.3, type = "scatter", mode = "markers") %>%
                     layout(
                         showlegend = FALSE,
                         xaxis = list(
                             range = c(0, 39),
                             showticklabels = FALSE,
                             showgrid = FALSE,
                             title = "",
                             showline = FALSE,
                             zeroline = FALSE
                         ),
                         yaxis = list(
                             range = c(0, 20),
                             showticklabels = FALSE,
                             showgrid = FALSE,
                             title = "",
                             showline = FALSE,
                             zeroline = FALSE
                         ),
                         images = list(
                             list(source =  "https://raw.githubusercontent.com/floriancosta/SoccerShotAnalysis/master/soccer-goal.png",
                                  xref = "x",
                                  yref = "y",
                                  x = 8,
                                  y = 13,
                                  sizex = 23,
                                  sizey = 13,
                                  sizing = "stretch",
                                  opacity = 1,
                                  layer = "below"
                             )
                         )
                     )
             
         }
     )
    
}

shinyApp(ui = ui, server = server)
