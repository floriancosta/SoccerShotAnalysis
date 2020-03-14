library(shiny)
library(tidyverse)
library(rintrojs)

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

ui <- fluidPage(

        titlePanel(tagList(
        img(src = "soccer-logo.png", height = 80, width = 80),
        
        span("Soccer Shot Analysis", 
             span(
                 introBox(
                     actionButton("github",
                                  label = "Code",
                                  icon = icon("github"),
                                  width = "80px",
                                  onclick ="window.open(`https://github.com/floriancosta`, '_blank')",
                                  style="color: #fff; background-color: #767676; border-color: #767676"),
                     data.step = 1,
                     data.intro = "View Code"),
                 style = "position:absolute;right:2em;"))),
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
            # leflet frame
            tabsetPanel(
                tabPanel("Shot Placement Plot", plotOutput(outputId = "shotPlot")), 
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

# Define server logic required to draw a histogram
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
    
}

shinyApp(ui = ui, server = server)
