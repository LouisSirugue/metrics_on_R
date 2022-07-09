
library(shiny)
library(plotly)
library(tidyverse)
library(stargazer)
library(shinyWidgets)

dep_data <- as_tibble(read.csv("dep_data.csv"))
depvars <- names(dep_data)[!names(dep_data) %in% c("Department", "Year")]

ui <- fluidPage(
  
  HTML("<center><h2>Relationships between department characteristics</h2></center><br><br>"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 HTML("<center><h4><b>Select inputs</b></h4></center><br>"),
                 
                 selectInput(inputId = "xvar", label = "X variable:", 
                             choices = depvars, selected = "Log.population"),
                 
                 selectInput(inputId = "yvar", label = "Y variable:", 
                             choices = depvars, selected = "PM2.5.concentration"),
                 
                 sliderTextInput(inputId = "year", label = "Select Year:",
                                 choices = 2012:2017, selected = 2015),
                 
                 HTML("<br><br><center>"), actionButton(inputId = "random", label = "Random selection"), HTML("</center><br>"),
                 
    ), 
    mainPanel(width = 9, 
              HTML("<center><h4><b>Regression results</b></h4></center>"),
              column(7, plotlyOutput("plot")),
              column(width = 5, HTML("<br><br><br><br><br>"), htmlOutput("reg_table")))
              
  )
)

server <- function(input, output, session) {
  reactive_plot <- reactive({
    ggplotly(
      ggplot(dep_data %>% filter(Year == input$year),
             aes(x = get(input$xvar), y = get(input$yvar))) +
        geom_point(alpha = .6,
                   aes(text = paste0(Department, "<br>",
                                     input$xvar, ": ", round(get(input$xvar), 2), "<br>",
                                     input$yvar, ": ", round(get(input$yvar), 2)))) + 
        geom_smooth(method = "lm", se = F) + xlab(input$xvar) + ylab(input$yvar),
      tooltip = "text"
    )
  })
  
  output$plot <- renderPlotly({reactive_plot()})
  
  reg_table <- reactive({
    stargazer(lm(formula(paste0(c(input$yvar, input$xvar), collapse = "~")), 
                 dep_data %>% filter(Year == input$year)), type  = "html",
              dep.var.labels = input$yvar, keep.stat = c("n", "rsq"))
  })
  
  output$reg_table <- renderPrint({reg_table()})
  
  observeEvent(input$random, {
    updateSelectInput(session, inputId = "xvar", label = "X variable:", 
                      choices = depvars, selected = sample(depvars, 1))
    updateSelectInput(session, inputId = "yvar", label = "Y variable:",
                      choices = depvars, selected = sample(depvars, 1))
    updateSliderTextInput(session, inputId = "year", label = "Select Year:",
                          choices = 2012:2017, selected = sample(2012:2017, 1))
  })
}

shinyApp(ui = ui, server = server)
