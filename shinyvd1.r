# Load packages ----------------------------------------------------------------

  library(shiny)
  library(ggplot2)
  library(tools)
  library(dplyr)
  library(tidyverse)
  library(magrittr)
  library(ggplot2)
  library(shinydashboard)
  library(dplyr)    # data.frames
  library(sf)       # Spatial
  # Interactive Data Viz
  library(leaflet)  # Maps
  library(dygraphs) # Charts
  library(DT)       # tables
  library(rvest)    # webscraping
  library(dqshiny)    # auto complete
  library(shiny)       # Starting Reactivity
  library(shinythemes) # themes
  library(plotly)
  library(DT)

# Load data --------------------------------------------------------------------


linelist <- read_rds("./linelist_cleaned.rds")
n_total <- nrow(linelist)
min_date <- min(linelist$date_hospitalisation)
head(max_date)
max_date <- max(linelist$date_hospitalisation)
linelist <- na.omit(linelist)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c(

          
          "Age" = "age",
          "Height cm " = "ht_cm",
          "Weight kg " = "wt_kg"

        ),
        selected = "age"
      ),
      
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c(


          "Height cm " = "ht_cm",
          "Weight kg " = "wt_kg"

        ),
        selected = "ht_cm"
      ),
      
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c(
          
          "Age" = "age",
          "Age catagory" = "age_cat",
          "Gender" = "gender",
          "Height cm " = "ht_cm",
          "Weight kg " = "wt_kg"
        ),
        selected = "wt_kg"
      ),
      
      dateRangeInput(
        inputId = "date",
        label = "Select dates:",
        start = "2014-04-17", end = "2015-04-30",
        min = min_date, max = max_date,
        startview = "year"
      ),
      br(), br(),
      
      selectInput(
        inputId = "b",
        label = "Histogram",
        choices = c(
          "Age" = "age",
          "Height cm " = "ht_cm",
          "Weight kg " = "wt_kg",
          "CT Blood " = "ct_blood"

        ),
        selected = "age"
      ),
      


      selectInput(
        inputId = "f",
        label = "Bar Plot",
        choices = c(
          "Gender" = "gender",
          "Out come" = "outcome",
          "Hospital" = "hospital"
       


        ),
        selected = "gender"
      ),
      
      
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 0.5
      ),
      
      sliderInput(
        inputId = "size",
        label = "Size:",
        min = 0, max = 5,
        value = 2
      ),
      
      textInput(
        inputId = "plot_title",
        label = "Plot title",
        placeholder = "Enter text to be used as plot title"
      ),
      
      actionButton(
        inputId = "update_plot_title",
        label = "Update plot title"
      ),
      
      br(), br(),
      HTML(paste("Enter a value between 1 and", n_total)),
      
      numericInput(
        inputId = "n",
        label = "Sample size:",
        value = 30,
        min = 1, max = n_total,
        step = 1
      )
    ),
    
    
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Point PLot", plotOutput(outputId = "scatterplot")),
        tabPanel("Bar Plot", plotOutput(outputId = "scatterplot1")),
        tabPanel("Histogram Plot",plotOutput(outputId = "test")),
        tabPanel("Data", DT::dataTableOutput(outputId = "moviestable")),
 
      )
    )
    
    
  )
)

# Define server ----------------------------------------------------------------

server <- function(input,output, session) {
  
  output$test <- renderPlot({
    req(input$date)
    movies_selected_date <- linelist %>%
    filter(date_hospitalisation >= as.POSIXct(input$date[1]) & date_hospitalisation <= as.POSIXct(input$date[2]))
    ggplot(data = linelist, aes_string(x = input$b))+       # set data and axes
      geom_histogram(              # display histogram
        binwidth = 7,                # width of bins
        color = "red",               # bin line color
        fill = "blue",               # bin interior color
        alpha = 0.1)                 # bin transparency
  })
  
  new_plot_title <- eventReactive(
    eventExpr = input$update_plot_title,
    valueExpr = {
      toTitleCase(input$plot_title)
    }
  )
  
  output$scatterplot <- renderPlot({
    req(input$date)
    movies_selected_date <- linelist %>%
      filter(date_hospitalisation >= as.POSIXct(input$date[1]) & date_hospitalisation <= as.POSIXct(input$date[2]))
    ggplot(data = movies_selected_date, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(title = new_plot_title())
  })
  output$scatterplot1 <- renderPlot({
    
    ggplot(data = linelist, mapping = aes(x = hospital))+     
      geom_bar(aes_string(fill = input$f), color = "yellow")+         
      labs(title = "")
     
  })

  
  
  
  
  
  
  
  output$moviestable <- DT::renderDataTable({
    movies_sample <- linelist %>%
      sample_n(input$n) %>%
      select(case_id:age_cat)
    datatable(data = movies_sample,
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)

