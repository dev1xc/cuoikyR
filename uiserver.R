body2 <- fluidRow(
      sidebarPanel(
        selectInput(
          inputId = "y",
          label = "Y-axis:",
          choices = c(
            "Hospital" = "hospital",
            "Age" = "age",
            "Age year" = "age_years",
            "Age catagory" = "age_cat",
            "Gender" = "gender",
            "Outcome" = "outcome",
            "Height cm " = "ht_cm",
            "Weight kg " = "wt_kg"
            
          ),
          selected = "hospital"
        ),
        
        selectInput(
          inputId = "x",
          label = "X-axis:",
          choices = c(
            "Hospital" = "hospital",
            "Age year" = "age_years",
            "Age" = "age",
            "Age catagory" = "age_cat",
            "Gender" = "gender",
            "Outcome" = "outcome",
            "Height cm " = "ht_cm",
            "Weight kg " = "wt_kg"
          ),
          selected = "age_cat"
        ),
        
        selectInput(
          inputId = "z",
          label = "Color by:",
          choices = c(
            
            "Age" = "age",
            "Hospital" = "hospital",
            "Age year" = "age_years",
            "Age catagory" = "age_cat",
            "Gender" = "gender",
            "Outcome" = "outcome",
            "Height cm " = "ht_cm",
            "Weight kg " = "wt_kg"
          ),
          selected = "gender"
        ),
        
        br(), br(),
        
        dateRangeInput(
          inputId = "date",
          label = "Select dates:",
          start = "2014-04-17", end = "2015-04-30",
          min = min_date, max = max_date,
          startview = "year"
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
    
)
body3<- 
  fluidRow(
  tabBox(
    
    tabPanel("LinearRegession", plotOutput(outputId = "scatterplot")),
    tabPanel("LinearRegession", plotOutput(outputId = "scatterplot1")),
    tabPanel("Test",plotOutput(outputId = "test")),
    tabPanel("Summary", tableOutput("summary")),
    tabPanel("Data", 
             box(DT::dataTableOutput(outputId = "moviestable", style = "width:400px ,height:500px; overflow-y: scroll;overflow-x: scroll"))),
  )
)
    
    








############################################
############################################
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content"),
            body2,
            body3
    ),
    
    tabItem(tabName = "linear",
            h2("Linear tab content",
               plotOutput("plot1")
            ),
   
    ),
    tabItem(tabName = "table",
            h2("Table tab content",
               
      ),
    fluidRow(
      box(
           title = "Tỉ lệ tuổi và giới tính",
        tableOutput("table1")
      ),
      box( 
           title = "Tỉ lệ bệnh viện và giới tính",
           tableOutput("table2")
      ),
      box( 
        title = "Tỉ lệ bệnh viện và giới tính",
        tableOutput("table3")
      ),
      box( 
        title = "Tỉ lệ xuất viện và độ tuổi",
        tableOutput("table4")
      ),
    )
    ),
    tabItem(tabName = "graph",
            h2("Graph tab content"),
    box( 
      title = "Tỉ lệ xuất viện và độ tuổi",
      plotOutput("graph1")
     ),    
    box( 
      title = "Tỉ lệ xuất viện và độ tuổi",
      plotOutput("graph2")
    ),     
      
    )
  )
)








ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Tuyến tính", tabName = "linear", icon = icon("th")),
      menuItem("Bảng", tabName = "table", icon = icon("th")),
      menuItem("Biểu đồ", tabName = "graph", icon = icon("th"))
    )
  ),
  body
 
  
)

server <- function(input, output) {
  
  ########################################################
  output$test <- renderPlot(
    ggplot(data = linelist, mapping = aes(x = age))+       # set data and axes
      geom_histogram(              # display histogram
        binwidth = 7,                # width of bins
        color = "red",               # bin line color
        fill = "blue",               # bin interior color
        alpha = 0.1)                 # bin transparency
  )
  
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
    req(input$date)
    movies_selected_date <- linelist %>%
      filter(date_hospitalisation >= as.POSIXct(input$date[1]) & date_hospitalisation <= as.POSIXct(input$date[2]))
    ggplot(data = movies_selected_date, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_histogram() +
      labs(title = new_plot_title())
  })
  
  
  
  
  
  ggplot(data = linelist, mapping = aes(x = age))+       # set data and axes
    geom_histogram(              # display histogram
      binwidth = 7,                # width of bins
      color = "red",               # bin line color
      fill = "blue",               # bin interior color
      alpha = 0.1)                 # bin transparency
  
  
  
  
  output$moviestable <- DT::renderDataTable({
    movies_sample <- linelist %>%
      sample_n(input$n) %>%
      select(case_id:age_cat)
    datatable(data = movies_sample,
              options = list(pageLength = 10,scrollX = TRUE,
                             scrollY = "500px"),
              rownames = FALSE)
  })
  #######################################################
  
  
  
  
  
  
  
  output$plot1 <- renderPlot(
    ggplot(points, aes(x = age)) + 
      ## add points for height 
      geom_point(aes(y = ht_cm)) + 
      ## add your regression line 
      geom_line(aes(y = .fitted), colour = "red")
  )
  
  #########################################################
  output$table1 <- renderTable(
    table1,
    
  )
  output$table2 <- renderTable(
    table2,
    
  )
  output$table3 <- renderTable(
    table3,
    
  )
  output$table4 <- renderTable(
    table4,
    
  )
  
  
  
  ###################################################################
  output$graph1 <- renderPlot(
    linelist %>%                      # begin with linelist
      count(age_cat, outcome1) %>%     # group and tabulate counts by two columns
      ggplot()+                       # pass new data frame to ggplot
      geom_col(                     # create bar plot
        mapping = aes(   
          x = outcome1,              # map outcome to x-axis
          fill = age_cat,           # map age_cat to the fill
          y = n))                   # map the counts column `n` to the height
    
  )
  output$graph2 <- renderPlot(
    linelist %>%                      # begin with linelist
      count(gender, outcome1) %>%     # group and tabulate counts by two columns
      ggplot()+                       # pass new data frame to ggplot
      geom_col(                     # create bar plot
        mapping = aes(   
          x = outcome1,              # map outcome to x-axis
          fill = gender,           # map age_cat to the fill
          y = n))               # map the counts column `n` to the height
    
  )
  
}


shinyApp(ui, server)

