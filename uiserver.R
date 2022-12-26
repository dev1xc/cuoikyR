



body1 <- fluidRow(
  valueBox(valuebox1, "Tổng bệnh nhân", icon = icon("credit-card"), color = "blue"),
  valueBox(valuebox2, "Chiều cao trung bình", icon = icon("credit-card"), color = "light-blue"),
  valueBox(valuebox3, "Cân nặng trung bình", icon = icon("credit-card"), color = "teal"),
  valueBox(valuebox4, "% bệnh nhân khỏi bệnh", icon = icon("credit-card"), color = "aqua"),
  valueBox(valuebox5, "Tuổi trung bình", icon = icon("credit-card"), color = "olive"),
)

body2 <- 
  fluidRow(
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
  tabBox(
    width = 8,

    tabPanel("Point PLot", plotOutput(outputId = "scatterplot")),
    tabPanel("Bar Plot", plotOutput(outputId = "scatterplot1")),
    tabPanel("Histogram Plot",plotOutput(outputId = "test")),
    tabPanel("Data", DT::dataTableOutput(outputId = "moviestable")),
  ),

)






############################################
############################################
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content"),
            body1,
            body2,
           
    ),
    
    tabItem(tabName = "linear",
            h2("Linear tab content",
               
            ),
            tabBox(
              width = "auto",
              tabPanel("Tuyến tính đơn biến", plotOutput(outputId = "plot1")),
              tabPanel("Tuyến tính đa biến", plotOutput(outputId = "plot2")),
            )
   
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
           title = "Tỉ lệ tuổi và xuất viện",
           tableOutput("table4")
      ),
      box( 
        title = "Tỉ lệ bệnh viện và giới tính",
        tableOutput("table3")
      ),
      box( 
        title = "Tỉ lệ xuất viện và độ tuổi",
        tableOutput("table2")
      ),
    )
    ),
    tabItem(tabName = "graph",
            h2("Graph tab content"),
    fluidRow(box( 
      title = "Tỉ lệ xuất viện và độ tuổi",
      plotOutput("graph1")
    ),    
    box( 
      title = "Tỉ lệ giới tính và xuất viện",
      plotOutput("graph2")
    ),    
    box( 
      title = "Tỉ lệ giữa bệnh viện và độ tuổi",
      plotOutput("graph3")
    ),    
    box( 
      title = "Tỉ lệ bệnh viện - giới tính - xuất viện",
      plotOutput("graph4")
    ),    )
      
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

server <- function(input, output,session) {

  ########################################################
  
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
              options = list(pageLength = 20, scrollX = TRUE,
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
  
  
  output$plot2 <- renderPlot(
    linear_da_bien 
  )

  #########################################################
  output$table1 <- renderTable(
    table1,

  )
  output$table2 <- renderTable(
    table2,

  )
  output$table3 <- renderTable(
    hospital_gender_outcome_ratio,
    options = list(pageLength = 20, scrollX = TRUE,
                   scrollY = "500px"),

  )
  output$table4 <- renderTable(
    table3,

  )



  ###################################################################
  output$graph1 <- renderPlot(
    graph1                 # map the counts column `n` to the height

  )
  output$graph2 <- renderPlot(
    graph2           # map the counts column `n` to the height

  )
  output$graph3 <- renderPlot(
    graph3           # map the counts column `n` to the height
    
  )
  output$graph4 <- renderPlot(
    graph4           # map the counts column `n` to the height
    
  )

}


shinyApp(ui, server)
  

