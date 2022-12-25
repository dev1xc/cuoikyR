body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
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
  output$plot1 <- renderPlot(
    ggplot(points, aes(x = age)) + 
      ## add points for height 
      geom_point(aes(y = ht_cm)) + 
      ## add your regression line 
      geom_line(aes(y = .fitted), colour = "red")
  )
  
  
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

}


shinyApp(ui, server)

