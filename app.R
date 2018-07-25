library(shiny)
library(RPostgreSQL)

source("utils")
source("jobPlots")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, 
                 dbname = "torquemonitor",
                 host = "mrm.wehi.edu.au", 
                 port = 5432,
                 user = "readaccess", 
                 password = "readaccess")

ui <- fluidPage(
  
  # Title
  titlePanel("Simple Job Analysis"),
  
  # Sidebare
  sidebarLayout(
    
    # Controls
    sidebarPanel(
      
      # Search on job id
      textInput(inputId = "jobId",
                label = "List of job ids:",
                value = ""),
      
      # Search on user name
      textInput(inputId = "userName",
                label = "userid:",
                value = ""),
      
      # Search on date range
      dateRangeInput('dateRange',
                     label = 'Date range',
                     start = Sys.Date()-7,
                     end = Sys.Date())
      ),
    
    mainPanel(
      # Output
      plotOutput(outputId = "cpuPlot")
    )
  )
)

server <- function(input, output) {
  
  output$cpuPlot <- renderPlot({
    users   <- input$userName
    jobs    <- input$jobId
    before  <- input$dateRange$end 
    after   <- input$dateRange$start
    query   <- createQuery(jobs, users, before, after)
    jobData <- dbGetQuery(con, query)
    
    makePlot(jobData)
  })
  
}

shinyApp(ui = ui, server = server)
