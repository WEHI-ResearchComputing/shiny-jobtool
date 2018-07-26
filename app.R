library(shiny)
library(RPostgreSQL)

source('utils.R')
source('jobPlots.R')

drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, 
                 dbname = 'torquemonitor',
                 host = 'mrm.wehi.edu.au', 
                 port = 5432,
                 user = 'readaccess', 
                 password = 'readaccess')

ui <- fluidPage(
  
  # Title
  titlePanel('Simple Job Analysis'),
  
  # Sidebare
  sidebarLayout(
    
    # Controls
    sidebarPanel(
      
      # Only call the plot when the user hits enter.
      # Otherwise a heavy, pointless DB query is executed for every key stroke
      tags$script(keyPressHandler),
      
      # Search on job id
      textInput(inputId = 'jobId',
                label = 'One or more job ids:',
                value = ''),
      
      # Search on user name
      textInput(inputId = 'userName',
                label = 'One or more user names:',
                value = ''),
      
      # Search on date range
      dateRangeInput('dateRange',
                     label = 'Date range (inclusive)',
                     start = Sys.Date()-7,
                     end = Sys.Date())
      ),
    
    mainPanel(
      # Output
      plotOutput(outputId = 'cpuPlot')
    )
  )
)

server <- function(input, output) {
  
  output$cpuPlot <- renderPlot({
    
    users  <- input$userName
    jobs   <- input$jobId
    after  <- input$dateRange[1]
    before <- input$dateRange[2]
    
    query  <- createQuery(jobs, users, before, after)
    print(query)
    jobData <- dbGetQuery(con, query)

    if ( length(jobData) > 0 ) {
      print(jobData)
      makePlots(jobData)
    }    
  })
  
}

shinyApp(ui = ui, server = server)
