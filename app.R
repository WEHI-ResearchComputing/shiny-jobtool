library(shiny)
library(RPostgreSQL)
library(shinyjs)
library(shinycssloaders)

source('utils.R')
source('jobPlots.R')

if ( !exists("con") ) {
  con <- dbConnect(dbDriver('PostgreSQL'),
                   dbname   = 'torquemonitor',
                   host     = 'mrm.wehi.edu.au',
                   port     = 5432,
                   user     = 'readaccess',
                   password = 'readaccess')
}

ui <- fluidPage(
 useShinyjs(),
 
 # This ridiculous hack pulls in the loader css class, provided by shinycssloaders,
 # so that I can get programmatic control to show the spinner.
 tags$head(
   tags$link(rel = "stylesheet", type = "text/css", href = "css-loaders/css/load1.css")
 ),  
 
  # Title
  titlePanel('Simple Job Query'),
  
  # Sidebare
  sidebarLayout(
    
    # Controls
    sidebarPanel(
      
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
                     label = 'Date range (inclusive):',
                     start = Sys.Date()-7,
                     end = Sys.Date()),
      
      fluidRow(
        # Create a layout with the action button and a loading spinner.
        # The spinner is hidden until needed.
        column(3,
               # Execute the query
               actionButton("go", "Go!")
              ),
        column(9,
               shinyjs::hidden(div(id = "loadingText", class = "load-container load1", div("Loading...", class = "loader", style = "font-size: 5px; margin: 4px; margin-left: 30px")))
               )
      ),
      
      # Output for the SQL for anyone interested
      h4('SQL query (FYI):'),
      verbatimTextOutput('query', placeholder = TRUE)
      ),
    
    mainPanel(
      # Output
      plotOutput(outputId = 'cpuPlot')
    )
  )
)

server <- function(input, output, session) {
  
  showLog()

  jobData <- NULL
  query   <- ""
  plotReady <- reactiveValues(ok = FALSE)
  
  observeEvent(input$go, {
    if ( isEmpty(query) ) {
      jobData <<- NULL
    } else {
      plotReady$ok <- FALSE
      shinyjs::disable("go")
      shinyjs::show("loadingText")
      jobData <<- dbGetQuery(con, query)
      plotReady$ok <- TRUE
    }
  })
  
  output$cpuPlot <- renderPlot({
    if ( plotReady$ok ) {
      shinyjs::enable("go")
      shinyjs::hide("loadingText")
      validate(need(length(jobData)>0, message = "Query returned no data"))
      makePlots(jobData)
    }
  })
  
  output$query <- renderText({
    users  <- input$userName
    jobs   <- input$jobId
    after  <- input$dateRange[1]
    before <- input$dateRange[2]
    
    query <<- createQuery(jobs, users, before, after)
    
    query
  })
  
    
}

shinyApp(ui = ui, server = server)
