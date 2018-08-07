library(shiny)
library(RPostgreSQL)
library(shinyjs)
library(shinycssloaders)
library(plotly)

source('utils.R')
source('jobPlots.R')
source('pagerui.R')

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
 
 # This ridiculous hack pulls in the spinner css class, provided by shinycssloaders,
 # so that I can get programmatic control over the spinner.
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
        column(3,
               # Execute the query
               actionButton("go", "Go!")
              ),
        column(9,
               # The spinner is hidden until needed.
               shinyjs::hidden(div(id = "loadingSpinner", class = "load-container load1", div("Loading...", class = "loader", style = "font-size: 5px; margin: 3px; margin-left: 30px")))
               )
      ),
      
      pageruiInput('jobPager'),
      
      # Output for the SQL for anyone interested
      h4('SQL query (FYI):'),
      verbatimTextOutput('query', placeholder = TRUE)
      ),
    
    mainPanel(
      # Output
      plotlyOutput('cpuPlot')
    )
  )
)

server <- function(input, output, session) {
  
  showLog()

  jobData <- NULL
  query   <- ""
  before <- NULL
  after  <- NULL
  plotReady <- reactiveValues(ok = FALSE)
  firstJob <- 1
  showJobs <- 10
  totalJobs <- 0
  
  observeEvent(input$go, {
    if ( isEmpty(query) ) {
      jobData <<- NULL
    } else {
      shinyjs::disable("go")
      shinyjs::show("loadingSpinner")
      
      # Load from the database and preprocess
      jobData <<- processJobData(dbGetQuery(con, query))
      totalJobs <<- length(pJobData[[1]])
      totalPages <- if ( totalJobs %% 10 == 0 ) {
        totalJobs / 10
      } else {
        totalJobs / 10 + 1
      }
      
      # This generates a pager event, so let the pager trigger the update
      updatePageruiInput(session, 'jobPager', page_current = 1, pages_total = totalPages)
    }
  })
  
  observeEvent(input$jobPager, {
    plotReady$ok <- FALSE
    page <- input$jobPager$page_current
    firstJob <<- (page-1)*10 + 1
    showJobs <<- min(10, totalJobs - firstJob + 1)
    plotReady$ok <- TRUE
  })
  
  output$cpuPlot <- renderPlotly({
    if ( plotReady$ok ) {
      validate(need(length(jobData)>0, message = "Query returned no data"))
      p <- makePlots(jobData, after, before, firstJob = firstJob, nJobs = showJobs)
      shinyjs::enable("go")
      shinyjs::hide("loadingSpinner")
      p
    }
  })
  
  output$query <- renderText({
    users  <- input$userName
    jobs   <- input$jobId
    after  <<- input$dateRange[1]
    before <<- input$dateRange[2]
    
    query <<- createQuery(jobs, users, before, after)
    
    query
  })
  
    
}

shinyApp(ui = ui, server = server)
