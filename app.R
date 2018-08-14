library(shiny)
library(RPostgreSQL)
library(shinyjs)
library(shinycssloaders)
library(plotly)

source('utils.R')
source('jobPlots.R')
source('pagerui.R')

# The database connection
dbCnx <- NULL
print('session start')
if ( is.null(dbCnx) ) {
  print('Create DB connection')
  # Create the database connection, try to reuse if still in scope
  dbCnx <- dbConnect(dbDriver('PostgreSQL'),
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

 # This bit brings in some js that allows the containing iframe to resize with the app.
 tags$head(
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                  type="text/javascript")
      ),
 
 # Title
 titlePanel('Simple Job Analysis'),
 
 fluidRow(
   # This row provides the input panel. Although I've used the sidebarPanel, 
   # it actually sits on top of the output plots
   column(12,
          # The width is specified to stop the panel resizing in a jarring manner. 
          # 1600px is based on trial and error, not common sense.
          div(style="width:1600px;", 
            sidebarPanel(
              
              # Search on job ids
              textInput(inputId = 'jobId',
                        label = 'One or more job ids:',
                        value = ''),
              
              # Search on user names
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
                       shinyjs::hidden(div(id = "loadingSpinner", 
                                           class = "load-container load1", 
                                           div("Loading...", 
                                               class = "loader", 
                                               style = "font-size: 5px; margin: 3px; margin-left: 30px")
                                           )
                                       )
                )
              ),
              
              # The pager control for stepping through lots of jobs
              pageruiInput('jobPager'),
              
              # Output for the SQL for anyone interested
              h4('SQL query (FYI):'),
              verbatimTextOutput('query', placeholder = TRUE)
            )
          )
        )
   ),
 
 # Plots here. The entire div is hidden until populated.
 # Warning: shinyjs::hidden seems to suppress from these elements
 #          makes the event handling unnecassarily complicated
 fluidRow(column(12,
                 shinyjs::hidden(div(id = "plots",
                   # Output
                   plotlyOutput('cpuPlot', width = "800px"),
                   hr(),
                   plotlyOutput('rssPlot', width = "800px"),
                   hr(),
                   plotlyOutput('virtPlot', width = "800px")
                 ))
              )
          )
)

server <- function(input, output, session) {
  
  showLog()

  # Initialise globals
  jobData   <- NULL
  query     <- ""
  before    <- NULL
  after     <- NULL
  plotReady <- reactiveValues(ok = FALSE)
  firstJob  <- 1
  showJobs  <- 10
  totalJobs <- 0
  
  showSpinner <- function() {
    shinyjs::disable("go")
    shinyjs::show("loadingSpinner")
  }
  
  hideSpinner <- function() {
    shinyjs::enable("go")
    shinyjs::hide("loadingSpinner")
  }
  
  hidePlots <- function() {
    shinyjs::hide("plots")
  }
  
  showPlots <- function() {
    shinyjs::show("plots")
  }
  
  observeEvent(input$go, {
    # User has pressed the Go! button
    
    if ( isEmpty(query) ) {
      jobData <<- NULL
    } else {
      plotReady$ok <- FALSE
      showSpinner()
      
      # Load from the database and preprocess
      jobData <<- processJobData(dbGetQuery(dbCnx, query))
      
      if ( is.null(jobData) ) {
        totalJobs <<- 0
        totalPages <- 0
        # Generate a plot ready event, even though there are no data so force the UI to update
        hidePlots()
        plotReady$ok <- TRUE
      } else {
        totalJobs <<- length(jobData[[1]])
        totalPages <- if ( totalJobs %% 10 == 0 ) {
          totalJobs / 10
        } else {
          totalJobs / 10 + 1
        }
        # This generates a pager event, so let the pager trigger the update
        updatePageruiInput(session, 'jobPager', page_current = 1, pages_total = totalPages)
      }
      
    }
    
  })
  
  observeEvent(plotReady$ok, {
    if ( plotReady$ok ) {
      showPlots()
      hideSpinner()
    }
  })
  
  observeEvent(input$jobPager, {
    # The pager generates a spurious event on startup so ignore if there is no query
    if ( !isEmpty(query) ) {
      plotReady$ok <- FALSE
      page <- input$jobPager$page_current
      firstJob <<- (page-1)*10 + 1
      showJobs <<- min(10, totalJobs - firstJob + 1)
      plotReady$ok <- TRUE
    }
  })
  
  output$cpuPlot <- renderPlotly({
    if ( plotReady$ok ) {
      validate(need(totalJobs>0, message = "Query returned no data"))
      makeCpuPlots(jobData, after, before, firstJob = firstJob, nJobs = showJobs)
    }
  })
  
  output$rssPlot <- renderPlotly({
    if ( plotReady$ok ) {
      validate(need(totalJobs>0, message = ""))
      makeRssPlots(jobData, after, before, firstJob = firstJob, nJobs = showJobs)
    }
  })
  
  output$virtPlot <- renderPlotly({
    if ( plotReady$ok ) {
      validate(need(totalJobs>0, message = ""))
      makeVirtPlots(jobData, after, before, firstJob = firstJob, nJobs = showJobs)
    }
  })
  
  output$query <- renderText({
    users  <-  input$userName
    jobs   <-  input$jobId
    after  <<- input$dateRange[1]
    before <<- input$dateRange[2]
    
    query <<- createQuery(jobs, users, before, after)
    
    query
  })
}


shinyApp(
  ui = ui, 
  server = server,
  onStart = function() {
    # Close the connection at the end of the session
    onStop(function() {
      print('session ended - closing db connection')
      if ( !is.null(dbCnx) ) {
        dbDisconnect(dbCnx)
        dbCnx <- NULL
      }
      })
    }
  )
