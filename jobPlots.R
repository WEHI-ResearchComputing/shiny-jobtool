
processJobData <- function(rawJobData) {
  jobf      <- unique(rawJobData$jobid)
  nJobs     <- length(unique(jobf))
  
  jobData  <- list()
  jobNames <- list()
  indx <- 1
  for ( i in 1:nJobs ) {
    job <- jobf[i]
    
    # User time and system time for this job.
    cpu <- rawJobData$utime[rawJobData$jobid == job] + rawJobData$stime[rawJobData$jobid == job]
    
    # Time points for this job
    tm2 <- rawJobData$timestamp[rawJobData$jobid == job]
    
    # CPU time is cumulative so we need the difference
    dcpu <- diff(cpu)
    # The time interval between this measurement and the last
    dtm  <- as.numeric(diff(tm2), units = "secs")
    
    # Calculate as a percentage
    pcpu <- dcpu / dtm * 100
    
    # 1. Some event seem to be duplicated leading 0/0 values.
    # 2. If there is a pid change pcpu may be -ve
    zIndx <- (!is.nan(pcpu) & pcpu>=0)
    pcpu <- pcpu[zIndx]
    
    if ( length(pcpu) != 0 ) {
      # Grab the time values, less the duplicates
      tm <- tm2[2:length(tm2)][zIndx]

      jobData[[indx]]  <- data.frame(tm <- tm, pcpu <- pcpu)
      jobNames[indx] <- strsplit(job, '\\.')[[1]][1]
      indx <- indx + 1
    }
    
  }
  
  list(names = jobNames, data = jobData)
}

makePlots <- function(jobData, start, end, firstJob = 1, nJobs = 10) {

  p <- plot_ly(mode = 'line', type = 'scatter')
  for ( i in 1:nJobs ) {
    jindx <- firstJob - 1 + i
    jd <- jobData$data[[jindx]]
    y <- jd$pcpu
    x <- jd$tm
    n <- jobData$name[[jindx]]
    p <- add_trace(p, x = x, y = y, name = n, mode = 'line', type = 'scatter')
  }
  
  layout(p, xaxis = list(title = 'Date'), yaxis = list(title = '% CPU'))
}

