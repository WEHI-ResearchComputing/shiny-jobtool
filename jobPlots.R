
makePlots <- function(jobData, firstJob = 1, nJobs = 10) {
  
  jobf      <- unique(jobData$jobid)
  nJobs     <- min(nJobs, length(unique(jobf)))
  jobf      <- jobf[firstJob:(firstJob+nJobs-1)]
  exJobData <- jobData[jobData$jobid %in% jobf, ]
  
  start <- min(exJobData$timestamp)
  end   <- max(exJobData$timestamp)
  
  pcpu   <- vector("list", nJobs)
  tm     <- vector("list", nJobs)
  maxCpu <- 0
  
  for ( i in 1:nJobs ) {
    job <- jobf[i]
    
    # User time and system time for this job.
    cpu <- exJobData$utime[exJobData$jobid == job] + exJobData$stime[exJobData$jobid == job]
    
    # Time points for this job
    tm2 <- exJobData$timestamp[exJobData$jobid == job]
    
    # CPU time is cumulative so we need the difference
    dcpu <- diff(cpu)
    # The time interval between this measurement and the last
    dtm  <- as.numeric(diff(tm2), units = "secs")
    
    # Calculate as a percentage
    pcpu[[i]] <- dcpu / dtm * 100
    
    # Some event seem to be duplicated leading 0/0 values which now clean out.
    zIndx <- !is.nan(pcpu[[i]])
    pcpu[[i]] <- pcpu[[i]][zIndx]
    
    if ( length(pcpu[[i]]) == 0 ) {
      tm[[i]] = numeric(0)
    } else {
      # Grab the time values, less the duplicates
      tm[[i]]   <- tm2[2:length(tm2)][zIndx]
      
      # max CPU value to set the Y axis limits
      maxCpu <- max(maxCpu, max(pcpu[[i]]))
    }
    
  }

  p <- plot(
    range(start, end), 
    range(0, maxCpu), 
    type = 'n', 
    xlab = 'Time',
    ylab = 'CPU %',
    )

  cols <- rainbow(nJobs)
  
  for ( i in 1:nJobs ) {
    lines(tm[[i]], pcpu[[i]], col = cols[i])
  }
  
  legend("bottomleft", jobf, fill = cols)
  
  p
}