
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
    cpu <- exJobData$utime[exJobData$jobid == job] + exJobData$stime[exJobData$jobid == job]
    tm2 <- exJobData$timestamp[exJobData$jobid == job]
    
    dcpu <- diff(cpu)
    dtm  <- as.numeric(diff(tm2), units = "secs")
    
    pcpu[[i]] <- dcpu / dtm * 100
    if ( length(pcpu[[i]]) == 0 ) {
      tm[[i]] = numeric(0)
    } else {
      tm[[i]]   <- tm2[2:length(tm2)]
    }
    
    maxCpu <- max(maxCpu, max(pcpu[[i]]))
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
  
  legend("bottomright")
  p
}