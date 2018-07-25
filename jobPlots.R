
makePlots <- function(jobData, firstJob = 1, nJobs = 10) {
  
  jobf  <- unique(jobData$jobid)[firstJob:(firstJob+nJobs-1)]
  nJobs <- length(unique(jobf))
  exJobData <- jobData[jobData$jobid %in% jobf, ]
  
  start  <- min(exJobData$timestamp)
  end    <- max(exJobData$timestamp)
  
  pcpu <- vector("list", nJobs)
  tm <- vector("list", nJobs)
  maxCpu <- 0
  for ( i in 1:nJobs ) {
    job <- jobf[i]
    cpu <- exJobData$utime[exjobData$jobid == job] + exJobData$stime[exjobData$jobid == job]
    tm2 <- exJobData$timestamp[exjobData$jobid == job]
    
    dcpu <- diff(cpu)
    dtm  <- as.numeric(diff(tm2), units = "secs")
    
    pcpu[[i]] <- dcpu / dtm * 100
    tm[[i]] <- tm2[2:length(tm2)]
    
    maxCpu <- max(maxCpu, max(pcpu[[i]]))
  }

  plot(
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
  
}