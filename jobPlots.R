
processJobData <- function(rawJobData) {
  jobf      <- unique(rawJobData$jobid)
  nJobs     <- length(unique(jobf))
  
  if ( nJobs == 0 ) {
    return(NULL)
  }
  
  cpuData  <- list()
  memData  <- list()
  jobNames <- list()
  cpuIndx <- 1
  memIndx <- 1
  
  for ( i in 1:nJobs ) {
    job <- jobf[i]
    
    cd <- extractCpuData(rawJobData, job)
    if ( !is.null(cd) ) {
      cpuData[[cpuIndx]]  <- cd
      jobNames[cpuIndx] <- strsplit(job, '\\.')[[1]][1]
      cpuIndx <- cpuIndx + 1
    }
    
    md <- extractMemData(rawJobData, job)
    if ( !is.null(md) ) {
      memData[[memIndx]] <- md
      memIndx <- memIndx + 1
    }
    
  }
  
  list(names = jobNames, cpuData = cpuData, memData = memData)
}

extractMemData <- function(rawJobData, job) {
  # Convert memory from bytes to GiB
  rss   <- rawJobData$rss[rawJobData$jobid == job] / 2^30
  vsize <- rawJobData$vsize[rawJobData$jobid == job] / 2^30
  tm    <- rawJobData$timestamp[rawJobData$jobid == job]
  data.frame(tm = tm, rss = rss, vsize = vsize)
}
  
extractCpuData <- function(rawJobData, job) {
    
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

  if ( length(pcpu) == 0 ) {
    NULL
  } else {
    # Grab the time values, less the duplicates
    tm <- tm2[2:length(tm2)][zIndx]
    data.frame(tm <- tm, pcpu <- pcpu)
  }
}

makeCpuPlots <- function(jobData, start, end, firstJob = 1, nJobs = 10) {
  
  p <- plot_ly(mode = 'line', type = 'scatter')
  for ( i in 1:nJobs ) {
    jindx <- firstJob - 1 + i
    jd <- jobData$cpuData[[jindx]]
    y <- jd$pcpu
    x <- jd$tm
    n <- jobData$name[[jindx]]
    p <- add_trace(p, x = x, y = y, name = n, mode = 'line', type = 'scatter')
  }
  
  layout(p, xaxis = list(title = 'Date'), yaxis = list(title = '% CPU'), title = 'CPU use')
}

makeRssPlots <- function(jobData, start, end, firstJob = 1, nJobs = 10) {
  
  p <- plot_ly(mode = 'line', type = 'scatter')
  for ( i in 1:nJobs ) {
    jindx <- firstJob - 1 + i
    md <- jobData$memData[[jindx]]
    y <- md$rss
    x <- md$tm
    n <- jobData$name[[jindx]]
    p <- add_trace(p, x = x, y = y, name = n, mode = 'line', type = 'scatter')
  }
  
  layout(p, xaxis = list(title = 'Date'), yaxis = list(title = 'Physical memory (GiB)'), title = 'Real Memory')
}

makeVirtPlots <- function(jobData, start, end, firstJob = 1, nJobs = 10) {
  
  p <- plot_ly(mode = 'line', type = 'scatter')
  for ( i in 1:nJobs ) {
    jindx <- firstJob - 1 + i
    md <- jobData$memData[[jindx]]
    y <- md$vsize
    x <- md$tm
    n <- jobData$name[[jindx]]
    p <- add_trace(p, x = x, y = y, name = n, mode = 'line', type = 'scatter')
  }
  
  layout(p, xaxis = list(title = 'Date'), yaxis = list(title = 'Virtual memory (GiB)'), title = 'Virtual Memory')
}

