
isEmpty <- function(s) is.na(s) || s == '' || is.null(s)

createQuery <- function(jobIdString, userString, before, after) {
  makeQueryString <- function(asString, var) {
    if ( isEmpty(asString) ) {
      asString
    } else {
      asList  <- strsplit(asString, "[, ]+")
      asList  <- mapply(function(j) { paste(var, "='", j, "'", sep = "") }, asList)
      paste(asList, collapse = " or ")
    }
  }
  
  jobQuery  <- makeQueryString(jobIdString, 'jobid')
  userQuery <- makeQueryString(userString, 'owner')

  baseQuery <- paste(
    "SELECT timestamp, jobid, owner, utime, stime FROM jobactivity WHERE ",
    "timestamp<='", before, "'",
    " AND timestamp>='", after, "'",
    sep = ""
  )
  
  if ( !isEmpty(jobQuery) ) {
    baseQuery <- paste(
      baseQuery,
      " AND ",
      "(", jobQuery, ")",
      sep = ""
      )
  }
  
  if ( !isEmpty(userQuery) ) {
    baseQuery <- paste(
      baseQuery,
      " AND ",
      "(", userQuery, ")",
      sep = ""
    )
  }
    
  paste(baseQuery, ' ORDER BY timestamp;', sep = "")
}