splitstring <- function(string, numchars) {
  ## Returns the string as a vector of substrings, all of length numchars.
  ## Klunky...
  starts <- seq(1,nchar(string), by=numchars)
  result <- sapply(starts, function(iii) { substr(string, iii, iii + (numchars - 1))})
  return(result)
}



splitstring2 <- function(string, numchars=3) {
  ## Same as splitstring(), but different methodology.
  #gregexpr and regmatches:

  ## Unfortunately, this first one drops any tail elements that form a group < numchars.
  ## i.e. if the string length is 21 and numchars = 6, then the last 3 characters are dropped.
  #result <- regmatches(string, gregexpr(paste(".{",numchars,"}",sep=""), string))[[1]] 
  ## OR can also use the below
  
  result <- strsplit(string, paste("(?<=.{",numchars,"})",sep=""), perl = TRUE)[[1]]
  return(result)
}
