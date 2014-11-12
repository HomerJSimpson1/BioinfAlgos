readfiletostring <- function(filename, usespace=FALSE) {
  ## Read the contents of a file into a single string.
  if(usespace)
    result <- paste(readLines(filename), collapse=" ")
  else
    result <- paste(readLines(filename), collapse="")

  return(result)
}
