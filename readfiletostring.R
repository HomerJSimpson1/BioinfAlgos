readfiletostring <- function(filename, usespace=FALSE, nocollapse=FALSE) {
  ## Read the contents of a file into a single string.
  if(usespace)
    if(nocollapse)
      result <- paste(readLines(filename))
    else
      result <- paste(readLines(filename), collapse=" ")
  else
    if(nocollapse)
      result <- paste(readLines(filename))
    else
      result <- paste(readLines(filename), collapse="")

  return(result)
}




readfiletovec <- function(filename, header=FALSE, stringsAsFactors=FALSE) {
  ## Read the contents of a file into a vector
  ## Each line in the file corresponds to a single element of the vector.
  vec <- read.table(filename, header=header, stringsAsFactors=stringsAsFactors)
  return(vec[,1])
}
