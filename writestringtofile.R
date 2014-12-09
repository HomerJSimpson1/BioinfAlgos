writestringtofile <- function(filename, string) {
  ## Write a string to a file given by filename
  #print(filename)
  if (is.vector(string))
    {
      string <- paste(string, "", sep=" ", collapse="")
      #print(string)
    }
  fileConn<-file(filename)
  writeLines(c(string), fileConn)
  close(fileConn)

}






writestringtofile1 <- function(filename, string) {
  ## Write a string to a file given by filename
  ## Add newline characters after each string
  #print(filename)
  if (is.vector(string))
    {
      string <- paste(string, "\n", sep=" ", collapse="")
      #print(string)
    }
  fileConn<-file(filename)
  writeLines(c(string), fileConn)
  close(fileConn)

}


writestringtofile2 <- function(filename, string) {
  ## Write a string to a file given by filename
  ## Add newline characters after each string
  #print(filename)
  if (is.vector(string))
    {
      string <- paste(string, "\n", sep="", collapse="")
      #print(string)
    }
  fileConn<-file(filename)
  writeLines(c(string), fileConn)
  close(fileConn)

}
