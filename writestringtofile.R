writestringtofile <- function(filename, string) {
  ## Write a string to a file given by filename
  print(filename)
  if (is.vector(string))
    {
      string <- paste(string, "", sep=" ", collapse="")
      print(string)
    }
  fileConn<-file(filename)
  writeLines(c(string), fileConn)
  close(fileConn)

}
