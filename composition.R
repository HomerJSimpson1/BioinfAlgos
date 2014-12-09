composition <- function(k, text) {
  ## Split the given text into all possible strings of length k
  ## and return the fragments in lexicographic order.
  textlen <- nchar(text)
  fragments <- character()
  
  for (i in 1:(textlen - k + 1)) {
    nextstr <- substr(text, i, i+k-1)
    fragments <- append(fragments, nextstr)
  }
  
  return(sort(fragments))
}
