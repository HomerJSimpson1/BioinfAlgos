numsubpeptides <- function(peptidelength) {
  count <- 0
  for (i in 0:(peptidelength - 1)) {
    count <- 2*i + count
  }
  return(count)
}





## ## Recursive version works, but only for smaller numbers.  :-(
## calcnumsubpeptides <- function(peptidelength, iter, count) {
##   if(peptidelength <= 1)
##     return(count)
##   else
##     {
##       count <- count + 2 * iter
##       iter <- iter + 1
##       peptidelength <- peptidelength - 1
##       return(calcnumsubpeptides(peptidelength, iter, count))
##     }
## }



## numsubpeptides <- function(peptidelength) {
##   return(calcnumsubpeptides(peptidelength, 1, 0))
## }
