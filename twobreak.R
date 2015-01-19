twobreak <- function(strP, strQ) {
  ## Input:  Genomes P and Q
  ## Output: The 2-break distance d(P, Q)

  Pcycles <- countcycles(strP)
  Qcycles <- countcycles(strQ)
  cyclesPQ <- Pcycles + Qcycles

  print(cyclesPQ)

  P <- as.integer(strsplit(strP, "[[:punct:]]")[[1]])
  #print(P)
  Q <- as.integer(strsplit(strQ, "[[:punct:]]")[[1]])
  numblocksP <- max(P, na.rm=TRUE)
  print(numblocksP)
  numblocksQ <- max(Q, na.rm=TRUE)
  print(numblocksQ)
  distPQ <- numblocksP - cyclesPQ

  return(distPQ)
}



countcycles <- function(Pstring) {



}



## ## The following is definitely incorrect.
## countcycles <- function(Pstring) {
##   #numcycles <- 0

##   P <- strsplit(Pstring, "[(]")[[1]]
##   #print(P)
##   #print(length(P))
##   numcycles <- length(P) - 1
  
##   ##for (i in 1:nchar(P)) {
##    ## print(P[i])
##     ## if (P[i] == "(") {
## ##       numcycles <- numcycles + 1
## ##     }
    
##   ##}

##   return(numcycles)
## }





## readgenomefromfile <- function(fname="") {
##   conn <- file(fname, "r")
##   linecount <- 0
  
##   line <- readLines(conn, 1)
##   linecount <- linecount + 1
##   while(line > 0) {
##     ##if (line[nchar(line)] == '\n')
##     ##  print(line)

##     if (linecount < 20) {
##       print(line)
##       print(line[nchar(line)])
##     }
    
##     line <- readLines(conn, 1)
##     linecount <- linecount + 1
##   }
## }
