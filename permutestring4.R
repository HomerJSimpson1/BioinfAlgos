permutestring4 <- function(df, pattern, hammingdist) {
  ## Find set of all permutations of pattern within Hamming distance specified by the
  ## hemmingdist input parameter.  df is an input parameter that contains all permutations of length k.

  ## Generate all possible permutations of length k
  #df <- genperms(nchar(pattern))
  
  ## Call Hamming Distance function to see how many of the possible permutations are within the
  ## specified Hamming Distance.
  df$hamming <- sapply(df$strng, function(x) {findhamming(x, pattern)})

  hamdist <- df[df$hamming <= hammingdist, ]
  #hamdist <- df[df$hamming <= 3, ]  
  return(hamdist)
}



foo <- function(dfrow) {
  ## Paste together the column values in a row of a data frame.
  #stringres <- sapply(1:ncol(dfrow), paste0(dfrow, collapse=""))
  stringres <- paste0(dfrow, collapse="")
  return(stringres)
}

genperms <- function(k) {
  ## Generate all permutations of length k for a given alphabet.

  ## Assuming an alphabet here of {"A", "C", "G", "T"}
  alphabet = c('A', 'C', 'G', 'T')

  #patLength <- nchar(pattern)
  vects <- vector('character')

  for (i in 1:k) {
    vectA <- rep("A", 4^(k - i))
    vectC <- rep("C", 4^(k - i))
    vectG <- rep("G", 4^(k - i))
    vectT <- rep("T", 4^(k - i))
    vect <- c(vectA, vectC, vectG, vectT)
    vects <-cbind(vects, vect)
  }

  ## for (i in 1:patLength) {
  ##   vectA <- rep("A", 4^(patLength - i))
  ##   vectC <- rep("C", 4^(patLength - i))
  ##   vectG <- rep("G", 4^(patLength - i))
  ##   vectT <- rep("T", 4^(patLength - i))
  ##   vect <- c(vectA, vectC, vectG, vectT)
  ##   vects <-cbind(vects, vect)
  ## }  

  df <- data.frame(vects, stringsAsFactors = FALSE)
  df$strng <- apply(df, 1, function(x) {foo(x)})
  
  return(df)
}






## permutestring4 <- function(pattern, hammingdist) {
##   ## Find set of all permutations of pattern within Hamming distance specified by the
##   ## hemmingdist input parameter.

##   ## Generate all possible permutations of length k
##   df <- genperms(nchar(pattern))
  
##   ## Call Hamming Distance function to see how many of the possible permutations are within the
##   ## specified Hamming Distance.
##   df$hamming <- sapply(df$strng, function(x) {findhamming(x, pattern)})

##   hamdist <- df[df$hamming <= hammingdist, ]
##   #hamdist <- df[df$hamming <= 3, ]  
##   return(hamdist)
## }



## permutestring4 <- function(pattern, hammingdist) {
##   ## Find set of all permutations of pattern within Hamming distance specified by the
##   ## hemmingdist input parameter.

##   ## Assuming an alphabet here of {"A", "C", "G", "T"}
##   alphabet = c('A', 'C', 'G', 'T')

##   patLength <- nchar(pattern)
##   vects <- vector('character')

##   for (i in 1:patLength) {
##     vectA <- rep("A", 4^(patLength - i))
##     vectC <- rep("C", 4^(patLength - i))
##     vectG <- rep("G", 4^(patLength - i))
##     vectT <- rep("T", 4^(patLength - i))
##     vect <- c(vectA, vectC, vectG, vectT)
##     vects <-cbind(vects, vect)
##   }
    
  
## ##   ## Need to revise in the future to do this more generally, rather than specifically
## ##   ## for the various nucleotide values, so that it applies to a general alphabet.
## ##   vect1A <- rep("A",64)
## ##   vect1C <- rep("C",64)
## ##   vect1G <- rep("G",64)
## ##   vect1T <- rep("T",64)
## ##   vect1 <-c(vect1A, vect1C, vect1G, vect1T)
  
## ##   vect2A <- rep("A",16)
## ##   vect2C <- rep("C",16)
## ##   vect2G <- rep("G",16)
## ##   vect2T <- rep("T",16)
## ##   vect2 <-c(vect2A, vect2C, vect2G, vect2T)
## ##   vect2 <- rep(vect2, 4)

## ##   vect3A <- rep("A",4)
## ##   vect3C <- rep("C",4)
## ##   vect3G <- rep("G",4)
## ##   vect3T <- rep("T",4)
## ##   vect3 <-c(vect3A, vect3C, vect3G, vect3T)
## ##   vect3 <- rep(vect3, 16)

## ##   vect4base <- c("A", "C", "G", "T")
## ##   vect4 <- rep(vect4base, 64)

##   #print(paste("Vector lengths: ", length(vect1), length(vect2), length(vect3), length(vect4), sep=" "))

##   #df <- as.data.frame(cbind(vect1, vect2, vect3, vect4))
##   #df <- data.frame(cbind(vect1, vect2, vect3, vect4), stringsAsFactors = FALSE)
##   df <- data.frame(vects, stringsAsFactors = FALSE)
##   #print(str(df))

##   #df$strng <- apply(df, 1, paste0(df, collapse=""))
##   #df$strng <- apply(df, 1, foo(df))
##   #df$strng <- sapply(1:nrow(df), foo(df))
##   #df$strng <- sapply(df, function(x) {foo(x)})
##   df$strng <- apply(df, 1, function(x) {foo(x)})    
##   #print(head(df$strng))

##   #df$strng <- mapply(function(w, x, y, z) {paste0(w, x, y, z, collapse="")}, df$vect1, df$vect2, df$vect3, df$vect4)
  
##   ## Call Hamming Distance function to see how many of the possible permutations are within the
##   ## specified Hamming Distance.
##   df$hamming <- sapply(df$strng, function(x) {findhamming(x, pattern)})

##   hamdist <- df[df$hamming <= hammingdist, ]
##   #hamdist <- df[df$hamming <= 3, ]  
##   return(hamdist)
## }
