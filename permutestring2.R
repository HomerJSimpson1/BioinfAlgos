permutestring2 <- function(pattern, hammingdist) {
  ## Find set of all permutations of pattern within Hamming distance specified by the
  ## hemmingdist input parameter.

  ## Assuming an alphabet here of {"A", "C", "G", "T"}
  alphabet = c('A', 'C', 'G', 'T')
  
  ## Need to revise in the future to do this more generally, rather than specifically
  ## for the various nucleotide values, so that it applies to a general alphabet.
  vect1A <- rep("A",64)
  vect1C <- rep("C",64)
  vect1G <- rep("G",64)
  vect1T <- rep("T",64)
  vect1 <-c(vect1A, vect1C, vect1G, vect1T)
  
  vect2A <- rep("A",16)
  vect2C <- rep("C",16)
  vect2G <- rep("G",16)
  vect2T <- rep("T",16)
  vect2 <-c(vect2A, vect2C, vect2G, vect2T)
  vect2 <- rep(vect2, 4)

  vect3A <- rep("A",4)
  vect3C <- rep("C",4)
  vect3G <- rep("G",4)
  vect3T <- rep("T",4)
  vect3 <-c(vect3A, vect3C, vect3G, vect3T)
  vect3 <- rep(vect3, 16)

  vect4base <- c("A", "C", "G", "T")
  vect4 <- rep(vect4base, 64)

  print(paste("Vector lengths: ", length(vect1), length(vect2), length(vect3), length(vect4), sep=" "))

  #df <- as.data.frame(cbind(vect1, vect2, vect3, vect4))
  df <- data.frame(cbind(vect1, vect2, vect3, vect4), stringsAsFactors = FALSE)

  df$strng <- mapply(function(w, x, y, z) {paste0(w, x, y, z, collapse="")}, df$vect1, df$vect2, df$vect3, df$vect4)
  
  ## Call Hamming Distance function to see how many of the possible permutations are within the
  ## specified Hamming Distance.
  df$hamming <- sapply(df$strng, function(x) {findhamming(x, pattern)})

  hamdist <- df[df$hamming <= 3, ]
  return(hamdist)
}
