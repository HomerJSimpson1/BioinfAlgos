runlinearspectrum <- function(peptide) {
  imt <- integermasstable()
  #print(head(imt))
  peptide <- strsplit(peptide,"")[[1]]
  linearspec <- linearspectrum(peptide, imt$proteins, imt$intmass)
  
  return(linearspec)
}




linearspectrum <- function(peptide, aminoacid, aminoacidmass) {
  ## peptide is the peptide for which we are determining the sequence
  ## aminoacid is a vector of amino acids, and the aminoacidmass
  ## parameter is a vector of amino acid masses.  This is
  ## structured so that for all i in aminoacid, the mass of aminoacid[i]
  ## equals aminoacidmass[i].
  #print(peptide)
  prefixmass <- vector('numeric')
  
  for (i in 1:length(peptide)) {
    #print(length(peptide))
    #print(peptide[i])
    for (j in 1:20) {
      #print(aminoacid[j])
      if(aminoacid[j] == peptide[i]) {
        #print(aminoacid[j])
        #print(peptide[i])
        #print(prefixmass[i])
        #print(aminoacidmass[j])
        #print(is.numeric(prefixmass[i]))
        #if (!is.na(prefixmass[i]))
        if (i > 1)          
            prefixmass <- c(prefixmass, prefixmass[i-1] + aminoacidmass[j])
        else
            prefixmass <- c(prefixmass, aminoacidmass[j])
      }
    }
  }

  print(prefixmass)
  
  linspec <- rep(0,1)
  #print(linspec)
  #print(length(peptide) - 1)
  #for (i in 0:(length(peptide)-1)) {
  for (i in 1:(length(peptide)-1)) {    
    for (j in (i + 1):length(peptide)) {
      linspec <- c(linspec, prefixmass[j] - prefixmass[i])
    }
  }

  return(linspec)
}






integermasstable <- function() {
  proteins <- c('G', 'A', 'S', 'P', 'V', 'T', 'C', 'I', 'L', 'N', 'D', 'K', 'Q', 'E', 'M', 'H', 'F', 'R', 'Y', 'W')
  intmass <- as.integer(c(57, 71, 87, 97, 99, 101, 103, 113, 113, 114, 115, 128, 128, 129, 131, 137, 147, 156, 163, 186))

  df <- data.frame(cbind(proteins, intmass), stringsAsFactors=FALSE)
  df$intmass <- as.integer(df$intmass)

  return(df)
}
