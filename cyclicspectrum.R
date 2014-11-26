runcyclicspectrum <- function(peptide) {
  imt <- integermasstable()
  peptide <- strsplit(peptide,"")[[1]]
  cyclicspec <- cyclicspectrum(peptide, imt$proteins, imt$intmass)
  
  return(cyclicspec)
}




cyclicspectrum <- function(peptide, aminoacid, aminoacidmass) {
  ## peptide is the peptide for which we are determining the sequence
  ## aminoacid is a vector of amino acids, and the aminoacidmass
  ## parameter is a vector of amino acid masses.  This is
  ## structured so that for all i in aminoacid, the mass of aminoacid[i]
  ## equals aminoacidmass[i].
  prefixmass <- rep(0,1)  
  
  for (i in 1:length(peptide)) {
    for (j in 1:20) {
      if(aminoacid[j] == peptide[i]) {
        if (i > 1)          
            prefixmass <- c(prefixmass, prefixmass[i] + aminoacidmass[j])
        else
           prefixmass <- c(prefixmass, aminoacidmass[j])
      }
    }
  }
  peptidemass <- prefixmass[length(prefixmass)]

  cyclspec <- rep(0,1)
  for (i in 1:length(peptide)) {
    for (j in (i + 1):(length(peptide) + 1)) {
      cyclspec <- c(cyclspec, prefixmass[j] - prefixmass[i])

      if (i > 1 && j < (length(peptide) + 1))
        cyclspec <- c(cyclspec, (peptidemass - (prefixmass[j] - prefixmass[i])))
    }
  }

  return(sort(cyclspec))
}




integermasstable <- function() {
  proteins <- c('G', 'A', 'S', 'P', 'V', 'T', 'C', 'I', 'L', 'N', 'D', 'K', 'Q', 'E', 'M', 'H', 'F', 'R', 'Y', 'W')
  intmass <- as.integer(c(57, 71, 87, 97, 99, 101, 103, 113, 113, 114, 115, 128, 128, 129, 131, 137, 147, 156, 163, 186))

  df <- data.frame(cbind(proteins, intmass), stringsAsFactors=FALSE)
  df$intmass <- as.integer(df$intmass)

  return(df)
}
