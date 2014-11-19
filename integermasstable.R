integermasstable <- function() {
  proteins <- c('G', 'A', 'S', 'P', 'V', 'T', 'C', 'I', 'L', 'N', 'D', 'K', 'Q', 'E', 'M', 'H', 'F', 'R', 'Y', 'W')
  intmass <- c(57, 71, 87, 97, 99, 101, 103, 113, 113, 114, 115, 128, 128, 129, 131, 137, 147, 156, 163, 186)

  print(length(proteins))
  print(length(intmass))

  return(data.frame(cbind(proteins, intmass)))
  
## G 57
## A 71
## S 87
## P 97
## V 99
## T 101
## C 103
## I 113
## L 113
## N 114
## D 115
## K 128
## Q 128
## E 129
## M 131
## H 137
## F 147
## R 156
## Y 163
## W 186


}
