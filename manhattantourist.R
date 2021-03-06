manhattantourist <- function(n, m, Down, Right, Diag) {
  ## n is the number of rows
  ## m is the number of columns
  ## Down is the matrix of weights of the vertical
  ## edges pointing down
  ## Right is the matrix of weights of the horizontal
  ## edges pointing to the right
  ## Diag is the matrix of weights of the diagonal
  ## edges pointing diagonally down and to the right.

  ## number of nodes is (n + 1) * (m + 1)
  ## number of rows is (n + 1)
  ## number of columns is (m + 1)
  ## Initialize to -1 (so that anything is larger)
  s <- matrix(-1, nrow = (n + 1), ncol = (m + 1))
  
  s[1,1] <- 0

  for (i in 2:(n + 1)) {
    s[i, 1] <- s[(i - 1), 1] + Down[i-1, 1]
  }

  for (j in 2:(m + 1)) {
    s[1, j] <- s[1, (j - 1)] + Right[1, j-1]
  }
  
  for (i in 2:(n + 1)) {
    for (j in 2:(m + 1)) {
      s[i, j] <- max(s[i - 1, j] + Down[i-1, j], s[i, j - 1] + Right[i, j-1], s[i - 1, j - 1] + Diag[i-1, j-1])
    }
  }

  return(s)
}




callmantour <- function(n, m) {
  down <- readmatrixfromfile("datasets/down3.txt")
  right <- readmatrixfromfile("datasets/right3.txt")
  diag <- readmatrixfromfile("datasets/diag3.txt")
  
  s <- manhattantourist(n, m, down, right, diag)

  return(s)
}



readmatrixfromfile <- function(filename) {
  ## filename is the path to the file which contains the matrix
  ## the expected format of the matrix is each row is on a separate line
  ## each element of the row is separated by a space
  ## when encountering a "-", first matrix is done
  ## second matrix begins on the line after that

  matrixres <- as.matrix(read.table(filename, header=FALSE, sep = " "))
  return(matrixres)
}
