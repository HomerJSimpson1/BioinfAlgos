minnumcoins <- function(amount, coin) {
  ## Coin array is a vector of possible values for the currency
  ## The integer (scalar) amount is the total amount of change we need to generate

  m <- 0:amount
  #minnumcns <- rep(0, (amount + 1))
  BIGVALUE <- 100000000
  minnumcns <- rep(BIGVALUE, (amount + 1))
  minnumcns[1] <- 0
  vals <- vector('numeric',length(coin))
  change <- rep(0, length(coin))

  for (i in 1:amount) {
    for (j in 1:length(coin)) {
      #vals <- rep(0,length(coin))
      #vals <- vector('numeric',length(coin))
      diff <- i - coin[j]
      #print(diff)
      if (diff < 0) {
        ## Then this value doesn't count.  Make it a really large value so it won't
        ## be selected.
        val <- BIGVALUE
        #vals[j] <- BIGVALUE
        #vals[j] <- 10000
        #print(vals)
      }
      else {
        val <- minnumcns[diff + 1] ## Arrrays are not zero-based in R
        #vals[j] <- minnumcns[diff + 1]
      }
      #print(val)
      #vals <- c(vals, val)
      vals[j] <- val
    } ## end for (j in 1:length(coin))
    #print(vals)
    #print(min(vals))
    minnumcns[i + 1] <- min(vals) + 1
    indx <- which.min(vals)
    change[indx] = change[indx] + 1
  } ## end for (i in 1:amount)

  #return(minnumcns)
  return(minnumcns[length(minnumcns)])
  #return(change)
}


    ## Algorithm given in text is: (which is slightly different than what I did above)
    ## DPCHANGE(money, Coins)
    ##  MinNumCoins(0) <- 0
    ##  for m <- 1 to money
    ##     MinNumCoins(m) <- Infinity
    ##         for i <- 1 to |Coins|
    ##             if m >= coini
    ##                 if MinNumCoins(m - coini) + 1 < MinNumCoins(m)
    ##                     MinNumCoins(m) <- MinNumCoins(m - coini) + 1
    ## output MinNumCoins(money) 



## sumvec <- function(vec) {  ## Unneeded, just use sum(vec)

## getmin <- function(coin, minnumcoinsarray) {
## }
