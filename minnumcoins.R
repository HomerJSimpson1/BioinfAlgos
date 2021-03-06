minnumcoins <- function(amount, coin) {
  ## Coin array is a vector of possible values for the currency
  ## The integer (scalar) amount is the total amount of change we need to generate

  #m <- 0:amount
  #minnumcns <- rep(0, (amount + 1))
  BIGVALUE <- 100000000
  #minnumcns <- rep(BIGVALUE, (amount + 1))
  ## Modify so array size does not exceed largest coin denomination.
  ARRAYSIZE = coin[1] + 1
  minnumcns <- rep(BIGVALUE, ARRAYSIZE)
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
        #val <- minnumcns[diff + 1] ## Arrrays are not zero-based in R
        
        val <- minnumcns[(diff + 1) %% ARRAYSIZE ] ## Arrrays are not zero-based in R        
        #vals[j] <- minnumcns[diff + 1]
      }
      #print(val)
      #vals <- c(vals, val)
      vals[j] <- val
    } ## end for (j in 1:length(coin))
    #print(vals)
    #print(min(vals))
    if((min(vals) + 1) < minnumcns[(i + 1) %% ARRAYSIZE]) {
      minnumcns[(i + 1) %% ARRAYSIZE] <- min(vals) + 1
    }

  } ## end for (i in 1:amount)

  #indx <- which.min(vals)
  #print(indx)
  #change[indx] = change[indx] + 1

  minval = min(vals)
  amountremain = amount
  for (i in 1:length(coin)) {
    if(vals[i] == minval) {
      #change[i] = vals[i] + 1
      change[i] = floor(amountremain / coin[i])
      amountremain = amountremain %% coin[i]
    }
  }
  
  print(change)

  #return(minnumcns)
  #return(minnumcns[length(minnumcns)])
  #return(min(vals) + 1)
  return(change)
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
