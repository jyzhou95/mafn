require(quantstrat)

suppressWarnings(rm("order_book.pair1",pos=.strategy))
suppressWarnings(rm("account.pairs", "portfolio.pair1", pos=.blotter))
suppressWarnings(rm("startDate", "endDate", "startDate", "initEq", "SD", "N", 
                    "symb1", "symb2", "portfolio1.st", "account.st", 
                    "etfVolStrat", "out1"))

startDate <- '2005-01-01'    
endDate <- '2018-11-02'
initEq <- 100000

MaxPos <- 1500  #max position in stockA; 
# max position in stock B will be max * ratio, i.e. no hard position limit in 
# Stock B
lvls <- 3  #how many times to fade; Each order's qty will = MaxPos/lvls


symb1 <- 'SPY' #change these to try other pairs
symb2 <- 'TLT' #if you change them, make sure position limits still make sense

portfolio1.st <- 'pair1'
account.st <- 'pairs'

getSymbols(c(symb1, symb2), from=startDate, to=endDate, adjust=TRUE) 

# The following function is used to make sure the timestamps of all symbols are 
# the same deletes rows where one of the stocks is missing data
alignSymbols <- function(symbols, env=.GlobalEnv) {
  # This is a simplified version of qmao::alignSymbols()
  if (length(symbols) < 2) 
    stop("Must provide at least 2 symbols")
  if (any(!is.character(symbols))) 
    stop("Symbols must be vector of character strings.")
  ff <- get(symbols[1],env=env)
  for (sym in symbols[-1]) {
    tmp.sym <- get(sym,env=env)
    ff <- merge(ff, tmp.sym, all=FALSE)
  }
  for (sym in symbols) {
    assign(sym,ff[,grep(sym, colnames(ff))], env=env)
  }
  symbols
}
alignSymbols(c(symb1, symb2))

# Define Instruments
currency("USD")
stock(symb1, currency="USD", multiplier=1)
stock(symb2, currency="USD", multiplier=1)

# Initialize Portfolio, Account, and Orders
initPortf(name=portfolio1.st, c(symb1,symb2))
initAcct(account.st, portfolios=portfolio1.st, initEq=initEq)
initOrders(portfolio=portfolio1.st)

# osFUN will need to know which symbol is leg 1 and which is leg 2 as well as 
# what the values are for MaxPos and lvls.  So, create a slot in portfolio to 
# hold this info.
pair <- c(1, 2, MaxPos, lvls)
names(pair) <- c(symb1, symb2, "MaxPos", "lvls")
.blotter[[paste('portfolio', portfolio1.st, sep='.')]]$pair <- pair

# Create initial position limits and levels by symbol
# allow 3 entries for long and short if lvls=3.
addPosLimit(portfolio=portfolio1.st, timestamp=startDate, symbol=symb1, 
            maxpos=MaxPos, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)
addPosLimit(portfolio=portfolio1.st, timestamp=startDate, symbol=symb2, 
            maxpos=MaxPos, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)

# Create a strategy object 
etfVolStrat <- strategy('etfVolStrat')

# Indicator function
calcHistoricVolatility <- function(x) { 
  
}




