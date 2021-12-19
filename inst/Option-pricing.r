####### Pricing options ###################
##########################################


################# Black Scholes formula ########

#dS = Stock Price

#dK = Strike Price 

#drf = Risk-free Interest Rate


#iT = Time to Expiration( In fraction of years)

#dSigma = Volatility of the Underlying asset


BlackScholes <- function(dS, dK, dr, iT, dSigma, type){
  
  if(type=="C"){
    d1 <- (log(dS/dK) + (drf + dSigma^2/2)*iT) / (dSigma*sqrt(iT))
    d2 <- d1 - dSigma*sqrt(iT)
    
    value <- dS*pnorm(d1) - dK*exp(-drf*iT)*pnorm(d2)
    return(value)}
  
  if(type=="P"){
    d1 <- (log(dS/dK) + (drf + dSigma^2/2)*iT) / (dSigma*sqrt(iT))
    d2 <- d1 - dSigma*sqrt(iT)
    
    value <-  (dK*exp(-drf*iT)*pnorm(-d2) - dS*pnorm(-d1))
    return(value)}
}


########### Retrive Stock data 

#install.packages("quantmod")
library("quantmod")

Currentdate = Sys.Date()




############################################
####### Input for the Option pricing ######
##########################################



date_t    = Sys.Date()
date_t_1  = Sys.Date() -365 

####### Get prices from Yahoo #########################
mPrice = getSymbols("NOVO-B.CO", from = "2020-11-28", to = "2021-11-28", auto.assign = FALSE)



######### Convert the Adjusted closing prices to numeric 

vPrice = as.numeric(mPrice[,6])


############  log Calculate Returns in % ################

vReturns = diff(log(vPrice)[-1])

######### Calculate standard deviation###############



dSigma_daily = sd(vReturns)


######### Converting into annualy standard deviation(250 trading days in a year) ##########

dSigma = dSigma_daily * sqrt(250)


########## The latest stock price #############


dS = vPrice[249]

#Strike Price 


dK = 700

# Risk-free Interest Rate

drf = 0.01

#Time to Expiration( In fraction of years)

iT = 0.25

################################################################
############# Now we can calculate the option price ############
################################################################




Optionprice = BlackScholes(dS = dS, dK = dK , iT=iT, dSigma = dSigma, type = "C")



############################################################################
#             Option Princing With the RQuantlib package 




install.packages("RQuantLib")
library("RQuantLib")

?option
?EuropeanOption
EuropeanOption



EuropeanOption(type = "call", underlying = dS, strik = dK, dividendyield =  0.03, riskFreeRate = drf, maturity = iT  )
