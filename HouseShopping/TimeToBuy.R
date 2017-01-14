################################################################################
#
# Time to buy analysis
#
# Zachary Barker, January 2017
#
################################################################################

# House terms
HousePrice <- 200000
DownPayment <- 20000
InterestRate <- 0.04
TaxRate <- 0.013
MortgageLength <- 15

Rent <- 600
Roommate <- 400


# Calculated monthly payments following the equation:
#    P <- (Pv*R) / [1 - (1 + R)^(-n)] 
MonthlyEffectiveInterest <- (InterestRate + TaxRate)/12
MonthlyPayment <- (HousePrice - DownPayment)
     *(MonthlyEffectiveInterest*((1+ MonthlyEffectiveInterest)^(MortgageLength*12)))
     /((1+MonthlyEffectiveInterest)^(MortgageLength*12)-1)

# Calculate time series
month <- c(1:360)
df <- data.frame(month)
df$CumulativeAppartment <- df$month*Rent
df$CumulativeHouse <- df$month*MonthlyPayment

# Calculate first row 
df$MonthlyInterest[1] <- (HousePrice-DownPayment)*MonthlyEffectiveInterest
df$MonthlyEquity[1] <- MonthlyPayment - df$MonthlyInterest[1]
df$CumulativeInterest[1] <- df$MonthlyInterest[1]
df$CumulativeEquity[1] <- df$MonthlyEquity[1]

for(i in 2:360){
     df$MonthlyInterest[i] <- MonthlyEffectiveInterest*(HousePrice-DownPayment-df$CumulativeEquity[i-1])
     df$MonthlyEquity[i] <- MonthlyPayment - df$MonthlyInterest[i]
     df$CumulativeInterest[i] <- df$MonthlyInterest[i] + df$MonthlyInterest[i-1]
     df$CumulativeEquity[i] <- df$MonthlyEquity[i] + df$MonthlyEquity[i-1]
}






