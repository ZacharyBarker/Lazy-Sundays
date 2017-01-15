################################################################################
#
# Time to buy analysis
#
# Zachary Barker, January 2017
#
################################################################################

# Import libraries
library(ggplot2)
library(reshape2)

# House terms
HousePrice <- 200000
DownPayment <- 10000
InterestRate <- 0.04
TaxRate <- 0.013
MortgageLength <- 15

# Other factors
Rent <- 600
WithRoommate <- 300
IncomeTaxRate <- 0.25 + 0.0575
Income <- 75000

# Calculated monthly payments following the equation:
#    P <- (Pv*R) / [1 - (1 + R)^(-n)] 
MonthlyEffectiveInterest <- (InterestRate + TaxRate)/12
MonthlyPayment <- (HousePrice - DownPayment)*
     (MonthlyEffectiveInterest*((1+ MonthlyEffectiveInterest)^(MortgageLength*12)))/
     ((1+MonthlyEffectiveInterest)^(MortgageLength*12)-1)

# Calculate time series
month <- c(1:360)
df <- data.frame(month)
df$Appartment <- df$month*Rent
df$CumulativeHouse <- df$month*MonthlyPayment

# Calculate first row 
df$MonthlyInterest[1] <-(HousePrice-DownPayment)*MonthlyEffectiveInterest
df$MonthlyEquity[1] <- MonthlyPayment - df$MonthlyInterest[1]
df$MortgageInterest[1] <- df$MonthlyInterest[1]
df$CumulativeEquity[1] <- df$MonthlyEquity[1]

# Calculate first year
for(i in 2:11){
     df$MonthlyInterest[i] <- MonthlyEffectiveInterest*(HousePrice-DownPayment-df$CumulativeEquity[i-1])
     df$MonthlyEquity[i] <- MonthlyPayment - df$MonthlyInterest[i]
     df$MortgageInterest[i] <- df$MonthlyInterest[i] + df$MortgageInterest[i-1]
     df$CumulativeEquity[i] <- df$MonthlyEquity[i] + df$CumulativeEquity[i-1]
}

# Remainder of timeseries include income tax write offs
for(i in 12:360){
     df$MonthlyInterest[i] <- MonthlyEffectiveInterest*(HousePrice-DownPayment-df$CumulativeEquity[i-1])
     df$MonthlyEquity[i] <- MonthlyPayment - df$MonthlyInterest[i]
     df$MortgageInterest[i] <- df$MonthlyInterest[i] + df$MortgageInterest[i-1]
     df$CumulativeEquity[i] <- df$MonthlyEquity[i] + df$CumulativeEquity[i-1]
     if(i%%12==0){
          df$MortgageInterest[i] <- df$MortgageInterest[i]-
          (sum(df$MonthlyInterest[(i-11):i])*IncomeTaxRate)}

}

# Calculate with roomate
df$WithRoommate <- df$MortgageInterest - (WithRoommate*month)

# Subset to only graphing parameters
scenarios <- df[,c("month","Appartment","MortgageInterest","WithRoommate")]

# Reshape to plot
scenarios <- melt(scenarios, id="month")

# Plot
timeseries <- ggplot(data = scenarios) + 
     geom_line(aes(month,value, colour=variable), size = 1) +
     theme_bw()+
     xlab("Month")+
     ylab("Cumulative Cost ($)")+
     coord_cartesian(ylim = c(0,30000), xlim = c(0, 60))+
     theme(legend.justification=c(0,1), 
           legend.position=c(0,1), 
           legend.title=element_blank(), 
           legend.background = element_rect(fill="transparent"),
           plot.title = element_text(size = rel(2)),
           axis.text = element_text(size = rel(1.2)),
           axis.title = element_text(size = rel(1.5)),
           legend.text = element_text(size = rel(1.5)))

print(timeseries)


