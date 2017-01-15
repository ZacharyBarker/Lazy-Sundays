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
HousePrice <- 164000
DownPayment <- 10000
InterestRate <- 0.04
TaxRate <- 0.013
MortgageLength <- 15

# Other factors
Rent <- 600
Roommate <- 400
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
df$CumulativeAppartment <- df$month*Rent
df$CumulativeHouse <- df$month*MonthlyPayment

# Calculate first row 
df$MonthlyInterest[1] <-(HousePrice-DownPayment)*MonthlyEffectiveInterest
df$MonthlyEquity[1] <- MonthlyPayment - df$MonthlyInterest[1]
df$CumulativeInterest[1] <- df$MonthlyInterest[1]
df$CumulativeEquity[1] <- df$MonthlyEquity[1]

# Calculate remainder of timeseries 
for(i in 2:360){
     df$MonthlyInterest[i] <- MonthlyEffectiveInterest*(HousePrice-DownPayment-df$CumulativeEquity[i-1])
     df$MonthlyEquity[i] <- MonthlyPayment - df$MonthlyInterest[i]
     df$CumulativeInterest[i] <- df$MonthlyInterest[i] + df$CumulativeInterest[i-1]
     df$CumulativeEquity[i] <- df$MonthlyEquity[i] + df$CumulativeEquity[i-1]
}

# Calculate with roomate
df$Roommate <- df$CumulativeInterest - Roommate

# Subset to only graphing parameters
scenarios <- df[,c("month","CumulativeInterest","CumulativeAppartment", "Roommate")]

# Reshape to plot
scenarios <- melt(scenarios, id="month")

# Plot
timeseries <- ggplot(data = scenarios) + 
     geom_line(aes(month,value, colour=variable), size = 1) +
     theme_bw()+
     xlab("Month")+
     ylab("Cumulative Cost ($)")+
     coord_cartesian(ylim = c(0,30000), xlim = c(0, 24))+
     theme(legend.justification=c(0,1), 
           legend.position=c(0,1), 
           legend.title=element_blank(), 
           legend.background = element_rect(fill="transparent"),
           plot.title = element_text(size = rel(2)),
           axis.text = element_text(size = rel(1.2)),
           axis.title = element_text(size = rel(1.5)),
           legend.text = element_text(size = rel(1.5)))

print(timeseries)


