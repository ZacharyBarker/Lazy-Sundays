################################################################################
#
# Mortgage rate analysis
#
# Zachary Barker, March 2019
#
################################################################################

# Import libraries
library(ggplot2)
library(reshape2)


# Decision Variable
MonthlyPayment <- 2000


# House terms
HousePrice <- 263000
DownPayment <- HousePrice * .2
LoanAmmount <- HousePrice * .8
TaxRate <- 0.013
Insurance <- 75
HOA <- 29


# Financing options
A_i <- 0.03625
A_f <- 2600
A_t <- 15

B_i <- 0.04
B_f <- -263
B_t <- 15

C_i <- 0.04125
C_f <- 2600
C_t <- 30

D_i <- 0.04375
D_f <- 263
D_t <- 30


# Calculated monthly payments following the equation:
#    M = P [ i(1 + i)^n ] / [ (1 + i)^n - 1]
# MonthlyPayment <- function(InterestRate, MortgageLength, HousePrice, DownPayment){
#   MonthlyEffectiveInterest <- (InterestRate)/12
#   MonthlyPayment <- (HousePrice - DownPayment)*
#     (MonthlyEffectiveInterest*((1+ MonthlyEffectiveInterest)^(MortgageLength*12)))/
#     ((1+MonthlyEffectiveInterest)^(MortgageLength*12)-1)
#   return(MonthlyPayment)
# }




# Calculate time series
month <- c(1:360)
df <- data.frame(month)


# Calculate first row
df$A_interest[1] <- (HousePrice - DownPayment)*(A_i/12)
df$A_remainingPrincipal[1] <- LoanAmmount - (MonthlyPayment - df$A_interest[1])



# Reshape to plot
scenarios <- melt(scenarios, id="month")

# Plot
timeseries <- ggplot(data = scenarios) + 
  geom_line(aes(month,value, colour=variable), size = 1) +
  theme_bw()+
  xlab("Month")+
  ylab("Cumulative Cost ($)")+
  coord_cartesian(ylim = c(0,30000), xlim = c(0, 360))+
  theme(legend.justification=c(0,1), 
        legend.position=c(0,1), 
        legend.title=element_blank(), 
        legend.background = element_rect(fill="transparent"),
        plot.title = element_text(size = rel(2)),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)))

print(timeseries)


