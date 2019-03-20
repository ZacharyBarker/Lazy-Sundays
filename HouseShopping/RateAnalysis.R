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
library(plotly)


# Set working directory
setwd("C:/Users/zacha/Desktop/Lazy-Sundays/HouseShopping")

# Load functions
source("Functions.R")

# Decision Variable
ActuallyPay <- 3000

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


# Calculate monthly payments
A_m <- MonthlyPayment(A_i, A_t, LoanAmmount)
B_m <- MonthlyPayment(B_i, B_t, LoanAmmount)
C_m <- MonthlyPayment(C_i, C_t, LoanAmmount)
D_m <- MonthlyPayment(D_i, D_t, LoanAmmount)
print("Monthly payment options:")
print(c("A = ", A_m))
print(c("B = ", B_m))
print(c("C = ", C_m))
print(c("D = ", D_m))


# Populate time series
# A <- CalculateTimeSeries(LoanAmmount, A_i, A_m, A_f)
# B <- CalculateTimeSeries(LoanAmmount, B_i, B_m, B_f)
# C <- CalculateTimeSeries(LoanAmmount, C_i, C_m, C_f)
# D <- CalculateTimeSeries(LoanAmmount, D_i, D_m, D_f)
A <- CalculateTimeSeries(LoanAmmount, A_i, ActuallyPay, A_f)
B <- CalculateTimeSeries(LoanAmmount, B_i, ActuallyPay, B_f)
C <- CalculateTimeSeries(LoanAmmount, C_i, ActuallyPay, C_f)
D <- CalculateTimeSeries(LoanAmmount, D_i, ActuallyPay, D_f)


# Cumulative cost
print("Total costs")
print(c("A = ", A$CumulativeInterest[360]))
print(c("B = ",B$CumulativeInterest[360]))
print(c("C = ", C$CumulativeInterest[360]))
print(c("D = ", D$CumulativeInterest[360]))


# Create master data frame
month <- c(1:360)
scenarios <- data.frame(month,A$CumulativeInterest,B$CumulativeInterest,C$CumulativeInterest,D$CumulativeInterest)


# Reshape to plot
scenarios <- melt(scenarios, id="month")

# Plot
timeseries <- ggplot(data = scenarios) +
  geom_line(aes(month,value, color = variable), size = .75) +
  theme_bw()+
  xlab("Month")+
  ylab("Cumulative Cost ($)")+
  # coord_cartesian(ylim = c(0,30000), xlim = c(0, 50))+
  theme(legend.justification=c(0,1),
        legend.position=c(0,1),
        legend.title=element_blank(),
        legend.background = element_rect(fill="transparent"),
        plot.title = element_text(size = rel(2)),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)))

timeseries<- ggplotly(timeseries)
print(timeseries)


