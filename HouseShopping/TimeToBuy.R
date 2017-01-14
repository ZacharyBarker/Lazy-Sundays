################################################################################
#
# Time to buy analysis
#
# Zachary Barker, January 2017
#
################################################################################

# House terms
HousePrice = 200000
DownPayment = 20000
InterestRate = 0.04
TaxRate = 0.013
MortgageLength = 15

Rent = 600
Roommate = 400


# Calculated monthly payments following the equation:
#
#    P = (Pv*R) / [1 - (1 + R)^(-n)] 
#
#    where:
#
#    Pv  = Present Value (beginning value or amount of loan)
#    APR = Annual Percentage Rate (one year time period)
#    R  = Periodic Interest Rate = APR/ # of interest periods per year
#    P   = Monthly Payment
#    n   = # of interest periods for overall time period (i.e., interest
#      periods per year * number of years)

MonthlyEffectiveInterest = (InterestRate + TaxRate)/12
MonthlyPayment = (HousePrice - DownPayment)
     *(MonthlyEffectiveInterest*((1+ MonthlyEffectiveInterest)^(MortgageLength*12)))
     /((1+MonthlyEffectiveInterest)^(MortgageLength*12)-1)




