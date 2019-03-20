# Functions for rate analysis


######## Calculates monthly payments following the equation ###########
#   M = P [ i(1 + i)^n ] / [ (1 + i)^n - 1]
MonthlyPayment <- function(InterestRate, MortgageLength, LoanAmmount){
  MonthlyEffectiveInterest <- (InterestRate)/12
  MonthlyPayment <- (LoanAmmount)*
    (MonthlyEffectiveInterest*((1+ MonthlyEffectiveInterest)^(MortgageLength*12)))/
    ((1+MonthlyEffectiveInterest)^(MortgageLength*12)-1)
  MonthlyPayment <- MonthlyPayment
  return(MonthlyPayment)
}




############# Populate data frames ####################################
CalculateTimeSeries <- function(LoanAmmount, Interest, MonthlyPayment, Fee){
  
  # Populate time series
  month <- c(1:360)
  df <- data.frame(month)
  
  # Calculate first row
  df$Interest[1] <- (LoanAmmount)*(Interest/12)
  df$CumulativeInterest[1]<- (LoanAmmount)*(Interest/12)
  df$RemainingPrincipal[1] <- LoanAmmount - (MonthlyPayment - (LoanAmmount)*(Interest/12))
  
  # Calculate subsequent months
  for(i in 2:360){
    if (df$RemainingPrincipal[i-1]>0){
      df$Interest[i] <- (Interest/12)*df$RemainingPrincipal[i-1]
    } else {
      df$Interest[i] <- 0
    }
    df$CumulativeInterest[i] <- df$CumulativeInterest[i-1] + df$Interest[i]
    df$RemainingPrincipal[i] <- df$RemainingPrincipal[i-1] - (MonthlyPayment - df$Interest[i])
  }
  
  # Offset by the fee
  df$CumulativeInterest <- df$CumulativeInterest + Fee
  
  return(df)
}
