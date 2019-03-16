################################################################################
#
# Hedonic evaluation
#
# Zachary Barkjohn, March 2019
#
################################################################################

# Import libraries
library(ggplot2)
library(GGally)
library(nnls)


# Read in data
dataH <- read.csv("C:/Users/zachary.barker/Desktop/Lazy-Sundays/HouseShopping/Data/HouseData.csv",header=T)

# Target house attributes (1, SqFeet, Lot, Bed, Bath, Garage)
target <- c(1,1553,0.142,3,2.5,1)


# Facet plot of all variable combinations
#facetPlot <- ggpairs(dataH, columns = 4:ncol(dataH)) + theme_bw()
#print(facetPlot)




### Multivariete regression ###

# Model 1: All variables, no constraints
dataH1 <- dataH[-c(1:3)]
model1 <- lm(dataH1$Price ~ ., data = dataH1)
price1 <- model1$coefficients %*% target

# Model 2: Non-negative coefficients
A <- data.matrix(dataH1[1:5])
A <- cbind(A, 1)
B <- data.matrix(dataH1[6])
model2 <- nnls(A, B)
target2 <- target[-c(1)]
target2 <- append(target2,1, after = length(target2))
price2 <- model2$x %*% target2

# Model 3: Non-negative constraints, no Lot size
A <- data.matrix(dataH1[-c(2,6)])
A <- cbind(A,1)
B <- data.matrix(dataH1[6])
model3 <- nnls(A, B)
target3 <- target2[-c(2)]
price3 <- model3$x %*% target3

