rm(list = ls())


install.packages("MPV")
library(MPV)
library(tidyverse)
install.packages("matlib")
library(matlib)

data(table.b1)
attach(table.b1)

#3.1-a

model <- lm(table.b1$y ~ table.b1$x2 + table.b1$x7 + table.b1$x8)
summary(model)

regressors <- cbind(table.b1$x2, table.b1$x7, table.b1$x8)

regressors_reduced <- cbind(table.b1$x2, table.b1$x8)

anova(lm(table.b1$y ~ regressors))

general_model <- lm(table.b1$y ~ regressors)

reduced_model <- lm(table.b1$y ~ regressors_reduced)

anova(reduced_model, general_model)

#3.3b

x_transpose <- matrix(c(1, 2300,56,2100), ncol=4, nrow=1)
x_zero <- matrix(c(1, 2300,56,2100), ncol=1, nrow=4)
beta <- matrix(c(-1.808372, 0.003598,0.193960,-0.004816), ncol=1, nrow=4)
y_hat <- x_transpose %*% beta

sigma_hat_square = 69.87/(24)

sigma_hat <- sigma_hat_square^(0.5)

#X <- tibble(table.b1$x2, table.b1$x7, table.b1$x8)

ones <- c(1,1,1,1,1,1,1,1,1,1,
          1,1,1,1,1,1,1,1,1,1,
          1,1,1,1,1,1,1,1)

X <- tibble(ones, table.b1$x2, table.b1$x7, table.b1$x8)

X_t <- t(X)

hold <- X_t

X <- t(hold)

X_multiplied <- X_t %*% X

XX_inverse <- (X_multiplied)^(-1)

answer <- sqrt(sigma_hat_square %*% x_transpose %*% XX_inverse %*% x_zero)
answer

threshold_1 <- y_hat - 2.064*answer
threshold_2 <- y_hat + 2.064*answer

threshold_1
threshold_2


#3.7
data(table.b4)
attach(table.b4)

regressors_house <- cbind(table.b4$x1, table.b4$x2, table.b4$x3,
                    table.b4$x4, table.b4$x5, table.b4$x6,
                    table.b4$x7, table.b4$x8, table.b4$x9)

model_house <- lm(table.b4$y ~ regressors_house)
summary(model_house)

anova(model_house)

#3.7c

regressors_reduced <- cbind(table.b4$x1, table.b4$x2, table.b4$x5,
                            table.b4$x6, table.b4$x7, table.b4$x8, table.b4$x9)

reduced_model <- lm(table.b4$y ~ regressors_reduced)

anova(reduced_model, model_house)
anova(model_house)

anova(reduced_model)

result =  (707.30 - 701.69)/(2*8.696)
result

#question3.15

install.packages("Sleuth2")
library(Sleuth2)

df <- ex1123

df$Mort <- format(df$Mort, digits = 5)

model_pollution <- lm(df$Mort ~ df$Precip + df$Educ + df$Nonwhite + df$NOx + df$SO2)
summary(model_pollution)




