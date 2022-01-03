rm(list = ls())

library(MPV)
install.packages("olsrr")
install.packages("metafor")
library(metafor)

#5.9
data(table.b8)
attach(table.b8)

regressors_house <- cbind(table.b8$x1, table.b8$x2)

model_house <- lm(table.b8$y ~ regressors_house)
summary(model_house)

anova(model_house)

model.res <- resid(model_house)

qqnorm(model.res, ylab="Standardized Residuals", xlab="Normal Scores", main="Table B.8") 
qqline(model.res)

plot(predict(model_house),model.res)
autoplot(model_house)

#b

plot(table.b8$x1, table.b8$y)
plot(table.b8$x2, table.b8$y)


model_house <- lm(sqrt(table.b8$y) ~ regressors_house)
summary(model_house)

anova(model_house)

model.res <- resid(model_house)

qqnorm(model.res, ylab="Standardized Residuals", xlab="Normal Scores", main="Square Root y case: Table B.8") 
qqline(model.res)

plot(predict(model_house),model.res)
autoplot(model_house)

#5.12
df.5.12 <- read.table('HW4_5_12.txt',header = T)
df.5.12

df.5.12$w <- (1/((df.5.12$si)**2))

df.5.12$w[10] <- 1
df.5.12$w[10]
df.5.12$w[14] <- 1
df.5.12

#b^2

df.5.12$xbar <- (df.5.12$x1+df.5.12$x2+df.5.12$x3)/3
df.5.12$variance <- (df.5.12$si)**2
df.5.12

model_b <- lm(df.5.12$si ~ df.5.12$x1 + df.5.12$x2 +
                df.5.12$x3)

summary(model_b)


#a

hold_1 <- df.5.12
hold_2 <- df.5.12

hold_1$y1 <- df.5.12$y2
hold_2$y1 <- df.5.12$y3

df.merged <- rbind(df.5.12, hold_1, hold_2)
df.merged

#now, calculating weights

df.5.12$inverse_weight <- 48.000 + 11.517*df.5.12$x1 +
  15.322*df.5.12$x2 + 29.172*df.5.12$x3

df.5.12$weight_variance <- (1/df.5.12$inverse_weight)

#df.merged <- df.merged[ - c(6,7)]
df.merged

#now, only y1 is left to comprehend all y variations.

regressors_house <- cbind(df.merged$x1, df.merged$x2,
                          df.merged$x3)

model_house <- lm((df.merged$y1) ~ regressors_house)
summary(model_house)

anova(model_house)

model.res <- resid(model_house)

qqnorm(model.res, ylab="Standardized Residuals", xlab="Normal Scores", main="Q. 5#12") 
qqline(model.res)

plot(predict(model_house),model.res)
autoplot(model_house)


#to find the beta^:

df.5.12.x <- subset(df.5.12, select = c("x1","x2","x3"))

W <- as.numeric(as.matrix(df.5.12$weight_variance))
W
W_diagonal <- diag(W, 27, 27)
det(W_diagonal)

W_diagonal <- solve(W_diagonal)

Y <- as.numeric(as.matrix(df.5.12$y_bar))
Y
mode(X[1])
df.5.12

Vec <- c(rep(1), rep(1, 26))
Vec
df.5.12.x$x0 <- Vec

df.5.12.x <- df.5.12.x[,c(4,1,2,3)]
df.5.12.x
Beta_hat <- ((solve((t(df.5.12.x) %*% W_diagonal) 
        %*% as.matrix(df.5.12.x)) 
  %*% t(df.5.12.x)) %*% W_diagonal) %*% Y 

Beta_hat

prediction_weighted <- (Beta_hat[1] + 
    Beta_hat[2]*df.merged$x1 +
    Beta_hat[3]*df.merged$x2 +
    Beta_hat[4]*df.merged$x3)

weighted_residuals <- df.merged$y1 - (Beta_hat[1] + 
                                        Beta_hat[2]*df.merged$x1 +
                                        Beta_hat[3]*df.merged$x2 +
                                        Beta_hat[4]*df.merged$x3)

qqnorm(weighted_residuals, ylab="Standardized Weighted Residuals", 
       xlab="Normal Scores", main="Weighted Case: Q. 5#12") 
qqline(weighted_residuals)

plot(prediction_weighted, weighted_residuals, main = "Weighted Case
     Residuals Plot")
autoplot(model_house)

plot(df.merged$x3, df.merged$y1)

#5.12, c

df.5.12$w <- sqrt(1/((df.5.12$si)**2))
df.5.12
df.5.12$w[10] <- 0.27
df.5.12$w[10]
df.5.12$w[14] <- 0.27
df.5.12

#contd

hold_1 <- df.5.12
hold_2 <- df.5.12

hold_1$y1 <- df.5.12$y2
hold_2$y1 <- df.5.12$y3

df.merged <- rbind(df.5.12, hold_1, hold_2)
df.merged

df.merged <- df.merged[ - c(6,7)]
df.merged

#contd

df.5.12.x <- subset(df.5.12, select = c("x1","x2","x3"))

W <- as.numeric(as.matrix(df.5.12$w))
W
W_diagonal <- diag(W, 27, 27)
det(W_diagonal)

W_diagonal <- solve(W_diagonal)

Y <- as.numeric(as.matrix(df.5.12$y_bar))
Y
mode(X[1])
df.5.12

Vec <- c(rep(1), rep(1, 26))
Vec
df.5.12.x$x0 <- Vec

df.5.12.x <- df.5.12.x[,c(4,1,2,3)]
df.5.12.x
Beta_hat <- ((solve((t(df.5.12.x) %*% W_diagonal) 
                    %*% as.matrix(df.5.12.x)) 
              %*% t(df.5.12.x)) %*% W_diagonal) %*% Y 

Beta_hat

prediction_weighted <- (Beta_hat[1] + 
                          Beta_hat[2]*df.merged$x1 +
                          Beta_hat[3]*df.merged$x2 +
                          Beta_hat[4]*df.merged$x3)

weighted_residuals <- df.merged$y1 - (Beta_hat[1] + 
                                        Beta_hat[2]*df.merged$x1 +
                                        Beta_hat[3]*df.merged$x2 +
                                        Beta_hat[4]*df.merged$x3)

qqnorm(weighted_residuals, ylab="Standardized Weighted Residuals", 
       xlab="Normal Scores", main="Variance Square Root, Weighted Case: Q. 5#12") 
qqline(weighted_residuals)

plot(prediction_weighted, weighted_residuals, main = "Variance Square Root,
Weighted Case
     Residuals Plot")
autoplot(model_house)

plot(df.merged$x3, df.merged$y1)

#6.5

data(table.b1)
attach(table.b1)

model_6_5 <- lm(table.b1$y ~ table.b1$x1 + table.b1$x2 + 
     table.b1$x3 + table.b1$x4 + table.b1$x5 + 
     table.b1$x6 + table.b1$x7 + table.b1$x8 + 
     table.b1$x9)

summary(model_6_5)

summary(influence.measures(model_6_5))

### calculate influence diagnostics
inf <- influence(model_6_5)

66/94

### plot the influence diagnostics
plot(inf, layout=c(8,1))

2*sqrt(11)/(17)

2*sqrt(10/28)
2*10/28
2*sqrt(10/28)
2/sqrt(28)



