rm(list = ls())

library(MPV)
library(tidyverse)
library(matlib)
install.packages("ggfortify")
library(ggfortify)
install.packages("car")
library(car)
library(MASS)
install.packages("qpcR")
library(qpcR)

install.packages("alr3")

#4.5

data(table.b4)
attach(table.b4)

regressors_house <- cbind(table.b4$x1, table.b4$x2, table.b4$x3,
                          table.b4$x4, table.b4$x5, table.b4$x6,
                          table.b4$x7, table.b4$x8, table.b4$x9)

model_house <- lm(table.b4$y ~ regressors_house)
summary(model_house)

anova(model_house)

model.res <- resid(model_house)

qqnorm(model.res, ylab="Standardized Residuals", xlab="Normal Scores", main="Table B.4") 
qqline(model.res)

plot(predict(model_house),model.res)
autoplot(model_house)

car::avPlots(model, id.n=2, id.cex=0.7)

studres(model_house)
plot(studres(model_house))

rstudent(model_house)
?rstandard

stdres(model_house)

?studres

rstandard(model_house)

#4.12

volume <- c(2084, 2084, 2273, 2273, 2273,
            2463, 2463, 2651, 2652, 2652,
            2842, 2842, 3030, 3031, 3031,
            3221, 3221, 3409, 3410, 3600,
            3600, 3788, 3789, 3789, 3979,
            3979, 4167, 4168, 4168, 4358, 
            4358, 4546, 4547)

pressure <- c(4599, 4600, 5044, 5043, 5044,
              5488, 5487, 5931, 5932, 5932,
              6380, 6380, 6818, 6817, 6818,
              7266, 7268, 7709, 7710, 8156,
              8158, 8597, 8599, 8600, 9048,
              9048, 9484, 9487, 9487, 9936,
              9939, 10377, 10379)

model <- lm(pressure ~ volume)

model.residual <- resid(model)

qqnorm(model.residual, ylab="Standardized Residuals", xlab="Normal Scores", main="Table Volume & Pressure") 
qqline(model.residual)

plot(predict(model),model.residual)
autoplot(model)

num <- seq(1:33)

plot(num, model.residual)

plot(num, model.residual, type="l", col="green", lwd=5, xlab="Observation Order", 
     ylab="Residuals")

#4.16
data(table.b8)
attach(table.b8)

regressors_b8 <- cbind(table.b8$x2)

model_b8 <- lm(table.b8$y ~ regressors_b8)

model.resid.b8 <- resid(model_b8)

qqnorm(model.resid.b8, ylab="Standardized Residuals", xlab="Normal Scores", main="B8") 
qqline(model.resid.b8)

plot(predict(model_b8),model.resid.b8)
autoplot(model_b8)

full.model.res <- resid(model_b8)
pr.full <- full.model.res/(1 - lm.influence(model_b8)$hat)
pr.full

press.b8 <- sum(pr.full^2)
press.b8
model.b8.anova <- anova(model_b8)
#' Calculate the total sum of squares
tss <- sum(model.b8.anova$'Sum Sq')
# Calculate the predictive R^2
pred.r.squared.full <- 1-press.b8/(tss)
pred.r.squared.full

#4.19

df.4.19 <- read.table('HW3_4_19.txt',header = T)

df.4.19

regressors.4.19 <- cbind(df.4.19$x1, df.4.19$x2, df.4.19$x3)

model.4.19 <- lm(df.4.19$y ~ regressors.4.19)
summary(model.4.19)

anova(model.4.19)

model.res.4.19 <- resid(model.4.19)

qqnorm(model.res.4.19, ylab="Standardized Residuals", xlab="Normal Scores", main="4.19") 
qqline(model.res.4.19)

plot(predict(model.4.19), model.res.4.19)
autoplot(model.4.19)

predict(model.4.19)
model.res.4.19

df_full <- df.4.19[c(1:8),1:4]
df_red <- df.4.19[c(9:14),1:4]
df_red
df_full

regressors.pe <- cbind(df_red$x1, df_red$x2, df_red$x3)

model.pe <- lm(df_red$y ~ regressors.pe)
summary(model.pe)

anova(model.pe)

126.83/(14-9)

1624.3 - 126.83
1497.47 /(7-2)

213.9243 / 25.366
3902/7


#to find ssres of full model


df.4.19 <- read.table('HW3_4_19.txt',header = T)

df.4.19
df_full <- df.4.19[c(1:9),1:4]

df_full

regressors.4.19 <- cbind(df.4.19$x1, df.4.19$x2, df.4.19$x3)
regressors.4.19
model.4.19 <- lm(df_full$y ~ df_full$x1 + df_full$x2 + df_full$x3)
summary(model.4.19)

anova(model.4.19)

299*7+126.83

299.494 / 25.366 



