library(MPV)
library(zoo)
install.packages("glmulti")
library(glmulti)
#library(olsrr)
library(leaps)
rm(list = ls())

#8.3
#defining the database and adding binary variable columns
data(delivery, package="robustbase")

df <- delivery

san_diego <- c(1,2,3,4,5,6,7)
boston <- c(8,9,10,11,12,13,14,15,16,17)
austin <- c(18,19,20,21,22,23)
minneapolis <- c(24,25)

df$x_s <- ifelse(seq_len(nrow(df)) %in% san_diego, 1, 0)
df$x_b <- ifelse(seq_len(nrow(df)) %in% boston, 1, 0)
df$x_a <- ifelse(seq_len(nrow(df)) %in% austin, 1, 0)
df$x_m <- ifelse(seq_len(nrow(df)) %in% minneapolis, 1, 0)

model <- lm(delTime ~ n.prod + distance + x_s + x_b + x_a, data = df)

summary(model)

plot(model)

with(df,plot(delTime, distance))
abline(0.416, 1.77)


#b

regressors <- cbind(df$n.prod, df$distance, df$x_s, df$x_b,
                    df$x_a)

regressors_reduced <- cbind(df$n.prod, df$distance)

anova(lm(df$delTime ~ regressors))

general_model <- lm(df$delTime ~ regressors)

reduced_model <- lm(df$delTime ~ regressors_reduced)

anova(reduced_model)
anova(general_model)

anova(reduced_model, general_model)

233.7-169.5

?anova

#c
model.res <- resid(general_model)

qqnorm(model.res, ylab="Standardized Residuals", xlab="Normal Scores", main="Q. 8#3") 
qqline(model.res)

plot(predict(general_model),model.res, main="Q. 8#3") 
autoplot(model_house)

#8.11

tensile_strength <- c(7,7,15,11,9,
                      12,17,12,18,18,
                      14,18,18,19,19,
                      19,25,22,19,23,
                      7,10,11,15,11)

ones <- c(replicate(25,1))
x1 <- c(1,1,1,1,1,replicate(20,0))
x2 <- c(replicate(5,0),1,1,1,1,1,replicate(15,0))
x3 <- c(replicate(10,0),1,1,1,1,1,replicate(10,0))
x4 <- c(replicate(15,0),1,1,1,1,1,replicate(5,0))

df_c <- data.frame(ones, x1, x2, x3, x4)
df_c

y_vector <- data.frame(tensile_strength)
y_vector

model <- lm(y_vector$tensile_strength ~ df_c$x1 + df_c$x2 +
            df_c$x3 + df_c$x4)

summary(model)

#9.7

data(table.b3)
attach(table.b3)

df <- table.b3
df[is.na(df)] <- 0
df$x3 <- as.numeric(df$x3)

x_subset <- df[ -c(1) ]
cor(x_subset)

ones <- c(replicate(32,1))

x_subset$x0 <- ones
x_subset

x_subset <- x_subset[, c(12,1,2,3,4,5,6,7,8,9,10,11)]
x_subset

t_x <- (t(x_subset))

X_matrix <- t_x %*% as.matrix(x_subset)
eigen(X_matrix)
solve(X_matrix)
eigen
4.459509e+08/1.217247e-02

#10.1

data("table.b1")
df <- table.b1

model_I <- lm(y ~ 1, data = df)
model <- lm(y ~ ., data = df)

forward <- step(model_I, 
                direction='forward', 
                scope=formula(model), trace=0)

forward$anova
summary(forward)
forward$coefficients

backward <- step(model, 
                direction='backward', 
                scope=formula(model_I))

summary(backward)

backward$coefficients
anova(backward)
anova(model)

regfit_full = regsubsets(y~., data = df)
summary(regfit_full)
names(regfit_full)

#both <- step(intercept_only, direction='both', scope=formula(all), trace=0)

#10.5

data("table.b3")
df <- table.b3

df[is.na(df)] <- 0
df$x3 <- as.numeric(df$x3)

model_I <- lm(y ~ 1, data = df)
model <- lm(y ~ ., data = df)

forward <- step(model_I, 
                direction='forward', 
                scope=formula(model), trace=0)

forward$anova
summary(forward)
forward$coefficients

backward <- step(model, 
                 direction='backward', 
                 scope=formula(model_I))

summary(backward)

backward$coefficients
anova(backward)
anova(model)

regfit_full = regsubsets(y~., data = df)
info <- summary(regfit_full)
info$adjr2

cbind(info$which, round(cbind(rsq=info$rsq, 
                              adjr2=info$adjr2, 
                              cp=info$cp, bic=info$bic, 
                              rss=info$rss), 3))

stepw <- step(model_I, 
     direction='both', 
     scope=formula(model))

model_x1 <- lm(y ~ x1, data = df)
summary(model_x1)

#all-l-model

data("table.b3")
df <- table.b3

df[is.na(df)] <- 0
df$x3 <- as.numeric(df$x3)

model <- lm(y ~ ., data = df)

model_10_5 <- ols_step_all_possible(model)

#10.17

data("table.b12")
df <- table.b12

model <- lm(pitch ~ ., data = df)

model_10_17 <- ols_step_all_possible(model)

model_10_17 <- lm(pitch ~ soaktime + difftime, data = table.b12)

anova(model_10_17)

model.res <- resid(model_10_17)

qqnorm(model.res, ylab="Standardized Residuals", xlab="Normal Scores", main="Q: 10.17") 
qqline(model.res)

plot(predict(model_10_17),model.res)
autoplot(model_house)





#11.17

data("table.b13")
df_11_17 <- table.b13

train.index <- sample(c(1:dim(df_11_17)[1]), dim(df_11_17)[1]*0.7)

train.df <- df_11_17[train.index, ]
valid.df <- df_11_17[-train.index, ]

train.df

model_11_17 <- lm(y ~ x1+ x4+ x5+ x6, data = train.df)

summary(model_11_17)

pred_11_17 <- predict(model_11_17, valid.df)

pred_11_17-valid.df$y

pred_11_17

valid.df$y

ols_step_all_possible(model_11_17)






