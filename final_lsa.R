library(glmnet)
library(openxlsx)
library(MPV)
library(MASS)
library(zoo)
library(leaps)
library(olsrr)
library(rms)
library(devtools)
library(pls)
#install_version("Design", version = "2.0")

set.seed(2)

df <- read.xlsx("final_table.xlsx")

#trying with 0.7 size
train.index <- sample(c(1:dim(df)[1]), dim(df)[1]*0.7)

#df$y <- (df$y^lambda-1)/lambda

train.df <- df[train.index, ]
valid.df <- df[-train.index, ]


df.factor <- df

'As the first step for strategy of model building, full model
is fit.'

full_model <- lm(y ~ ., data = df)
summary(full_model)

'Summary of the full model is presented, above. As results show,
only variable x5 and intercept are statisticcally significant at 0.001. Other variables
are not statisticcally significant. Adjusted R^2 is 0.744. While, the problem
with the provided model is the indicator variable, x5. These should be
leveled and factored, so that ordinality effect (1-2-3) would be removed.'

'Now, indicator variable factored version is presented.'

df$x5.1 <- ifelse(df$x5 == 1, 1, 0)
df$x5.2 <- ifelse(df$x5 == 2, 1, 0)

#Since x5 is factored, now base variable is removed.
df <- subset(df, select = -c(x5))

model_indicator <- lm(y ~ ., data = df)
summary(model_indicator)

'Results for indicator variable version of the model shows that
x1 is significant for 0.05, x3 is significant at 0.01 level. x4 is
significant at 0.05 as well. x5 is also significant with its two factors.'

'At this point, residual analysis is introduced to assure model adequacy.'

model.res <- resid(model_indicator)

qqnorm(model.res, ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Normal Probability Plot of Residuals") 
qqline(model.res)

plot(predict(model_indicator),rstudent(model_indicator),
     xlab = "Predicted Values",
     ylab = "Externally Studentized Residuals",
     main = "Versus Fit: Studentized Residuals vs. Fitted Values")
abline(0,0)

'In the Normal probability plot, distortions from normality
is observed in tails of the plot. This refers to the fact that
there can be a problem regarding normality of residuals based on the model.'

'When Versus Fit is observed, a "U" shaped non-normal behaviour
is being observed. This may refer to the need for an extra addition
of variable to the model to fit the non-normality.'

'In order to explore which exact variable needs a transformation,
plot of residuals against individual regressors will be conducted.'

plot(df$x1,rstudent(model_indicator),
     xlab = "x1",
     ylab = "Externally Studentized Residuals",
     main = "Versus Fit: Studentized Residuals vs. Regressor #1")

plot(df$x2, rstudent(model_indicator),
     xlab = "x2",
     ylab = "Externally Studentized Residuals",
     main = "Versus Fit: Studentized Residuals vs. Regressor #2")

plot(df$x3, rstudent(model_indicator),
     xlab = "x3",
     ylab = "Externally Studentized Residuals",
     main = "Versus Fit: Studentized Residuals vs. Regressor #3")

plot(df$x4, rstudent(model_indicator),
     xlab = "x4",
     ylab = "Externally Studentized Residuals",
     main = "Versus Fit: Studentized Residuals vs. Regressor #4")

plot(df$x5.1, rstudent(model_indicator),
     xlab = "x5.1",
     ylab = "Externally Studentized Residuals",
     main = "Versus Fit: Studentized Residuals vs. Regressor #5.1")

plot(df$x5.2, rstudent(model_indicator),
     xlab = "x5.2",
     ylab = "Externally Studentized Residuals",
     main = "Versus Fit: Studentized Residuals vs. Regressor #5.2")

'Above, versus fits between studentized residuals and regressors one by one
show that each x1, x2 and x3 show a U shape just like the predictions.'

'Below, plots between regressors and response are provided.
There can not be distinguished any strictly non-linear behaviour. Only
variable #3 may need a transformation to achieve
more linear relationship. This leads to the need to transform response variable, appropriately. No single
transform of any regressor will help to residual non-normality, since
mutliple linear regression.'

plot(df$x1, df$y,
     main = "Versus Fit: Response vs. Regressor x1")
plot(df$x2, df$y,
     main = "Versus Fit: Response vs. Regressor x2")
plot(df$x3, df$y,
     main = "Versus Fit: Response vs. Regressor x3")
plot(df$x4, df$y,
     main = "Versus Fit: Response vs. Regressor x4")


'Exponential transformation for x4 may offer more linear relationship
with the response variable.'

df$x4 <- df$x4^2
colnames(df)[colnames(df) == 'x4'] <- 'x4.square'

'Box-Cox transformation is found to be appropriate due to non-normality
of residuals. Box-Cox will provide a set of possible power
transformations.'

bc <- boxcox(y ~ ., data = df)
bc
lambda <- bc$x[which.max(bc$y)]
lambda

model.boxcox <- lm(((y^lambda-1)/lambda) ~ ., data = df)
summary(model.boxcox)

'An increase in the R^2 result is observed, with Adj. R-squared = 0.8734'

'At this point, residual analysis is presented
for box-cox applied model. Lambda = 0.7878788'

model.boxcox.res <- resid(model.boxcox)

sort(model.boxcox.res)
'When residuals are sorted, highest and lowest data points are
observed to be potential outliers. So, these points: 83 and 52
will be removed.'

qqnorm(model.boxcox.res, ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Box-Cox: Normal Probability Plot of Residuals") 
qqline(model.boxcox.res)

plot(predict(model.boxcox),rstudent(model.boxcox),
     xlab = "Predicted Values",
     ylab = "Externally Studentized Residuals",
     main = "BoxCox Versus Fit: Studentized Residuals vs. Fitted Values")
abline(0,0)

'With transformed model; linearity assumption is re-tested for
regressors. Linearity assumption can be assured as the 
initial model.'

'Relationship between x3 and transformed y seem to be slightly
different than linear. There are some points which also carry the potential
of being influential points. '

plot(df$x1, ((df$y)^lambda-1)/lambda)
plot(df$x2, ((df$y)^lambda-1)/lambda)
plot((df$x3), ((df$y)^lambda-1)/lambda)
plot(df$x4.square, ((df$y)^lambda-1)/lambda)

'Potential Outlier Detection based on Residuals'

'In this part, potential outliers will be analyzed from the Versus plot,
where studentized residuals and fitted y values are presented.'

plot(predict(model.boxcox),rstudent(model.boxcox),
     xlab = "Predicted Values",
     ylab = "Externally Studentized Residuals",
     main = "BoxCox Versus Fit: Studentized Residuals vs. Fitted Values")
abline(2.5,0)
abline(-2.5,0)

versus.residuals.df <- data.frame(predict(model.boxcox),rstudent(model.boxcox))
versus.residuals.df
filter(versus.residuals.df, versus.residuals.df$rstudent.model.boxcox. > 2.5 |
         versus.residuals.df$rstudent.model.boxcox. < -2.5)

'Based on the empirical rule, 2 standard deviations distance from
mean approximately includes 95% of the dataset, while 3 standard deviations
include 99.8% of the dataset. To achieve an approximately accurate
threshold to determine outliers, boundaries at 2.5 and -2.5 are
determined to eliminate outliers based on residuals vs. fitted values plot.'

#Dropping data points at rows: 52, 59, 83, 119

df.non.outliers <- df[-c(52,59,83,119),]

#Setting up model again without outliers

model.non.outliers <- lm(((y^lambda-1)/lambda) ~ ., 
                         data = df.non.outliers)

summary(model.non.outliers)
summary(model.boxcox)
anova(model.boxcox)
anova(model.non.outliers)

'Model summary when outliers are removed show the increase in
adjusted R^2 to 0.8973 and decrease in MSRes to 19.2 from 26.2
in the initial model. Removal of outliers created an increase of
26%, which would be important.'

plot(predict(model.non.outliers),rstudent(model.non.outliers),
     xlab = "Predicted Values",
     ylab = "Externally Studentized Residuals",
     main = "NonOutliers & BoxCox Versus Fit: Studentized Residuals vs. Fitted Values")
abline(2.5,0)
abline(-2.5,0)

model.non.outliers.res <- resid(model.non.outliers)
model.non.outliers.res
qqnorm(model.non.outliers, ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="NonOutliers & Box-Cox: Normal Probability Plot of Residuals") 
qqline(model.boxcox.res)

model.press <- PRESS(model.boxcox)

'Press statistic for the model is equal to 1841.907.
It will be compared with other models in the next steps.'

'Influential Point Analysis'

"In this section, analysis on Cook's Distance, hat value,
DFFITs, DFBETAs, and COVRATIO is presented to identify
potential leverage, influential and outliers."

"Based on hat value: 9, 24, 55, 67, 116, 130 
points are detected to be leverage points, potentially. These
points exceed the boundary of 0.107, as provided below."

"Following data points are determined
to be potential influential points based on DFFITs value: 2*sqrt(p/n);
which is equal to .4640955: 52, 59, 83, 119, 127.
While, based on COVRATIO, following data points are determined:
24, 37, 52, 54, 59, 66, 67, 83, 116, 119, 127. These points
exceeded the lower and upper bounds of COVRATIO, which are 
provided as: 0.8384615 and 1.161538. Since n>3p in the 
scope of this analysis, COVRATIO results can be used."

"DFFITs and DFBETAs results do not provide out of boundary outcomes,
as provided below."

#ols_plot_diagnostics(model)

dffits.cutoff <- 2*sqrt(7/130)
cov.ratio.upper <- 1 + 3*7/130
cov.ratio.lower <- 1 - 3*7/130
cov.ratio.lower

#All potentially influential points
summary(influence.measures(model.boxcox))
sort(hatvalues(model.boxcox))

model.bc.hat.values <- hatvalues(model.boxcox)

boundary.2pn <- (2*7)/130
boundary.2pn

potential.leverage.points <- 
  model.bc.hat.values[model.bc.hat.values>boundary.2pn]

'Potential leverage data points are detected based on 2p/n.'
potential.leverage.points

'All Possible Regressions Analysis'

'When all possible models are analyzed, the model including
all candidate terms is the most successful one, with a Mallows Cp
value of 7, R^2 of 0.94 and adjusted R^2 of 0.937.'

all.possible.models <- ols_step_all_possible(model.boxcox)
all.possible.models

model.twoway.int <- lm(y ~ .^2 - x5.1:x5.2, data = df)
summary(model.twoway.int)

all.possible.models.twoway <- ols_step_all_possible(model.twoway.int, print_plot = F)
all.possible.models.twoway

all.possible.models[which (all.possible.models$cp == min(all.possible.models$cp)),]
all.possible.models[which (all.possible.models$adjr == max(all.possible.models$adjr)),]

'Question b: MultiCollinearity Analysis'

df.multicol <- df
factor(df.multicol$x5)
df.multicol$x5 <- factor(df.multicol$x5)

model.sixway.int <- lm(y ~ .^5, data = df.multicol)
summary(model.sixway.int)

coll.diag.model.twoway <- ols_coll_diag(model.twoway.int)
coll.diag.model.twoway

coll.diag.model.sixway <- ols_coll_diag(model.sixway.int)
vif.df.coll.diag.model.sixway <- coll.diag.model.sixway$vif_t
eig_cindex.df.coll.diag.model.sixway <- coll.diag.model.sixway$eig_cindex
coll.diag.model.sixway
coll.diag.model.twoway <- ols_coll_diag(model.twoway.int)
vif.df.coll.diag.model.twoway <- coll.diag.model.twoway$vif_t
eig_cindex.df.coll.diag.model.twoway <- coll.diag.model.twoway$eig_cindex

coll.diag.model <- ols_coll_diag(model.boxcox)
coll.diag.model$vif_t
coll.diag.model$eig_cindex
coll.diag.model

vif.df.coll.diag.model.twoway

'Above, two way interaction terms and six ways interaction terms
are included in multicollinearity analysis. Based on condition indices
larger than 1000, numerous near-linear dependencies are clearly present. Also,
when VIF numbers for each regressor are examined, extremely large VIF
numbers are observed, especially in interaction terms. It can be expected
to have high VIF numbers for interaction terms. While, the book (Peck et al. 2012)
suggests that VIFs greater than 5-10 indicate poor prediction due to multicollinearity. It can
be clearly showed that almost all values exceed this limits, in both two way
and six way interaction terms included model analyses. Different approaches
can be taken to overcome issues presented by multicollinearity in the data.'

'Specificcally, x1 and x2 carry highest VIF values. x1 and x2 can
be mentioned to have severe multicollinearity issue as the result of the analysis.'

'c'

'Ridge Regression with all Regressors'


# Setting the range of lambda values
lambda_seq <- 10^seq(2, -2, by = -.001)

ridge.model <- lm.ridge(((y^lambda-1)/lambda) ~ ., 
                        data = df,
                        lambda = lambda_seq)

model.res.ridge <- resid(ridge.model)

matplot(lambda_seq, t(ridge.model$coef), type = 'l')
plot(ridge.model)
MASS::select(ridge.model)

matplot(ridge.model$lambda, coef(ridge.model)[, -1],
        type = "l", xlab = "Lambda", 
        ylab = "Coefficients"
)
text(coef(ridge.model)[1,-1], colnames(df)[2:7])

plot(ridge.model$lambda, 
     ridge.model$GCV, type = "l", col = "darkorange", 
     ylab = "GCV", xlab = "Lambda", lwd = 3)

chosen.l <- ridge.model$lambda[which.min(ridge.model$GCV)]

#pred

predicted = ridge.model$ym +
  scale(df.int[,-18], center = ridge.model$xm, 
        scale = ridge.model$scales) %*%
  ridge.model$coef[, which.min(ridge.model$GCV)]

plot(predicted, model.res.ridge, 
     main="Ridge Model: Residuals vs. Fitted Values") 

ssres.ridge <- (sum((predicted - df.int$y)^2))
ssres.ridge
sst.ridge <- sum((df.int$y - mean(df.int$y))^2)
sst.ridge
ssr.ridge <- sst.ridge - ssres.ridge
ssr.ridge

r.squared.ridge <- ssr.ridge/sst.ridge
adj.r.squared.ridge <- 1 - 
  ((ssres.ridge/(nrow(df.int)-ncol(df.int)-1))/(sst.ridge/(nrow(df.int)-1)))

adj.r.squared.ridge.org <- adj.r.squared.ridge
r.squared.ridge.org <- r.squared.ridge
msres.ridge <- ssres.ridge/(nrow(df)-17)
msres.ridge.org <- msres.ridge

df.int <- data.frame(model.matrix
                     (~(x1+x2+x3+x4.square)^5,df))

df.int <- subset(df.int, select = -c(X.Intercept.))
df.int$x5.1 <- df$x5.1
df.int$x5.2 <- df$x5.2
df.int$y <- ((df$y^lambda-1)/lambda)

#Ridge Regression with all interaction terms

lambda_seq <- 10^seq(2, -2, by = -.001)

lambda_seq

ridge.model <- lm.ridge(y ~ ., 
                        data = df.int,
                        lambda = lambda_seq)

matplot(lambda_seq, t(ridge.model$coef), type = 'l')
plot(ridge.model)
MASS::select(ridge.model)

matplot(ridge.model$lambda, coef(ridge.model)[, -1],
        type = "l", xlab = "Lambda", 
        ylab = "Coefficients"
)

#text(coef(ridge.model)[1,-1], colnames(df)[2:7])

plot(ridge.model$lambda, 
     ridge.model$GCV, type = "l", col = "darkorange", 
     ylab = "GCV", xlab = "Lambda", lwd = 3)

chosen.l <- ridge.model$lambda[which.min(ridge.model$GCV)]
chosen.l
#results

#pred

predicted = ridge.model$ym +
  scale(df.int[,-18], center = ridge.model$xm, 
        scale = ridge.model$scales) %*%
  ridge.model$coef[, which.min(ridge.model$GCV)]

ssres.ridge <- sum((predicted - df.int$y)^2)
ssres.ridge
sst.ridge <- sum((df.int$y - mean(df.int$y))^2)
sst.ridge
ssr.ridge <- sst.ridge - ssres.ridge
ssr.ridge

r.squared.ridge <- ssr.ridge/sst.ridge
adj.r.squared.ridge <- 1 - 
  ((ssres.ridge/(nrow(df.int)-ncol(df.int)-1))/(sst.ridge/(nrow(df.int)-1)))

adj.r.squared.ridge
r.squared.ridge
msres.ridge <- ssres.ridge/(nrow(df)-17)
msres.ridge

rearg_results_matrix <- matrix(c(r.squared.ridge,
                                 adj.r.squared.ridge,
                                 msres.ridge), 
                               nrow = 1, ncol = 3, byrow = T)

colnames(rearg_results_matrix) <- c("r.squared.ridge", 
                                    "adj.r.squared.ridge",
                                    "msres.ridge")

rearg_results_matrix

rearg_results_matrix_org <- matrix(c(r.squared.ridge.org,
                                     adj.r.squared.ridge.org,
                                     msres.ridge.org), 
                                   nrow = 1, ncol = 3, byrow = T)

colnames(rearg_results_matrix_org) <- c("r.squared.ridge.org", 
                                        "adj.r.squared.ridge.org",
                                        "msres.ridge.org")

rearg_results_matrix_org

'For the Ridge regression with full model,
including all the interaction terms, results matrix is provided.'

'After that, a table with all results for original data is provided. Clearly, 
data including all interaction terms achieve better result when compared
to original dataset.'

'Factored Model; (Number of Columns - 1) Interactions Included Full Model'

'In this part, a ridge regression on full model with 47 variables is presented.'
#df.factor.v <- subset(df.factor.v, select = -c(y_predicted))
#df.factor.v <- subset(df.factor.v, select = -c(y_predicted))

#valid.df <- subset(valid.df, select = -c(y_predicted))

df.factor <- train.df
df.factor.v <- valid.df

df.factor$x5 <- factor(df.factor$x5)
df.factor.v$x5 <- factor(df.factor.v$x5)

df.factor
lambda <- 0.7878
#arranging box-cox version of the response variable
df.factor$y <- ((df.factor$y^lambda-1)/lambda)
df.factor.v$y <- ((df.factor.v$y^lambda-1)/lambda)

df.x <- df.factor 
df.x.v <- df.factor.v 

df.x <- subset(df.x, select = -c(y))
df.x.v <- subset(df.x.v, select = -c(y))

#creating the interaction full dataset
df.x <- data.frame(model.matrix
                   (~(.)^2,df.x))
df.x.v <- data.frame(model.matrix
                   (~(.)^2,df.x.v))

df.x <- subset(df.x, select = -c(X.Intercept.))
df.x.v <- subset(df.x.v, select = -c(X.Intercept.))

lambda_seq <- 10^seq(2, -2, by = -.001)

lambda_seq

df.x$y <- df.factor$y
df.x.v$y <- df.factor.v$y

#running the ridge model with full model
ridge.model.x <- lm.ridge(y ~ ., 
                          data = df.x,
                          lambda = lambda_seq)

ridge.model.x
View(df.x)
summary(ridge.model.x)

df.x_y <- subset(df.x, select = -c(y))
df.x_y.v <- subset(df.x.v, select = -c(y))

#predicting the response variable based on ridge coefficients

summary(ridge.model.x)

ridge.model.x$coef[, which.min(ridge.model.x$GCV)]
?scale

ridge.model.x$ym

predicted.x = ridge.model.x$ym +
  scale(df.x_y.v, center = ridge.model.x$xm, 
        scale = ridge.model.x$scales) %*%
  ridge.model.x$coef[, which.min(ridge.model.x$GCV)]

df.f <- subset(df.x, select = -c(y))
df.f <- subset(df.x, select = -c(y))

data.trial <- data.frame(scale(df.f, center = ridge.model.x$xm, 
      scale = ridge.model.x$scales))

data.trial$y <- df.x$y

data.trial <- data.frame(scale(df.f, center = ridge.model.x$xm, 
                               scale = ridge.model.x$scales))

model.final <- lm(y ~ ., data = data.trial)
summary(model.final)
anova(model.final)



ridge.model.x$xm

ridge.model.x$coef[, which.min(ridge.model.x$GCV)]







#valid.df$y_predicted <- predicted.x

final.table <- df.x_y.v

final.table$y <- df.factor.v$y
final.table$y_predicted <- predicted.x

final.table

predicted.x
ridge.model.x$ym

plot(predicted.x, resid(ridge.model.x), 
     main="Ridge Model: Residuals vs. Fitted Values") 

#ssres, sst and ssr calculations
ssres.ridge.x <- sum((predicted.x - df.x.v$y)^2)
ssres.ridge.x
sst.ridge.x <- sum((df.x.v$y - mean(df.x.v$y))^2)
sst.ridge.x
ssr.ridge.x <- sst.ridge.x - ssres.ridge.x
ssr.ridge.x

#R^2 calculation
r.squared.ridge.x <- ssr.ridge.x/sst.ridge.x

#Adjusted R^2 calculation
adj.r.squared.ridge.x <- 1 - 
  ((ssres.ridge.x/(nrow(df.x.v)-ncol(df.x.v)-1))/(sst.ridge.x/(nrow(df.x.v)-1)))

#Presenting Adjusted R^2 and R^2
adj.r.squared.ridge.x
r.squared.ridge.x

#MSRES Presentation
msres.ridge.x <- ssres.ridge.x/(nrow(df.x)-ncol(df.x))
msres.ridge.x

rearg_results_matrix_full <- matrix(c(r.squared.ridge.x,
                                      adj.r.squared.ridge.x,
                                      msres.ridge.x), 
                                    nrow = 1, ncol = 3, byrow = T)

colnames(rearg_results_matrix_full) <- c("r.squared.ridge.org", 
                                         "adj.r.squared.ridge.org",
                                         "msres.ridge.org")

#Full Model Results of Ridge Regression
rearg_results_matrix_full

'd. PCA Regression'

df.pca <- train.df
df.pca.valid <- valid.df

df.pca[,6] <- as.factor(df.pca[,6])
df.pca.valid[,6] <- as.factor(df.pca.valid[,6])

#arranging box-cox version of the response variable
df.pca$y <- ((df.pca$y^lambda-1)/lambda)
df.pca.valid$y <- ((df.pca.valid$y^lambda-1)/lambda)

model.pcr <- pcr(y~.^5, data = df.pca, scale = TRUE)
summary(model.pcr)

r.store.pca <- 0
adj.r.store.pca <- 0
msres.store.pca <- 0

for (i in 1:47){
  predicted.pcr = predict(model.pcr,df.pca.valid,ncomp = i)
  
  #ssres, sst and ssr calculations
  ssres.pcr <- sum((predicted.pcr - df.pca.valid$y)^2)
  ssres.pcr
  sst.pcr <- sum((df.pca.valid$y - mean(df.pca.valid$y))^2)
  #sst.pcr
  ssr.pcr <- sst.pcr - ssres.pcr
  #ssr.pcr
  
  #R^2 calculation
  r.squared.pcr <- ssr.pcr/sst.pcr
  r.store.pca[i] <- r.squared.pcr
  #Adjusted R^2 calculation
  adj.r.squared.pcr <- 1 - 
    ((ssres.pcr/(nrow(df.pca.valid)-i))/(sst.pcr/(nrow(df.pca.valid)-1)))
  
  adj.r.store.pca[i] <- adj.r.squared.pcr
  #Presenting Adjusted R^2 and R^2
  #adj.r.squared.ridge.pcr
  #r.squared.ridge.pcr
  
  #MSRES Presentation
  msres.pcr <- ssres.pcr/(nrow(df.pca)-i)
  #msres.ridge.pca
  msres.store.pca[i] <- msres.pcr
}

msres.store.pca
r.store.pca
adj.r.store.pca

plot(1:47, msres.store.pca,
     type="b", pch = 19, 
     xlab="msres.store.pca",
     ylab="#PCA",
     main = "MSRes Values for # of Components")

plot(1:47, r.store.pca,
     type="b", pch = 19, 
     xlab="r.store.pca",
     ylab="#PCA",
     main = "R^2 Values for # of Components")

#EigenValue Calculation

df.eigen <- df

df.eigen <- subset(df.eigen, select = -c(y))

df.eigen$x5 <- factor(df.eigen$x5)

df.eigen <- data.frame(model.matrix
                       (~(.)^5,df.eigen))

df.eigen <- subset(df.eigen, select = -c(X.Intercept.))

pr.eigen <- prcomp(df.eigen, center = T, scale = T)

cor.eigen <- cor(df.eigen)
#cor.train.std <- cor(train.pred.std)

eigen.pca <- eigen(cor.eigen)
#eigen.train.std <- eigen(cor.train.std)

eigen.pca$sdev^2

'Eigenvectors can be presented as:'
eigen.pca$vectors

eigen.pca$values

summary(pr.eigen)

#DECISION 

predicted.pca.final = predict(model.pcr,df.pca.valid,ncomp = 6)

plot(predicted.pca.final, resid(model.pcr), 
     main = "PCR Model: Residuals vs. Fitted Values") 

#ssres, sst and ssr calculations
ssres.pcr <- sum((predicted.pca.final - df.pca.valid$y)^2)
ssres.pcr
sst.pcr <- sum((df.pca.valid$y - mean(df.pca.valid$y))^2)
#sst.pcr
ssr.pcr <- sst.pcr - ssres.pcr
#ssr.pcr

#R^2 calculation
r.squared.pcr <- ssr.pcr/sst.pcr

#Adjusted R^2 calculation
adj.r.squared.pcr <- 1 - 
  ((ssres.pcr/(nrow(df.pca.valid)-6))/(sst.pcr/(nrow(df.pca.valid)-1)))

#MSRES Presentation
msres.pcr <- ssres.pcr/(nrow(df.pca.valid)-6)
#msres.ridge.pca

adj.r.squared.pcr
r.squared.pcr
msres.pcr

pca_results_matrix <- matrix(c(r.squared.pcr,
                               adj.r.squared.pcr,
                               msres.pcr), 
                             nrow = 1, ncol = 3, byrow = T)

colnames(pca_results_matrix) <- c("r.squared.pcr", 
                                  "adj.r.squared.pcr",
                                  "msres.pcr")

#PCA Results for 6 Components from the full dataset
pca_results_matrix

#E)

#trying with 0.7 size
train.index <- sample(c(1:dim(df)[1]), dim(df)[1]*0.7)

#df$y <- (df$y^lambda-1)/lambda

train.df <- df[train.index, ]
valid.df <- df[-train.index, ]

model.a <- lm(y ~ ., data = train.df)
summary(model.a)

predicted.pca.final <- predict(model.a, valid.df)


#ssres, sst and ssr calculations
ssres.pcr <- sum((predicted.pca.final - valid.df$y)^2)
ssres.pcr
sst.pcr <- sum((valid.df$y - mean(valid.df$y))^2)
#sst.pcr
ssr.pcr <- sst.pcr - ssres.pcr
#ssr.pcr

#R^2 calculation
r.squared.pcr <- ssr.pcr/sst.pcr

#Adjusted R^2 calculation
adj.r.squared.pcr <- 1 - 
  ((ssres.pcr/(nrow(valid.df)-6))/(sst.pcr/(nrow(valid.df)-1)))

#MSRES Presentation
msres.pcr <- ssres.pcr/(nrow(valid.df)-6)
#msres.ridge.pca

adj.r.squared.pcr
r.squared.pcr
msres.pcr

pca_results_matrix <- matrix(c(r.squared.pcr,
                               adj.r.squared.pcr,
                               msres.pcr), 
                             nrow = 1, ncol = 3, byrow = T)

colnames(pca_results_matrix) <- c("r.squared.pcr", 
                                  "adj.r.squared.pcr",
                                  "msres.pcr")

#PCA Results for 6 Components from the full dataset
pca_results_matrix









#model.c

df.x <- train.df

df.x <- subset(df.x, select = -c(y))

df.x <- data.frame(model.matrix
                   (~(.)^5,df.x))

df.x <- subset(df.x, select = -c(X.Intercept.))

lambda_seq <- 10^seq(2, -2, by = -.001)

lambda_seq

df.x$y <- train.df$y

#running the ridge model with full model
ridge.model.x <- lm.ridge(y ~ ., 
                          data = df.x,
                          lambda = lambda_seq)

ridge.model.x
#View(df.x)
summary(ridge.model.x)

df.x_y <- subset(df.x, select = -c(y))

#predicting the response variable based on ridge coefficients

ridge.model.x$ym

ridge.model.x$coef[, which.min(ridge.model.x$GCV)]





df.x <- test.df

df.x <- subset(df.x, select = -c(y))

df.x <- data.frame(model.matrix
                   (~(.)^5,df.x))

df.x <- subset(df.x, select = -c(X.Intercept.))

lambda_seq <- 10^seq(2, -2, by = -.001)

lambda_seq

df.x$y <- test.df$y

tester.df <- df.x

predicted.x




predicted.x = ridge.model.x$ym +
  scale(df.x_y, center = ridge.model.x$xm, 
        scale = ridge.model.x$scales) %*%
  ridge.model.x$coef[, which.min(ridge.model.x$GCV)]

plot(predicted.x, resid(ridge.model.x), 
     main="Ridge Model: Residuals vs. Fitted Values") 

#ssres, sst and ssr calculations
ssres.ridge.x <- sum((predicted.x - df.x$y)^2)
ssres.ridge.x
sst.ridge.x <- sum((df.x$y - mean(df.x$y))^2)
sst.ridge.x
ssr.ridge.x <- sst.ridge.x - ssres.ridge.x
ssr.ridge.x

#R^2 calculation
r.squared.ridge.x <- ssr.ridge.x/sst.ridge.x

#Adjusted R^2 calculation
adj.r.squared.ridge.x <- 1 - 
  ((ssres.ridge.x/(nrow(df.x)-ncol(df.x)-1))/(sst.ridge.x/(nrow(df.x)-1)))

#Presenting Adjusted R^2 and R^2
adj.r.squared.ridge.x
r.squared.ridge.x

#MSRES Presentation
msres.ridge.x <- ssres.ridge.x/(nrow(df.x)-ncol(df.x))
msres.ridge.x

rearg_results_matrix_full <- matrix(c(r.squared.ridge.x,
                                      adj.r.squared.ridge.x,
                                      msres.ridge.x), 
                                    nrow = 1, ncol = 3, byrow = T)

colnames(rearg_results_matrix_full) <- c("r.squared.ridge.org", 
                                         "adj.r.squared.ridge.org",
                                         "msres.ridge.org")









