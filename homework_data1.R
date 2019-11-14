
# Importing Data
df <- read.csv(file = "data.csv",
               sep = ",",header = F,row.names = "V1")

require(ggplot2)
require(dplyr)
require(corrplot)
require(PerformanceAnalytics)
require(Hmisc)
require(olsrr)
require("car")
require("caret")
# Renaming the whole data set according to explanations
df <- df %>% rename(
  urban_pop = V3,
  late_birth = V4,
  wpc =  V5 ,
  lpc = V6 ,
  cr_death_rate = V7  
)


# Dropping unneccesary columns.
df <- df %>% select(c(-1))

# Checking for data type in the data frame
lapply(df,class)

# There are columns which seems factor but they should be 
# continous variables according to the explanation of data set.
df[,1:5] <- lapply(df[,1:5], as.double)
lapply(df,class)

# Creating a corealation matrix and computing p-values of the matrix
matris1 <-rcorr(as.matrix(df), type=c("pearson","spearman"))
matris1$P
matris1$r
corrplot(matris1$r, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

chart.Correlation(df,histogram = T)
# urban population and late_birth seems to be correlated.

###################################################################
# First we are going to see if there is multicollinearity.
# Because we observed r=0.83 correlation between "urban_pop" and "late_birth".
model1 <-  lm(cr_death_rate~urban_pop+late_birth+wpc+lpc, data=df)
summary(model1)
vif(model1)


# multicollinearity detected in "urban_pop" and "late_birth" as suspected.
# That means urban_pop and late_birth is dependent some of variables in our model.

# We'll first try ridge, lasso, elastic net methods.

# Lasso-Ridge-ElasticNet Regression #
library(glmnet)
# we need to feed data(independent and dependent seperately) 
# into glm function as a matrix.
X <- as.matrix(df[,1:4])
class(x)
y <- as.matrix(df$cr_death_rate)
class(y)

set.seed(123)
smp_size <- round(0.8 * nrow(df))
train_ind <- sample(x = nrow(df),size = smp_size,replace = F)
X_train <- X[train_ind,]
X_test <- X[-train_ind,]
y_train <- y[train_ind,]
y_test <- y[-train_ind,]

#fitting models
#ridge
alpha0.fit <- cv.glmnet(x = X_train, y = y_train, type.measure = "mse",
                        alpha = 0, family = "gaussian")
alpha0.predicted <- predict(alpha0.fit, s = alpha0.fit$lambda.1se, newx = X_test)
MSE_ridge <- mean((y_test - alpha0.predicted)^2)
MSE_ridge

#lasso
alpha1.fit <- cv.glmnet(x = X_train, y = y_train, type.measure = "mse",
                        alpha = 1, family = "gaussian")
alpha1.predicted <- predict(alpha1.fit, s = alpha1.fit$lambda.1se, newx = X_test)
MSE_lasso <- mean((y_test - alpha1.predicted)^2)
MSE_lasso

#elastic net
alpha0.5.fit <- cv.glmnet(X_train, y_train, type.measure = "mse",
                          alpha = 0.5, family = "gaussian")
alpha0.5.predicted <- predict(alpha0.5.fit, s=alpha0.5.fit$lambda.1se, newx = X_test)
MSE_elasticnet <- mean((y_test - alpha0.5.predicted)^2)
MSE_elasticnet

# we will try different alpha values to find the best elastic net model.
list.of.fits <- list()
for(i in 1:10){
  fit.name <- paste("alpha", i/10)
  list.of.fits[[fit.name]] <- 
    cv.glmnet(x = X_train, y = y_train, type.measure = "mse", alpha = i/10,
              family = "gaussian")
}

results <- data.frame()
for(i in 1:10){
  fit.name <- paste("alpha", i/10)
  predicted <- 
    predict(list.of.fits[[fit.name]], s = list.of.fits[[fit.name]]$lambda.1se, newx = X_test)
  mse <- mean((y_test - predicted)^2) 
  temp <- data.frame(alpha = i/10, mse = mse, fit.name = fit.name)
  results <- rbind(results, temp)
}
print(results)

# Seems like all models are equal. Thus, we will use ridge regression.
lambda = alpha0.fit$lambda.min
lambda
ridge.fit <- glmnet(x = X_train, y = y_train, nlambda = lambda,
                        alpha = 0, family = "gaussian",)

ridgereg_coef <- predict(alpha0.fit, type = "coef", newx = X_test)
ridgereg_coef

class(ridge.fit)

# Since glmnet package returns a different object, we are going to
# try PCR (Principal Component Regression)

# Principal Component Analysis (PCA)
require("pls")
pcr_model <- pcr(cr_death_rate~., data = df, scale = TRUE, validation = "CV")
summary(pcr_model)

# Plotting the root mean squared error
validationplot(pcr_model)

# Plotting the cross validation MSE
validationplot(pcr_model, val.type="MSEP")

# Plot the R2
validationplot(pcr_model, val.type = "R2")

# We will use first 2 principal components to predict.

# Splitting data to training and test sets.
set.seed(123)
smp_size <- round(0.8 * nrow(df))
train_ind <- sample(x = nrow(df),size = smp_size,replace = F)
train <- df[train_ind,]
test <- df[-train_ind,]

# Ftting the PCR.
pcr_model <- pcr(cr_death_rate~., data = train,scale =TRUE, validation = "CV")
pcr_pred <- predict(pcr_model, test, ncomp = 3)
pcr_mse <- mean((pcr_pred - y_test)^2)
pcr_mse

# PCR MSE is lower than ridge regression. 
# Also we got rid of multicollinearity.
summary(pcr_model)

# We are going to drop variable and run regression anyways.
# Because results are not interpretable as the way we wanted.

#selecting best subset.
attach(df)
modelm <- lm(cr_death_rate~urban_pop+wpc+lpc, data=df)
k = ols_step_all_possible(modelm)
summary(k)
plot(k)
# selecting model 4th and 7th model
# 4th model's variables are "urban_pop" and "lpc"	

model2 <-  lm(cr_death_rate~urban_pop+lpc, data=df)
summary(model2)

# lpc variable seems insignificant. We will try 7th model.
# 4th model's variables are "urban_pop", "lpc" and "wpc".
model3 <-  lm(cr_death_rate~urban_pop+lpc+wpc, data=df)
summary(model3)

# both wpc and lpc seems insignificant. We are going to run simple linear reg.
model4 <- lm(cr_death_rate~urban_pop, data=df)

library(ggplot2)
ggplot(data = df)+
  geom_point(mapping = aes(x = urban_pop, y = cr_death_rate, size = wpc)) +
  geom_abline(slope = model4$coefficients[2], intercept = model4$coefficients[1], col = "red")
plot(urban_pop, cr_death_rate)
identify(urban_pop, cr_death_rate)
outs <- which(df$cr_death_rate < 10 & df$urban_pop > 50)
# df[c(12,20,22,36),]
summary(model4)

# parameter seems significant. We are going to run diagnostics.
par(mfrow=c(2,2))
plot(model4)
#both 20 and 22 seems problematic(bad leverage), also 36 is suspicious.
# Normality Assumption.
attach(df)
par(mfrow=c(1,1))
hist(model4$residuals)
shapiro.test(model4$residuals)
ks.test(model4$residuals, y = "pnorm")
# Residuals are not distributed normally! It's left-skewed.
model4$residuals
# It's probably because of 20th and 22nd observations.
# Let's check about residuals, outliers and leverages more.

# looking for outliers.
st.res <- model4$residuals / sd(model4$residuals) 
plot(model4$fitted.values, st.res)
abline(h = 0, lty = 2)
abline(h= c(-2,2), col = "red")
identify(model4$fitted.values, st.res)
which(abs(st.res)>2)
# 20 and 22 are outliers.

# looking for bad leverage points now.
plot(hatvalues(model4), st.res)
abline(h = 0, lty = 2)
abline(h = c(-2,2), v = 2 * mean(hatvalues(model4)), col = "red")
identify(hatvalues(model4), st.res)


# calculating cooks distance.
plot(df$cr_death_rate, cooks.distance(model4))
abline(h = 4 / (length(cr_death_rate) - 2), col = "red")
identify(df$cr_death_rate, cooks.distance(model4))


# let's drop these bad leverage points.
df_drop <- df[-c(20,22,36,12),]

attach(df_drop)
model5 <- lm(cr_death_rate~urban_pop, data=df_drop)
summary(model5)
par(mfrow = c(2,2))
plot(model5)
par(mfrow = c(1,1))
hist(model5$residuals)


# checking for normality of residuals.
st.res2 <- model5$residuals / sd(model5$residuals) 
qqnorm(st.res2)
# nearly normal.
shapiro.test(st.res2)
ks.test(st.res2, y = "pnorm")
# residuals distributed normal.

# Checking for heteroscedasticity
plot(df_drop$urban_pop, st.res2)
library(lmtest)
bptest(df_drop$cr_death_rate~df_drop$urban_pop)
car::ncvTest(model5)

# I am checking if my previous model has heteroscedasticity Because
# I dropped some observations before model5.
par(mfrow = c(2,2))
plot(model4)
bptest(df$cr_death_rate~df$urban_pop)

# We can reject the null that the variance of the residuals is constant,
# thus heteroscedasticity is present.

# There is a rule-of-thumb 
# “the highest variability shouldn’t be greater than four times that of the smallest”
4 * min(abs(model5$residuals)) < max(abs(model5$residuals))


# I will use Box-Cox transformation.
BoxCox_cdr <-  caret::BoxCoxTrans(df_drop$cr_death_rate)
print(BoxCox_cdr)
# Adding transformed values to data frame.
df_drop <- cbind(df_drop, cr_death_rate_BCMod = predict(BoxCox_cdr, df_drop$cr_death_rate))

model6 <-  lm(df_drop$cr_death_rate_BCMod ~ df_drop$urban_pop, data = df_drop)
summary(model6)
bptest(model6)
par(mfrow = c(2,2))
plot(model6)


# Nothing changed. 
# We'll try to understand if there is correlation 
# between the residuals with a Durbin Watson test:
# Checking for Auto-Correlation...
dwtest(model6)
# If the statistic W is between 1 and 3, then there isn't correlation.

# No Auto-Correlation. We can use weighted regression now.
abs_res <- abs(model5$residuals)
fitted_val <- model5$fitted.values
wts <- 1/fitted(lm(abs_res ~ fitted_val))^2
model7 <- lm(df_drop$cr_death_rate~df_drop$urban_pop, weights = wts)
plot(model7)
bptest(model7)

# It's not solved again. We will try to add some observation that 
# we have excluded before.
df_drop2 <- df[-c(20,22),]

# I am going split my data.
set.seed(123)
smp_size <- round(0.9 * nrow(df_drop2))
train_ind <- sample(x = nrow(df_drop2),size = smp_size,replace = F)
train <- df_drop2[train_ind,]
test <- df_drop2[-train_ind,]

attach(train)
model8 <- lm(cr_death_rate ~ urban_pop, data = train)

# Let's check for heteroscedasticity again.
par(mfrow = c(2,2))
plot(model8)
bptest(model8)
# Heteroscedasticity solved. √

# Let's check for normality assumption.
st.res8 <- model8$residuals / sd(model8$residuals)
shapiro.test(st.res8)
ks.test(st.res8, y = "pnorm")
# Normality solved. √

# Let's check for auto-correlation.
dwtest(model8)
# No Auto-Correlation. √

# Our final model is "model8".
anova(model8)

# Finding confidence intervals for coefficients.
confint(model8,level=0.90)
# Finding condifence intervals for predictions.
pred_confint <- predict(model8, test, interval = "confidence")
test_confint <- cbind(test, pred_confint)
test_confint
# With %95 confidence we can say, y_hat is between "lwr" and "upr" intervals.

# We'll use Leave One Out Cross Validation.
train.control <- trainControl(method = "LOOCV")
model8_1 <- train(cr_death_rate ~ urban_pop, data = train, method = "lm",
               trControl = train.control)
model8_1$results
summary(model8_1)
# Our R-Squared is 0.538 means our model can explain %53.8 variability.
# Our only variable seems statistically significant.
# 1 unit difference in urban_pop will cause cr_death_rate change 0.597.
