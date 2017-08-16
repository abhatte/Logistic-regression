# Regression with binary outcomes

# Logistic regression

#   This far we have used the `lm' function to fit our regression models.
#   `lm' is great, but limited–in particular it only fits models for
#   continuous dependent variables. For categorical dependent variables we
#   can use the `glm()' function.

#   For these models we will use a different dataset, drawn from the
#   National Health Interview Survey. From the [CDC website]:

#         The National Health Interview Survey (NHIS) has monitored
#         the health of the nation since 1957. NHIS data on a broad
#         range of health topics are collected through personal
#         household interviews. For over 50 years, the U.S. Census
#         Bureau has been the data collection agent for the National
#         Health Interview Survey. Survey results have been
#         instrumental in providing data to track health status,
#         health care access, and progress toward achieving national
#         health objectives.

#   Load the National Health Interview Survey data:
getwd()
setwd("/Users/ashwinibhatte/R Projects/log_download")

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

#   [CDC website] http://www.cdc.gov/nchs/nhis.htm

# Logistic regression example

#   Let's predict the probability of being diagnosed with hypertension
#   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=NH11, family="binomial")
coef(summary(hyp.out))

# Logistic regression coefficients


#   Generalized linear models use link functions, so raw coefficients are
#   difficult to interpret. For example, the age coefficient of .06 in the
#   previous model tells us that for every one unit increase in age, the
#   log odds of hypertension diagnosis increases by 0.06. Since most of us
#   are not used to thinking in log odds this is not too helpful!

#   One solution is to transform the coefficients to make them easier to
#   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

# Generating predicted values

#   In addition to transforming the log-odds produced by `glm' to odds, we
#   can use the `predict()' function to make direct statements about the
#   predictors in our model. For example, we can ask "How much more likely
#   is a 63 year old female to have hypertension compared to a 33 year old
#   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

#   This tells us that a 33 year old female has a 13% probability of
#   having been diagnosed with hypertension, while and 63 year old female
#   has a 48% probability of having been diagnosed.

# Packages for  computing and graphing predicted values


#   Instead of doing all this ourselves, we can use the effects package to
#   compute quantities of interest for us (cf. the Zelig package).
install.packages("effects")
library(effects)
plot(allEffects(hyp.out))

# Exercise: logistic regression


#   Use the NH11 data set that we loaded earlier.

#   1. Use glm to conduct a logistic regression to predict ever worked
#      (everwrk) using age (age_p) and marital status (r_maritl).
#   2. Predict the probability of working for each level of marital
#      status.

#   Note that the data is not perfectly clean and ready to be modeled. You
#   will need to clean up at least some of the variables before fitting
#   the model.

str(NH11)
summary(NH11)

install.packages("mice")
library(mice)


levels(NH11$everwrk) # check levels of everwrk
summary(NH11$everwrk)

NH11$everwrk[(NH11$everwrk == "7 Refused") |
               (NH11$everwrk == "8 Not ascertained") |
               (NH11$everwrk == "9 Don't know")] <- NA
summary(NH11$everwrk)
levels(NH11$r_maritl)
# Use Multivariate Imputation to replace missing values in $everwrk. 
simple <- NH11[c("age_p", "everwrk", "r_maritl")]
summary(simple)

imputed <- complete(mice(simple))
summary(imputed)
NH11$everwrk <- imputed$everwrk
NH11$r_maritl <- imputed$r_maritl
summary(NH11$everwrk)
summary(NH11$r_maritl)

# Now that the data is tidy, let's explore it with some quick plots to get a better idea
# library(ggplot2)
# ggplot(imputed, aes(everwrk)) +
#   geom_bar()
# ggplot(imputed, aes(r_maritl)) +
#   geom_bar()
# ggplot(imputed, aes(age_p)) + 
#   geom_histogram()

# Our baseline model will assume that all responses are yes. 
# Since there are 33,014 total observations, and 27,978 actual yes responses, 
# this baseline model has an accuracy of 27,978/33,014 or 84.7%. 
# Let's see if we can beat that using logistic regression.

# Since we are dealing with a large amount of data, 
# I have decided to split our data 80/20 training/testing, to try to maximize accuracy. 

install.packages("caTools")
library(caTools)

set.seed(144)
split <- sample.split(imputed$everwrk, SplitRatio = 0.80)
train <- subset(imputed, split == TRUE)
test <- subset(imputed, split == FALSE)

workLog <- glm(everwrk~r_maritl+age_p, data = train, family = binomial)
summary(workLog)

predictTest <- predict(workLog, type = "response", newdata = test)
table(test$everwrk, predictTest > 0.3)

plot(allEffects(workLog))

# accuracy of the model: 82.44%
(5282+162) / (5282+331+828+162)

# max. number of cases 5282, hence baseline model mostly predicts everwork = yes 
# accuracy of baseline model: 85% 
(5282+331) / (5282+331+828+162)

table(test$everwrk, predictTest > 0.2) # 73.16371%
(4419+412)/(4419+1194+578+412)
(4419+1194)/(4419+1194+578+412) # 85.00682%


