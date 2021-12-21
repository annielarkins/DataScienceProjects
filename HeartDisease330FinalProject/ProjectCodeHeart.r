# Final Project 2
library(tidyverse)
library(naniar)

# Load in the initial data
heart_cleveland <- read.table('/Users/annielarkins/Desktop/330/FinalProject/processed.cleveland.data', sep = ",")
heart_hungarian <- read.table('/Users/annielarkins/Desktop/330/FinalProject/processed.hungarian.data', sep = ",")
heart_va <- read.table('/Users/annielarkins/Desktop/330/FinalProject/processed.va.data', sep = ",")
heart_switzerland <- read.table('/Users/annielarkins/Desktop/330/FinalProject/processed.switzerland.data', sep = ",")

# Merge the tables and add column names
heart_df <- rbind(heart_cleveland, heart_hungarian, heart_switzerland, heart_va)
names(heart_df) <- c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs', 'restecg', 'thalach', 'exang',
                     'oldpeak', 'slope', 'ca', 'thal', 'num')

# Save the merged data to a .csv to turn in
write.csv(heart_df, 'heartDiseaseDataset.csv')

# To use from the csv instead of the four data files I combined, uncomment this line:
# heart_df <- read.csv('heartDiseaseDataset.csv')

# Take a look
heart_df %>%
  slice_tail(n=10)

# Data Cleaning

# Convert ?'s to NA
heart_df <- heart_df %>% 
  replace_with_na_all(condition = ~.x == '?')

# Count NA's by column
sapply(heart_df, function(x) sum(is.na(x)))

# Looks like over half of the observations are missing ca and thal values so I think I'll drop those columns
heart_df_clean <- subset(heart_df, select = -c(ca, thal))

# We just want to detect presence of heart disease (num = 1,2,3,4)
heart_df_clean$num <- factor(as.integer(as.logical(heart_df_clean$num)))

# Make categorical variables factors
heart_df_clean$sex <- factor(heart_df_clean$sex)
heart_df_clean$cp <- factor(heart_df_clean$cp)
heart_df_clean$fbs <- factor(heart_df_clean$fbs)
heart_df_clean$restecg <- factor(heart_df_clean$restecg)
heart_df_clean$exang <- factor(heart_df_clean$exang)
heart_df_clean$slope <- factor(heart_df_clean$slope)

# Make sure quantitative variables are properly encoded
heart_df_clean$trestbps <- as.double(heart_df_clean$trestbps)
heart_df_clean$chol <- as.double(heart_df_clean$chol)
heart_df_clean$thalach <- as.double(heart_df_clean$thalach)
heart_df_clean$oldpeak <- as.double(heart_df_clean$oldpeak)

# EDA
summary(heart_df_clean)

# Compare some factors
table(heart_df_clean$sex, heart_df_clean$num, useNA = "always")
table(heart_df_clean$cp, heart_df_clean$num, useNA = "always")
table(heart_df_clean$fbs, heart_df_clean$num, useNA = "always")
table(heart_df_clean$restecg, heart_df_clean$num, useNA = "always")
table(heart_df_clean$exang, heart_df_clean$num, useNA = "always")
table(heart_df_clean$slope, heart_df_clean$num, useNA = "always")

# Create some initial comparison plots of quantitative data
pairs(~age+trestbps+chol+thalach,data=heart_df_clean,
      main="Simple Scatterplot Matrix")

# Analysis

# Fit model
heart_out1 <- glm(num ~ ., data = heart_df_clean, family = "binomial")

# Table of estimates and standard errors
summary(heart_out1)

# Assess Assumptions
plot(heart_out1)
# Predictions v residuals plot
#   see a few outliers (441, 81, 871)
# QQ-plot of residuals (assess normality)
#   see a few outliers(441, 81, 871)
# Predictions v root abs std resids plot (constant variance)
#   kind of a weird pattern
# Leverage (wt an obs has in predicting itself) vs std resids
#   Leverage is potentially bad 
#   see outliers (861, 801, 754)

hist(resid(heart_out1))
# Residuals look fairly normally distributed

# Hypothesis Testing

# Does gender have an effect on heart disease?
# Use Chi-squared test
exp(coef(heart_out1))

# HO: Sex has no effect on heart disease
# HA: Sex has an effect on heart disease

# Train a reduced model
heart_out_reduced <- glm(num ~ . - sex, data = heart_df_clean,
                           family = binomial)
anova(heart_out_reduced, heart_out1, test = "Chisq")

# p-value < .0001

# Formal: We reject H0 in favor of HA: there is a sex effect at the .05 significance level
# (dev = 27.023, pvalue < .0001).

# Informal: There is a statistically significant sex effect on probability of heart disease
# (dev = 27.023, pvalue < .0001).
barplot(table(heart_df_clean$sex, heart_df_clean$num),
        legend = c("Female", "Male"),
        ylim = c(0, 750),
        xlab = "Has Heart Disease",
        ylab = "Number of Patients")

# Does age have an effect on heart disease?
exp(coef(heart_out1))[2]

# We estimate a 1.5% increase in odds of having heart disease for one additional year
# holding all else constant. 

# HO: Age has no effect on heart disease
# HA: Age has an effect on heart disease

# z-value = -1.675, p-value = .320873

# Formal Conclusion: We fail to detect a statistically significant age effect at a .05 significance level
# (z-value = -1.675, p-value = .320873). 

# Informal Conclusion: We fail to detect an age effect (z-value = -1.675, p-value = .320873). 

# Create a graphic for the age effect
xstar <- seq(0, 100, length = 100)
plot(xstar, coef(heart_out1)[2] * xstar, type = 'l',
     xlab = "Age",
     ylab = "Partial Log-Odds of Having Heart Disease ")

# From a probability perspective
# Demonstrate the effect of age (all others at median)
xstar <- data.frame(age = seq(0, 100, length = 100),
                    chol=223,
                    sex=factor(1, levels = c(0,1)),
                    cp = factor(4, levels = c(1,2,3,4)),
                    trestbps=130,
                    fbs=factor(0, levels = c(0,1)),
                    restecg = factor(0, levels = c(0,1,2)),
                    thalach = 140,
                    exang = factor(0, levels = c(0,1)),
                    oldpeak = 0.5,
                    slope = factor(2, levels = c(1,2,3)))
# type = response since this is a probability
plot(xstar$age, predict(heart_out1, newdata = xstar, type = "response"),
     type = 'l',
     xlab = "Age",
     ylab = "P(Heart Disease), all other factors at median")


# Does cholesterol have an effect on heart disease? 
exp(coef(heart_out1))[8]

# We estimate a 0.4 % decrease in odds of having heart disease for a one unit increase in cholesterol
# holding all else constant. 

# HO: Chol has no effect on heart disease
# HA: Chol has an effect on heart disease

# z-value = -2.582, p-value = .0098

# Formal Conclusion: We reject H0 in favor of HA: chol has an effect on heart disease  at a .05 significance level
# (z-value = -2.582, p-value = .0098). 

# Informal Conclusion: There is a statistically significant chol effect (z-value = -2.582, p-value = .0098). 

# Create a graphic for the cholesterol effect
xstar <- seq(0, 700, length = 100)
plot(xstar, coef(heart_out1)[8] * xstar, type = 'l',
     xlab = "Chol",
     ylab = "Partial Log-Odds of Having Heart Disease ")

# From a probability perspective
# Demonstrate the effect of chol (all others at median)
xstar <- data.frame(chol = seq(0, 700, length = 100),
                      age=54,
                      sex=factor(1, levels = c(0,1)),
                      cp = factor(4, levels = c(1,2,3,4)),
                      trestbps=130,
                      fbs=factor(0, levels = c(0,1)),
                      restecg = factor(0, levels = c(0,1,2)),
                      thalach = 140,
                      exang = factor(0, levels = c(0,1)),
                      oldpeak = 0.5,
                      slope = factor(2, levels = c(1,2,3)))
# type = response since this is a probability
plot(xstar$chol, predict(heart_out1, newdata = xstar, type = "response"),
     type = 'l',
     xlab = "Chol",
     ylab = "P(Heart Disease), all other factors at median")

# Does exercise induced angina have an effect on heart disease? 

# HO: Exang has no effect on heart disease
# HA: Exang has an effect on heart disease

# Train a reduced model
heart_out_reduced2 <- glm(num ~ . - exang, data = heart_df_clean,
                         family = binomial)
anova(heart_out_reduced2, heart_out1, test = "Chisq")

# deviance = 4.62
# pvalue = .03151

# Formal: We reject H0 in favor of HA: there is a exang effect at the .05 significance level
# (dev = 4.62, pvalue = .03151).

# Informal: There is a statistically significant exang effect on probability of heart disease
# (dev = 4.62, pvalue = .03151).

barplot(table(heart_df_clean$exang, heart_df_clean$num, useNA = "ifany"),
        legend = c("No Exang", "Has Exang", "Missing Value"),
        ylim = c(0, 750),
        xlab = "Has Heart Disease",
        ylab = "Number of Patients")

# Predict my mom's risk of heart disease
predict(heart_out1, newdata = data.frame(chol = 87,
                                         age=46,
                                         sex=factor(0, levels = c(0,1)),
                                         cp = factor(1, levels = c(1,2,3,4)),
                                         trestbps=130, # Not sure
                                         fbs=factor(0, levels = c(0,1)), 
                                         restecg = factor(0, levels = c(0,1,2)),
                                         thalach = 140, # Not sure
                                         exang = factor(1, levels = c(0,1)),
                                         oldpeak = 0.5, # Not sure
                                         slope = factor(2, levels = c(1,2,3))), # Not sure
        type = "response"
)

mom_logit <- predict(heart_out1, newdata = data.frame(chol = 87,
                                                      age=46,
                                                      sex=factor(0, levels = c(0,1)),
                                                      cp = factor(1, levels = c(1,2,3,4)),
                                                      trestbps=130, # Not sure
                                                      fbs=factor(0, levels = c(0,1)), 
                                                      restecg = factor(0, levels = c(0,1,2)),
                                                      thalach = 140, # Not sure
                                                      exang = factor(1, levels = c(0,1)),
                                                      oldpeak = 0.5, # Not sure
                                                      slope = factor(2, levels = c(1,2,3))),
                       type = "link", se.fit = TRUE
)

logitL <- mom_logit$fit - 2 * mom_logit$se.fit
logitU <- mom_logit$fit + 2 * mom_logit$se.fit

# Untransform to probability
mom_phatL <- 1 / (1 + exp(-logitL))
mom_phatU <- 1 / (1 + exp(-logitU))
mom_phatL
mom_phatU

# A person like my mom has heart disease with a 0.261 probability (95% CI: .088, .564).

# Create train and test datasets

# RandomForest doesn't accept rows with NA (I learned by trying this...) so we have to run with a smaller dataset
# Drop all rows with NA
heart_df_clean_tree <- heart_df_clean[rowSums(is.na(heart_df_clean[ ,])) == 0, ]

n_model <- dim(heart_df_clean_tree)[1]
# 80% train, 20% test
0.8*531  # 425
train_rows <- sample(n_model, 400)
heart_train <- heart_df_clean_tree[train_rows, ]
heart_test <- heart_df_clean_tree[-train_rows, ]

# BIC
# I chose BIC because we want the least number of columns needed since medical observations can be expensive
min_model <- glm(num ~ +1, data = heart_train, family = "binomial")
biggest <- formula(glm(num ~ ., data = heart_train, family = "binomial"))
heart_out2 <- step(min_model, scope = biggest, direction = "both", k = log(1000)) # log(n) where n is size of dataset

# Table of estimates and std errors
summary(heart_out2)

# Train and Test error rate
mean( heart_train$num != as.numeric(predict(heart_out2, type = "response") > 0.5) )
mean( heart_test$num != as.numeric(predict(heart_out2, newdata = heart_test, type = "response") > 0.5) )

# ROC
library(pROC)
# train
roc(heart_train$num, predict(heart_out2, type = "response"),
    plot = TRUE,
    print.auc = TRUE)
# test
roc(heart_test$num, predict(heart_out2, newdata = heart_test, type = "response"),
    plot = TRUE, add = TRUE, col =  "green",
    print.auc = TRUE, print.auc.y = 0.45)

# Random Forest
set.seed(2319)

library(randomForest)

names(heart_train)
names(heart_test)

# Fit model
heart_out3 <- randomForest(x = heart_train[, -12], y = heart_train$num,
                             xtest = heart_test[, -12], ytest = heart_test$num,
                             replace = TRUE,  # use bootstrap samples
                             keep.forest = TRUE,  # store trees for future predictions
                             ntree = 50, # first try was 100, after plot we see 50 is plenty
                             mtry = 2, # rule of thumb is p/3 (round down)
                             nodesize = 10 # stop growing the tree when it has nodesize obs
)
# Confirm we have sufficient number of trees
plot(heart_out3)

# Misclassification rates
heart_out3
varImpPlot(heart_out3)

# ROC for Random Forest Model
library(pROC)
# train
roc(heart_train$num, predict(heart_out3, type = "prob")[, 2],
    plot = TRUE,
    print.auc = TRUE)
# test
roc(heart_test$num, predict(heart_out3, newdata = heart_test, type = "prob")[, 2],
    plot = TRUE, add = TRUE, col =  "green",
    print.auc = TRUE, print.auc.y = 0.45)


