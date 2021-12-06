#######################################################################################
# AUTHORS
#######################################################################################
# Cyril Janak, 16-611-287
# Jonas Husmann, 16-610-917
# Niklas Kampe, 16-611-618
# Robin Scherrer, 18-617-969

# ---- requirements
library(glmnet)
library(corrplot)
library(ggplot2)
library(dplyr)

load("GHA/student-mat-train.RData")
load("GHA/student-mat-test.RData")


# ---- exercise_1
(n_obs_train <- nrow(train))
(n_obs_test <- nrow(test))


# ---- exercise_2
(avg_grade <- mean(train$G3))
(min_grade <- min(train$G3))
(max_grade <- max(train$G3))

# ---- exercise_3
(final_grade_hist <- ggplot(data=train, aes(G3)) + 
    geom_histogram(breaks=seq(2,20, by=1),
                   col="red",
                   fill="black",
                   alpha = 0.2)+
    labs(title="Histogram Final Math Grade", x="Final Math Grade", y="Count"))

# ---- exercise_5
OLS1 <- lm(G3 ~ . ,
            data=select(train, G3, Medu, Fedu, studytime, schoolsup, higher))
(summary(OLS1))

OLS2 <- lm(G3 ~ . + .^2, 
            data=select(train, G3, Medu, Fedu, studytime, schoolsup, higher))
(summary(OLS2))

MSE_IS_OLS1 <- mean((train$G3 - OLS1$fitted.values)^2)
MSE_IS_OLS2 <- mean((train$G3 - OLS2$fitted.values)^2)
(MSE_IS <- data.frame(model = c("OLS1_IS", "OLS2_IS"), 
                      MSE = c(MSE_IS_OLS1, MSE_IS_OLS2)))

# ---- exercise_6
OLS3 <- lm(G3 ~ . , 
            data=select(train, G3, Medu, Fedu, studytime, schoolsup, higher, Pstatus, 
                        famrel, failures, famsup,internet))
(summary(OLS3))

OLS4 <- lm(G3 ~ . + .^2, 
            data=select(train, G3, Medu, Fedu, studytime, schoolsup, higher, Pstatus, 
                        famrel, failures, famsup,internet))
(summary(OLS4))

MSE_IS_OLS3 <- mean((train$G3 - OLS3$fitted.values)^2)
MSE_IS_OLS4 <- mean((train$G3 - OLS4$fitted.values)^2)

fit_OLS1 <- predict(OLS1, newdata = test)
MSE_OOS_OLS1 <- mean((test$G3 - fit_OLS1)^2)

fit_OLS2 <- predict(OLS2, newdata = test)
MSE_OOS_OLS2 <- mean((test$G3 - fit_OLS2)^2)

fit_OLS3 <- predict(OLS3, newdata = test)
MSE_OOS_OLS3 <- mean((test$G3 - fit_OLS3)^2)

fit_OLS4 <- predict(OLS4, newdata = test)
MSE_OOS_OLS4 <- mean((test$G3 - fit_OLS4)^2)

(MSE_IS <- data.frame(model = c("OLS1_IS", "OLS2_IS", "OLS3_IS", "OLS4_IS"), 
                      MSE = c(MSE_IS_OLS1, MSE_IS_OLS2, 
                              MSE_IS_OLS3, MSE_IS_OLS4)))
(MSE_OOS <- data.frame(model = c("OLS1_OOS", "OLS2_OOS", "OLS3_OOS", "OLS4_OOS"), 
                       MSE = c(MSE_OOS_OLS1, MSE_OOS_OLS2, 
                               MSE_OOS_OLS3, MSE_OOS_OLS4)))

(ggplot(MSE_IS, aes(model, MSE)) +
  geom_col(color = "red", fill = 'black', alpha = 0.2) +
  ggtitle("In-Sample MSEs") + 
  xlab("") +
  theme_classic())

(ggplot(MSE_OOS, aes(model, MSE)) +
  geom_col(color = "red", fill = 'black', alpha = 0.2) +
  ggtitle("Out-Of-Sample MSEs") + 
  xlab("") +
  theme_classic())


