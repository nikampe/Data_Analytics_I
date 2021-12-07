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

OLS2 <- lm(G3 ~ . + .^2, 
            data=select(train, G3, Medu, Fedu, studytime, schoolsup, higher))

MSE_IS_OLS1 <- round(mean((train$G3 - OLS1$fitted.values)^2), digits = 4)
MSE_IS_OLS2 <- round(mean((train$G3 - OLS2$fitted.values)^2), digits = 4)
R2_IS_OLS1 <- round(summary(OLS1)$r.squared, digits = 4)
R2_IS_OLS2 <- round(summary(OLS2)$r.squared, digits = 4)
(MSE_R2_IS <- data.frame(model = c("OLS1_IS", "OLS2_IS"), 
                      MSE = c(MSE_IS_OLS1, MSE_IS_OLS2),
                      R2 = c(R2_IS_OLS1, R2_IS_OLS2)))

# ---- exercise_6
OLS3 <- lm(G3 ~ . , 
            data=select(train, G3, Medu, Fedu, studytime, schoolsup, higher, Pstatus, 
                        famrel, failures, famsup,internet))

OLS4 <- lm(G3 ~ . + .^2, 
            data=select(train, G3, Medu, Fedu, studytime, schoolsup, higher, Pstatus, 
                        famrel, failures, famsup,internet))

fit_OLS1 <- predict(OLS1, newdata = test)
fit_OLS2 <- predict(OLS2, newdata = test)
fit_OLS3 <- predict(OLS3, newdata = test)
fit_OLS4 <- predict(OLS4, newdata = test)

MSE_IS_OLS3<- round(mean((train$G3 - OLS3$fitted.values)^2), digits = 4)
MSE_IS_OLS4 <- round(mean((train$G3 - OLS4$fitted.values)^2), digits = 4)
MSE_OOS_OLS1<- round(mean((test$G3 - fit_OLS1)^2), digits = 4)
MSE_OOS_OLS2 <- round(mean((test$G3 - fit_OLS2)^2), digits = 4)
MSE_OOS_OLS3<- round(mean((test$G3 - fit_OLS3)^2), digits = 4)
MSE_OOS_OLS4 <- round(mean((test$G3 - fit_OLS4)^2), digits = 4)
R2_IS_OLS3 <- round(summary(OLS3)$r.squared, digits = 4)
R2_IS_OLS4 <- round(summary(OLS4)$r.squared, digits = 4)
R2_OOS_OLS1 <- "-"
R2_OOS_OLS2 <- "-"
R2_OOS_OLS3 <- "-"
R2_OOS_OLS4 <- "-"

(MSE_R2_IS_OOS <- data.frame(model = c("OLS1_IS", "OLS2_IS", "OLS3_IS", "OLS4_IS", "OLS1_OOS", "OLS2_OOS", "OLS3_OOS", "OLS4_OOS"), 
                      MSE = c(MSE_IS_OLS1, MSE_IS_OLS2, MSE_IS_OLS3, MSE_IS_OLS4, MSE_OOS_OLS1, MSE_OOS_OLS2, MSE_OOS_OLS3, MSE_OOS_OLS4),
                      R2 = c(R2_IS_OLS1, R2_IS_OLS2, R2_IS_OLS3, R2_IS_OLS4, R2_OOS_OLS1, R2_OOS_OLS2, R2_OOS_OLS3, R2_OOS_OLS4)))

(ggplot(train, aes(x=OLS1$fitted.values, y = G3)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle("In-Sample Fit Plot - Model 1"))

(ggplot(train, aes(x=OLS2$fitted.values, y = G3)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle("In-Sample Fit Plot - Model 2"))

(ggplot(train, aes(x=OLS3$fitted.values, y = G3)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle("In-Sample Fit Plot - Model 3"))

(ggplot(train, aes(x=OLS4$fitted.values, y = G3)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle("In-Sample Fit Plot - Model 4"))

(ggplot(test, aes(x=fit_OLS1, y = G3)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle("Out-of-Sample Fit Plot - Model 1"))

(ggplot(test, aes(x=fit_OLS2, y = G3)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle("Out-of-Sample Fit Plot - Model 2"))

(ggplot(test, aes(x=fit_OLS3, y = G3)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle("Out-of-Sample Fit Plot - Model 3"))

(ggplot(test, aes(x=fit_OLS4, y = G3)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle("Out-of-Sample Fit Plot - Model 4"))


