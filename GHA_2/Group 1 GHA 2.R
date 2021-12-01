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

load("GHA/student-mat-train.RData")
load("GHA/student-mat-test.RData")


# ---- exercise_1
(n_obs_train <- nrow(train))
(n_obs_test <- nrow(test))


# ---- exercise_2
(avg_grade <- mean(train$G3))
(min_grade <- min(train$G3))
(max_grade <- max(train$G3))
