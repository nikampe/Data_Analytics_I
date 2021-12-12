#######################################################################################
# AUTHORS
#######################################################################################
# Cyril Janak, 16-611-287
# Jonas Husmann, 16-610-917
# Niklas Kampe, 16-611-618
# Robin Scherrer, 18-617-969

# ---- requirements
library(grf)
library(DiagrammeR)
library(glmnet)

browser_2006 <- read.csv(file = "GHA/browser_2006.csv")
browser_new <- read.csv(file = "GHA/browser_new.csv")


# ---- exercise_1