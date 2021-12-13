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


# ---- exercise_3
lasso.cv <- cv.glmnet(x = as.matrix(browser_2006[!names(browser_2006) %in% 
                                                    c("id", "spend")]), 
                      y = browser_2006$spend,
                      type.measure = "mse", 
                      family = "gaussian", 
                      nfolds = 5, 
                      alpha = 1)

coef.lasso.cv <- coef(lasso.cv, s = "lambda.min")
(best.lin.pred <- data.frame(name = coef.lasso.cv@Dimnames[[1]][coef.lasso.cv@i + 1], 
                   coefficient = coef.lasso.cv@x) %>% 
   .[order(abs(.$coefficient), decreasing = T),] %>%
   .[1:2,])
