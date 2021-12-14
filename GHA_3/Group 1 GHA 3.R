#######################################################################################
# AUTHORS
#######################################################################################
# Cyril Janak, 16-611-287
# Jonas Husmann, 16-610-917
# Niklas Kampe, 16-611-618
# Robin Scherrer, 18-617-969

# ---- requirements
library(ggplot2)
library(dplyr)
library(grf)
library(DiagrammeR)
library(glmnet)

browser_2006 <- read.csv(file = "GHA/browser_2006.csv")
browser_new <- read.csv(file = "GHA/browser_new.csv")

# ---- exercise_1
mean(browser_2006$spend)


# ---- exercise_2

row_id_1297 <- browser_2006[browser_2006$id==1297,3:ncol(browser_2006)]
which.max(row_id_1297)

# ---- exercise_3
lasso.cv <- cv.glmnet(x = as.matrix(browser_2006[!names(browser_2006) %in% c("id", "spend")]), 
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

# ---- exercise_4
post_lasso = lm(data = browser_2006, "spend ~ officedepot.com + staples.com")
all_variables = lm(data = browser_2006, "spend ~ . - id")

predict_post_lasso = predict(post_lasso, newdata = browser_new)
predict_all_variables = predict(all_variables, newdata = browser_new)

predictions = data.frame(cbind(predict_post_lasso,predict_all_variables))
(summary(predictions))

(ggplot(data = predictions) +
  geom_point(aes(x = predict_post_lasso, y = predict_all_variables),
             size = 0.5, alpha = 0.5) +
  labs(x = "post lasso", y = "all variables") +
  theme_bw())

(correlations = cor(predictions))






