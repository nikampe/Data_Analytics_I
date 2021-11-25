#######################################################################################
# AUTOHRS
#######################################################################################
# Cyril Janak, 16-611-287
# Jonas Husmann, 16-610-917
# Niklas Kampe, 16-611-618
# Robin Scherrer, 18-617-969

# ---- requirements
library(ggplot2)
load("GHA/insurance-all.RData")


# ---- exercise_1
(n_obs <- nrow(data))
(n_cov <- ncol(data) - 1)


# ---- exercise_2
(max_children <- max(data$children))
