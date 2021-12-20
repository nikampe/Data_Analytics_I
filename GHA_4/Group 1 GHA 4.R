#######################################################################################
# AUTHORS
#######################################################################################
# Cyril Janak, 16-611-287
# Jonas Husmann, 16-610-917
# Niklas Kampe, 16-611-618
# Robin Scherrer, 18-617-969

# ---- requirements
library(rpart)
library(rpart.plot)

load("GHA/drugs.RData")


# ---- exercise_1
(m_s_drug <- (nrow(drugs[drugs$Gender=="male" & drugs$Soft_Drug==T,]) / 
   nrow(drugs[drugs$Gender=="male",])) |>
   {\(x) round(x*100, digits = 2)}() |>
   paste0("%"))


# ---- exercise_2
m_h_drug <- nrow(drugs[drugs$Gender=="male" & drugs$Hard_Drug==T,]) / 
   nrow(drugs[drugs$Gender=="male",])

f_h_drug <- nrow(drugs[drugs$Gender=="female" & drugs$Hard_Drug==T,]) / 
   nrow(drugs[drugs$Gender=="female",])

(diff_h_drug <- (m_h_drug - f_h_drug) |>
      {\(x) round(x*100, digits = 2)}() |>
      paste0("%"))