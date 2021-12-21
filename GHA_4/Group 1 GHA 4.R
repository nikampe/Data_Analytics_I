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
library(dplyr)

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

# ---- exercise_3
share_softdrugs_16_17 <- round((nrow(drugs[drugs$Age=="16-17 years" & drugs$Soft_Drug==T,]) /
   nrow(drugs[drugs$Age=="16-17 years",]))*100, digits = 2)
share_softdrugs_18_19 <- round((nrow(drugs[drugs$Age=="18-19 years" & drugs$Soft_Drug==T,]) /
   nrow(drugs[drugs$Age=="18-19 years",]))*100, digits = 2)
share_softdrugs_20_24 <- round((nrow(drugs[drugs$Age=="20-24 years" & drugs$Soft_Drug==T,]) /
   nrow(drugs[drugs$Age=="20-24 years",]))*100, digits = 2)

(shares_softdrugs <- data.frame(
   age = c("16-17 Years", "18-19 Years", "20-24 Years"), 
   share = c(share_softdrugs_16_17, share_softdrugs_18_19, share_softdrugs_20_24)))

# ---- exercise_4
drugs_table <- table(drugs$Earning, drugs$Soft_Drug)
chi_squared <- chisq.test(drugs_table)
(statistics <- chi_squared$statistic)
(p_value <- chi_squared$p.value)
