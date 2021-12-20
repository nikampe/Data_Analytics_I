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
(m_drug <- nrow(drugs[drugs$Gender=="male" & drugs$Soft_Drug==T,])/nrow(drugs))
