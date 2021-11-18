################################################################################
# AUTOHRS
################################################################################
# Cyril Janak, 16-611-287
# Jonas Husmann, 16-610-917
# Niklas Kampe, 16-611-618
# Robin Scherrer, 18-617-969


################################################################################
# EXERCISE 1
################################################################################
# ---- config_part_1
library(stargazer)
library(AER)
load("aufgabenstellung/OHIE_data.RData")


# ---- solution_task_1
ols <- lm(good_health ~ insured, data = data)
stargazer(ols,
          title = "Regression von $insured$ auf $good\\_health$",
          report = ('vct*'),
          omit.table.layout = "s",
          no.space = TRUE,
          summary = TRUE,
          header = FALSE)


# ---- solution_task_1_percent
percent_gh <- table(data$good_health,data$insured)
percent_gh <- as.data.frame(cbind(c(paste(round(percent_gh[2]/(percent_gh[1]+percent_gh[2])*100, digits = 2), "%", sep = ""), 
                                    paste(round(percent_gh[1]/(percent_gh[1]+percent_gh[2])*100, digits = 2), "%", sep = "")),
                                  c(paste(round(percent_gh[4]/(percent_gh[3]+percent_gh[4])*100, digits = 2), "%", sep = ""), 
                                    paste(round(percent_gh[3]/(percent_gh[3]+percent_gh[4])*100, digits = 2), "%", sep = "")))
)
rownames(percent_gh) <- c("good_health = 1", "good_health = 0")
colnames(percent_gh) <- c("insured = 0", "insured = 1")
stargazer(percent_gh,
          title = "Gegenüberstellung von $insured$ und $good\\_health$",
          omit.table.layout = "s",
          no.space = TRUE,
          summary = FALSE,
          header = FALSE,
          label = "percent_gh")


# ---- solution_task_3_a
cross_table <- table("Angebot eines OHP" = data$winner, "Versicherungsstatus" = data$insured)
colnames(cross_table) <- c("Nicht Versichert", "Versichert")
rownames(cross_table) <- c("Kein Angebot", "Angebot")
print(cross_table)


# ---- solution_task_3_b
first_stage <- lm(insured ~ winner, data = data)
stargazer(first_stage,
          title = "Regression von $winner$ auf $insured$",
          report = ('vt*'),
          omit.table.layout = "sn",
          no.space = TRUE,
          summary = TRUE,
          header = FALSE)


################################################################################
# EXERCISE 2
################################################################################
# ---- config_part_2
library(MASS)
library(ggplot2)
library(latex2exp)
library(tidyr)

set.seed(412354329)

n = 1000

mu = c(0,0)
Sigma = matrix(c(100,-7,-7,1),2,2)
err = mvrnorm(n, mu, Sigma)
u = err[,1]
v = err[,2]

z = rbinom(n,1,0.5)

pi0 = -1
pi1 = 1
KV = pi0 + pi1 * z + v
KV = as.numeric(KV > 0)

b0 = 50
b1 = 10
G = b0 + b1 * KV + u


# ---- solution_task_6_a_calc_val
descriptive_data <- as.data.frame(cbind(G,KV,z))
stargazer(descriptive_data,
          title = "Deskriptive Statistik zu $G$, $KV$ und $z$",
          header = FALSE,
          summary.stat = c("min", "p25", "median", "p75", "max", "mean", "sd"))


# ---- solution_task_6_a_histogram
ggplot(data = descriptive_data, aes(x = G)) +
   geom_histogram(bins = 50, color = "black", fill = "white") +
   labs(y = "Anzahl Beobachtungen",
        x = "Gesundheit G")


# ---- solution_task_6_a_barchart
bar_data <- as.data.frame(cbind(c("KV","z","KV","z"),
                                c("0", "0", "1", "1"),
                                c(nrow(descriptive_data[KV == 0,]), 
                                  nrow(descriptive_data[z == 0,]), 
                                  nrow(descriptive_data[KV == 1,]), 
                                  nrow(descriptive_data[z == 1,])))
)

ggplot(bar_data, aes(y = V3, x = V2)) + 
   geom_bar(position="dodge", stat="identity", color = "black", fill = "white") +
   facet_wrap(~V1) +
   xlab("") +
   ylab("Anzahl")


# ---- solution_task_6_a_boxplot
ggplot(descriptive_data, aes(x = as.factor(KV), y = G)) +
   geom_boxplot() +
   labs(x = "Krankenversicherung KV",
        y = "Gesundheit G") +
   scale_x_discrete(breaks = c(0, 1), labels = c("Nicht Krankenversichert", "Krankenversichert"))


# ---- solution_task_6_b
cov_kv_u <- cov(KV, u)
print(paste("$Cov(KV,u)$ =", cov_kv_u, collapse = " "))


# ---- solution_task_6_c_regression
first_stage <- lm(KV ~ z, data = as.data.frame(cbind(KV,z)))
stargazer(first_stage,
          title = "Regression von $z$ auf $KV$",
          report = ('vct*'),
          omit.table.layout = "s",
          no.space = TRUE,
          summary = TRUE,
          header = FALSE)


# ---- solution_task_6_c_percent
percent_KV <- table(KV,z)
percent_KV <- as.data.frame(cbind(c(paste(round(percent_KV[2]/(percent_KV[1]+percent_KV[2])*100, digits = 2), "%", sep = ""), 
                                    paste(round(percent_KV[1]/(percent_KV[1]+percent_KV[2])*100, digits = 2), "%", sep = "")),
                                  c(paste(round(percent_KV[4]/(percent_KV[3]+percent_KV[4])*100, digits = 2), "%", sep = ""), 
                                    paste(round(percent_KV[3]/(percent_KV[3]+percent_KV[4])*100, digits = 2), "%", sep = "")))
                            )
rownames(percent_KV) <- c("Krankenversichert", "Nicht Krankenversichert")
colnames(percent_KV) <- c("z = 0", "z = 1")
stargazer(percent_KV,
          title = "Gegenüberstellung von $z$ und $KV$",
          omit.table.layout = "s",
          no.space = TRUE,
          summary = FALSE,
          header = FALSE,
          label = "percent_KV")