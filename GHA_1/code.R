#######################################################################################
# AUTHORS
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


# ---- exercise_3
pct_smokers_by_region <- aggregate(data$smoker == "yes", 
                                   by=list(region=data$region), 
                                   FUN=function(x) sum(x)/length(x))

pct_smokers_by_region[which.min(pct_smokers_by_region$x),]


# ---- exercise_4
ggplot2::ggplot(data = data, mapping = ggplot2::aes(x=age, y=charges, color=smoker)) +
   ggplot2::ggtitle(label = "Charges vs. Age") +
   ggplot2::geom_point() +
   ggplot2::scale_color_manual(values = c("green", "red"))


# ---- exercise_5
dynamic_plot <- function(data, x.variable, y.variable, color.variable) {
   ggplot2::ggplot(data = data, 
                   mapping = ggplot2::aes_string(x=x.variable, 
                                                 y=y.variable, 
                                                 color=color.variable)) +
      ggplot2::geom_point()
}

dynamic_plot(data = data, 
             x.variable = "bmi", 
             y.variable = "charges", 
             color.variable = "sex")

