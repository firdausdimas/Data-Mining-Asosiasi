library(arules)
library(arulesViz)
library(ggplot2)
library(gridExtra)
library(knitr)
library(tidyverse)
library(readr)
BreadBasket_ <- read_csv("DATA SCIENCE APP/Project Dwnld/DATA ANALYST/Databases/CSV_saved/Market Basket Analyst/BreadBasket_DMS.csv")
View(BreadBasket_)

trans <- read.transactions("DATA SCIENCE APP/Project Dwnld/DATA ANALYST/Databases/CSV_saved/Market Basket Analyst/BreadBasket_DMS.csv"
                           , format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)
trans
summary(trans)
glimpse(trans)
str(trans)

itemFrequencyPlot(trans, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

itemFrequencyPlot(trans, topN=15, type="relative", col="lightcyan2", xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot")

# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1 <- integer(length=9)
rules_sup0.5 <- integer(length=9)

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)){
  rules_sup10[i] <- length(apriori(trans, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)){
  rules_sup5[i] <- length(apriori(trans, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)){
  rules_sup1[i] <- length(apriori(trans, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)){
  rules_sup0.5[i] <- length(apriori(trans, parameter = list(sup = supportLevels[4], 
                                                          conf = confidenceLevels[i], target="rules")))
}

# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 10%
plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 10%") +
  theme_bw()

# Number of rules found with a support level of 5%
plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 5%") + 
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Number of rules found with a support level of 1%
plot3 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 1%") + 
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  theme_bw()

# Number of rules found with a support level of 0.5%
plot4 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 0.5%") + 
  scale_y_continuous(breaks=seq(0, 130, 20)) +
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

# Number of rules found with a support level of 10%, 5%, 1% and 0.5%
ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=rules_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())

# Apriori algorithm execution with a support level of 1% and a confidence level of 50%
rules_sup1_conf50 <- apriori(trans, parameter=list(sup = supportLevels[3], 
                                                   conf = confidenceLevels[5], target="rules"))
inspect(rules_sup1_conf50)

#-- Visualization --#

# Scatter plot
plot(rules_sup1_conf50, measure = c("support", "lift"), shading = "confidence")

# Graph (default layout)
plot(rules_sup1_conf50, method="graph")

# Grouped matrix plot
plot(rules_sup1_conf50, method="grouped")

# Apriori algorithm execution with a support level of 0.5% and a confidence level of 10%
rules_sup0.5_conf10 <- apriori(trans, parameter=list(sup=supportLevels[4], 
                                                     conf=confidenceLevels[9], target="rules"))
# Graph (circular layout)
plot(rules_sup0.5_conf10, method = "graph", 
     control = list(layout = igraph::in_circle()))

# Parallel coordinates plot
plot(rules_sup0.5_conf10, method="paracoord", control=list(reorder=TRUE))

# Grouped matrix plot
plot(rules_sup0.5_conf10, method="grouped")

# Scatter plot
plot(rules_sup0.5_conf10, measure=c("support", "lift"), shading="confidence", jitter=0)
