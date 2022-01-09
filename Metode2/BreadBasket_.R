library(arules)
library(arulesViz)
library(repr)
library(igraph)
library(tidyverse)
library(readr)
MBA_Bread <- read_csv("DATA SCIENCE APP/Project Dwnld/DATA ANALYST/Databases/CSV_saved/Market Basket Analyst/BreadBasket_DMS.csv")
View(MBA_Bread)

trans_Bread <- read.transactions("DATA SCIENCE APP/Project Dwnld/DATA ANALYST/Databases/CSV_saved/Market Basket Analyst/BreadBasket_DMS.csv"
                                 , format="single", cols=c(3,4), sep=",", rm.duplicates=TRUE)

inspect(trans_Bread[1:10,]) #These are our first 10 transactions:
summary(trans_Bread) #Summary of our dataset:

#-- Look our top products(By Support): These are items which exist atleast in 5% of transactions. --#

options(repr.plot.width=10,repr.plot.height=8)
itemFrequencyPlot(trans_Bread, support = .05,col="lightblue",xlab="Item name", 
                  ylab="Frequency (relative)", main="Item frequency plot with 5% support")

#-- By Frequency (Absolute): Top 15 items with highest absolute frequency. --#

itemFrequencyPlot(trans_Bread, topN=15, type="absolute", col="orange2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

#-- By Frequency(relative): Top 15 items with highest relative frequency --#

itemFrequencyPlot(trans_Bread, topN=15, type="relative", col="lightgreen", xlab="Item name", 
                  ylab="Frequency (relative)", main="Relative Item Frequency Plot")

#-- Apriori Algorithm --#

BreadBasket_rules <- apriori(trans_Bread, parameter=list(support=.03, confidence=.3, minlen=2))
summary(BreadBasket_rules)

inspect(sort(BreadBasket_rules, by="lift")) #Rules with highest lift value

#-- Visualization --#

plot(BreadBasket_rules) #ScatterPlot 

plot(BreadBasket_rules, method = "grouped") #Group Matrix representation
plot(BreadBasket_rules, method = "graph") #graphical representation
plot(BreadBasket_rules, method = "graph", 
     control = list(layout = igraph::in_circle())) #Circular graphical representation
plot(BreadBasket_rules, method = "paracoord", control = list(reorder=TRUE)) #Parallel Coordinates graphical representation
