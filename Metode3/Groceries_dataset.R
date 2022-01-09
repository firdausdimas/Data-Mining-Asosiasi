library(arules)
library(arulesViz)
library(plyr)
library(readr)

#-- Import the dataset --#
Groceries_dataset <- read_csv("DATA SCIENCE APP/Project Dwnld/DATA ANALYST/Databases/CSV_saved/Market Basket Analyst/Groceries_dataset.csv")
View(Groceries_dataset)

class(Groceries_dataset)

#-- Data Cleaning and Exploration --#
str(Groceries_dataset)
head(Groceries_dataset)
sum(is.na(Groceries_dataset)) #Checking NA values

sorted <- Groceries_dataset[order(Groceries_dataset$Member_number),] #Convert member number to numeric
sorted$Member_number <- as.numeric(sorted$Member_number) #Convert item description to categorical format
str(sorted)

#-- Group all the items that were bought together by the same customer on the same date --#
itemList <- ddply(sorted, c("Member_number","Date"), 
                  function(df1)paste(df1$itemDescription,collapse = ","))
head(itemList,15)

#-- Remove member number and date --#
itemList$Member_number <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("itemList") #Change the Column Name

#-- Save and Convert CSV file to Basket Format --#
write.csv(itemList,"ItemList.csv", quote = FALSE, row.names = TRUE)
head(itemList)

trans_Groceries = read.transactions(file="ItemList.csv", 
                                    rm.duplicates= TRUE, 
                                    format="basket",sep=",",cols=1);
print(trans_Groceries)

#-- Remove quotes from Transaction --#
trans_Groceries@itemInfo$labels <- gsub("\"","",trans_Groceries@itemInfo$labels)

#-- Apriori Algorithm --#
basket_rules <- apriori(trans_Groceries, parameter = list(minlen = 2, 
                                                          sup = 0.001, 
                                                          conf = 0.05, 
                                                          target="rules"))
print(length(basket_rules)) #Total rules generated
summary(basket_rules) #Total rules generated

inspect(basket_rules[1:20]) #Inspecting the basket rules

#-- Visualizing the Association Rules --#
plot(basket_rules, jitter = 0)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules[1:20], method="graph")
plot(basket_rules[1:50], method="graph")
plot(basket_rules[1:20], method="paracoord")

#-- Most Frequent Products --#
itemFrequencyPlot(trans_Groceries, topN = 10)

#-- Changing hyperparameters --#
basket_rules2 <- apriori(trans_Groceries, parameter = list(minlen = 2, 
                                                           sup = 0.001, 
                                                           conf = 0.1, target="rules"))
print(length(basket_rules2))
summary(basket_rules2)

inspect(basket_rules2)

plot(basket_rules2, method = "graph")
plot(basket_rules2, method="paracoord")
