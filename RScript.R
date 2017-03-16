# Association Rules for Market Basket Analysis (R)

library(arules)  # association rules
library(arulesViz)  # data visualization of association rules
library(RColorBrewer)  # color palettes for plots

data(Groceries)  # grocery transactions object from arules package

itemInfo(Groceries)# for finding which items are categorised as dairy produce

# show the dimensions of the transactions object
print(dim(Groceries))


# examine frequency for each item with support greater than 0.025
pdf(file="fig_market_basket_initial_item_support.pdf", 
    width = 8.5, height = 11)
itemFrequencyPlot(Groceries, support = 0.025, cex.names=0.8, xlim = c(0,0.3),
                  type = "relative", horiz = TRUE, col = "dark red", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))
dev.off()    

# explore possibilities for combining similar items
print(head(itemInfo(Groceries))) 
print(levels(itemInfo(Groceries)[["level1"]]))  # 10 levels... too few 
print(levels(itemInfo(Groceries)[["level2"]]))  # 55 distinct levels

# aggregate items using the 55 level2 levels for food categories
# to create a more meaningful set of items
groceries <- aggregate(Groceries, itemInfo(Groceries)[["level2"]])  
  

pdf(file="fig_market_basket_final_item_support.pdf", width = 8.5, height = 11)
itemFrequencyPlot(groceries, support = 0.025, cex.names=1.0, xlim = c(0,0.5),
                  type = "relative", horiz = TRUE, col = "blue", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))
dev.off()   

# selecting 201 association rules using thresholds for support=0.0343 and confidence 
rules <- apriori(groceries, 
                 parameter = list(support = 0.0343, confidence = 0.05))
print(summary(rules))  # yields 201 rules

# selecting 201 association rules using thresholds for support=0.0343 and confidence = 0.01
second.rules <- apriori(groceries, 
                        parameter = list(support = 0.0343, confidence = 0.01))
print(summary(second.rules))  # yields 206 rules

# data visualization of association rules in scatter plot
pdf(file="fig_market_basket_rules.pdf", width = 8.5, height = 8.5)
plot(rules, 
     control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])),
     shading = "lift")   
dev.off()    

# grouped matrix of rules 
pdf(file="fig_market_basket_rules_matrix.pdf", width = 8.5, height = 8.5)
plot(rules, method="grouped",   
     control=list(col = rev(brewer.pal(9, "Greens")[4:9])))
dev.off()    

# select rules with dairy produce in consequent (right-hand-side) item subsets
dairy.rules <- subset(rules, subset = rhs %pin% "dairy produce")
inspect(dairy.rules)  # 41 rules

# sort by lift and identify the top 10 rules
top.dairy.rules <- head(sort(dairy.rules, decreasing = TRUE, by = "lift"), 12)
inspect(top.dairy.rules) 

pdf(file="fig_market_basket_dairy_rules.pdf", width = 11, height = 8.5)
plot(top.dairy.rules, method="graph", 
     control=list(type="items"), 
     shading = "lift")
dev.off()  
