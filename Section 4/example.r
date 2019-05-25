
#@author: Romeo Kienzler
#This file is copyright protected - do not redistribute


library(arules)
trans = read.transactions(
  file=
    url(
      "https://raw.githubusercontent.com/romeokienzler/developerWorks/master/market_baskets.csv"), 
        sep = ",")
inspect(trans)
trans.matrix <- as(trans,"matrix") * 1
articles <- colnames(trans.matrix) 
buy.frequency <- 30
minimum.support <- buy.frequency/length(trans)
minimum.support
confidence <- 0.33
itemFrequencyPlot(trans, support = minimum.support)
itemFrequencyPlot(trans, topN = 10)

rules <- apriori(trans.matrix, parameter = list(supp = minimum.support, conf = confidence, target = "rules"))
rules
summary(rules)
inspect(rules)
inspect(sort(rules, by = "support")[1:10])
inspect(sort(rules, by = "confidence")[1:10])
inspect(sort(rules, by = "lift")[1:10])
rules
require("arulesViz")
plot(sort(rules, by = "confidence")[1:10], method="graph", control=list(type="items"))


eclat.itemsets <- eclat(trans.matrix, parameter = list(supp = minimum.support, maxlen = 7))
inspect(eclat.itemsets)
plot(eclat.itemsets)
