# Descripción: Association algorithms and Market Basket analysis with C3T4
#
# Autor: Roberto Alejandro Leyva Márquez
#
# Fecha: 2022-08-04
##########################D#####################################################

library(readr)


# had to install arule it with the url
# urlPackage <- "https://cran.microsoft.com/snapshot/2020-07-01/bin/macosx/contrib/4.0/arules_1.6-6.tgz"
# install.packages(urlPackage, repos=NULL, type="source") 
# urlPackage <- "https://cran.microsoft.com/snapshot/2020-07-01/bin/macosx/contrib/4.0/arulesViz_1.3-3.tgz"
# install.packages(urlPackage, repos=NULL, type="source") 

library(arules)
library(arulesViz)

data1 <- read.transactions("R Data Sets/ElectronidexTransactions2017/ElectronidexTransactions2017.csv", format = "basket", sep=",")#, rm.duplicates=TRUE)
data1
summary(data1)
# Total items purchased: 9835 transactions X 125 items X density 0.03506172
# Total items purchased: 43104 items purchased
# density is the total number of items that are purchased divided by 
# the total number of possible items in that matrix
# There were 2163 transactions of 1 item, 1647 transactions of 2 items, ... ,
# and 1 transaction of 30 items.
# Most customers buy a small number of items in each transaction.
# The distribution of the data is right skewed. Look at the mean, median, etc.

inspect(data1)
length(data1) # number of transactions
size(data1) # Number of items per transaction
LIST(data1)# Gives you a list of the items per transaction by conversion.
itemLabels(data1) # the labels that the items have


freq_plot <- itemFrequencyPlot(data1, topN=20, type='absolute') 
# You can see the most frequent items are the iMac, HP Laptop,
# CYBERPOWER Gamer Desktop, Apple Earpods, and Apple MacBook Air
# Customers purchase 1 item per transaction the most.
# Customers purchase an average of 4.383 items per transaction

image(sample(data1, 100)) # x- axis are the 125 possible items
                          # y - axis are the number of transactions

# Rules ######################################################################
rules <- apriori(data1, parameter = list(supp=0.001, conf=0.8))
rules_conf <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules_conf)
# The number of rules is 635.
# The distribution of rules by length: a set of 4 items has the most rules (303).
# Look at quality of measures.
# Number of data mined: 9,835 (the number of transactions).

inspect(rules_conf[1:10])
# 100% of customers who bought Brother Printer and Halter Acrylic Monitor Stand
# also bought iMac. The relationship cannot be the other way around with the
# metric confidence. Lift sees both directions of the relationship 
# as the same one.

betterRulesConf <- rules_conf[!is.redundant(rules_conf)]
inspect(betterRulesConf[1:10])

topRules <- rules_conf[1:10]
plot(topRules)
plot(topRules, method = "graph")
plot(topRules, method = "grouped")

top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(subset(top.lift), 10))

iMacRules <- subset(top.lift, items %in% "iMac")
inspect(iMacRules[1:10])

HPLaptopRules <- subset(top.lift, items %in% "HP Laptop")
inspect(HPLaptopRules[1:10])

bottom.lift <- sort(rules, decreasing = FALSE, na.last = NA, by = "lift")
# Redundant rules ######################################################################
is.redundant(top.lift)

inspect(top.lift[is.redundant(top.lift)])

betterTopLift <- top.lift[!is.redundant(top.lift)]

inspect(betterTopLift)
summary(betterTopLift)
# 591 rules
# a set of 4 items has the most rules (302).

inspect(betterTopLift[1:10])

plot(betterTopLift[1:10], method="graph", control=list(type="items")) 
plot(betterTopLift[1:200])

betBottomLift <- bottom.lift[!is.redundant(bottom.lift)]
inspect(betBottomLift[1:10])
