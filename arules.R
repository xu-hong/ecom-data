library(arules)
library(arulesViz)
dat <- read.csv("data.csv", sep=",")
colSums(dat!=0)
'''
for (i in 0:49) {
  item = paste("item_", i, sep="")
  dat[, item][dat[, item] == 1] <- item
  dat[, item][dat[, item] == 0] <- ""
}
'''


colnames <- names(dat)
datm <- data.matrix(dat)
#dimnames(datm) <- list(colnames)
trans <- as(datm[, -1], "transactions")
summary(trans)
image(trans)
#datm <- read.transactions("data.csv", format="basket", sep=",")
rules <- apriori(trans)
inspect(rules)


rules <- apriori(trans, parameter = list(minlen=3, supp=0.005, conf=0.8),
                #appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"), 
                control = list(verbose=F))


rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)


# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

plot(rules)
plot(rules, method="matrix", measure="lift", control=list(reorder=TRUE))
plot(rules, method="grouped", control=list(k=50))
plot(rules.sorted, method="graph", control=list(type="items"))
plot(rules.sorted, method="graph")
plot(rules.pruned, method="graph")
