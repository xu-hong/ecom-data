setwd('/Users/xuhong/Documents/Duke/JOB HUNTING/Job Interviews/Zappos')

#######################################
########## Prepare Data ###############
#######################################
zp <- read.csv("Analytics Challenge Data 2.csv", sep=";")
zp <- subset(zp, select=-c(X, X.1, X.2))
zp$day <- as.Date(as.POSIXct(strptime(zp$day, "%y-%m-%d %H:%M")))

'
# first weed out outliers
> quantile(zp$visits, 0.95)
95% 
9260 
'

mobiles <- c("Android", "BlackBerry", "iPad", "iOS", "iPhone", "SymbianOS", "WindowsPhone")
#qplot(data=zp, x=distinct_sessions, xlim=c(0, 90000))
is_mobile <- NA
zp$is_mobile <- ifelse(zp$platform %in% mobiles, 1, 0)

if_ordered <- NA
zp$if_ordered <- ifelse(zp$orders > 0, 1, 0)


zp$customer_type <- factor(zp$new_customer, exclude=NULL)
levels(zp$customer_type) <- c("returning customer", "new customer", "neither")


zp$month <- as.numeric(factor(months(zp$day, abbreviate = T), levels=month.abb))



library(ggplot2)
library(ggthemes)
library(scales)
library(gridExtra)
library(dplyr)
library(reshape2)

#######################################
########## Sales Trend  ###############
#######################################

zp.sales <- zp %>%
  group_by(day) %>%
  summarise(
    daily_sales = sum(gross_sales, na.rm=T),
    n = n()) %>%
  arrange(day)

p.sales_byday <- ggplot(data=zp.sales, aes(x=day, y=daily_sales)) + geom_point(aes(color=daily_sales), size=3) +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) +
  ggtitle("Daily sales in 2013") + 
  theme(plot.title = element_text(size = 25)) + 
  scale_colour_continuous(name="Daily Sales",
                          breaks=c(400000, 800000, 1200000, 1600000),
                          low = "blue",
                          high = "red",
                          labels=c("0.4M", "0.8M", "1.2M", "1.6M")) +
  ylab("Daily Sales")

p.sales_weeks <- ggplot(data=zp.sales, aes(x=day, y=daily_sales)) + geom_point(aes(color=daily_sales), size=3) +
  scale_x_date(breaks = date_breaks("weeks"), limits = c(as.Date("2013-9-28"), as.Date("2013-12-31")))+
  ggtitle("Daily sales from Sept to Dec 2013")  +
  theme(plot.title = element_text(size = 25)) + 
  scale_colour_continuous(name="Daily Sales",
                          breaks=c(400000, 800000, 1200000, 1600000),
                          low = "blue",
                          high = "red",
                          labels=c("0.4M", "0.8M", "1.2M", "1.6M")) +
  ylab("Daily Sales")




#######################################
######### Average Order Size###########
#######################################


zp.order_size <- zp %>%
  group_by(month) %>%
  summarise(
    avg_order_size_by_month = median(gross_sales, na.rm=T),
    n = n()) %>%
  arrange(month)

p.order_size <- ggplot(aes(x=factor(month), y=gross_sales, group=1), data=zp) +   
  geom_point(alpha=1/5, position=position_jitter(h=0), color="orange") +
  geom_line(stat="summary", fun.y=median, color="blue") +
  #geom_line(stat="summary", fun.y=quantile, prob=0.9, color="blue", linetype=2 ) +
  geom_line(stat="summary", fun.y=mean, color="blue", linetype=2 ) +
  ylab("order size") + xlab("month") +
  #coord_trans(y="log10") + 
  coord_cartesian(ylim=c(0, 30000)) + 
  ggtitle("Order Size, 2013")

p.order_size_median <- ggplot(aes(x=factor(month), y=gross_sales, group=1), data=zp) +   
  geom_point(alpha=1/5, position=position_jitter(h=0), color="orange") +
  geom_line(stat="summary", fun.y=median, color="blue") +
  #geom_line(stat="summary", fun.y=quantile, prob=0.9, color="blue", linetype=2 ) +
  #geom_line(stat="summary", fun.y=mean, color="blue", linetype=2 ) +
  ylab("order size") + xlab("month") +
  #coord_trans(y="log10") + 
  coord_cartesian(ylim=c(0, 3000)) + 
  ggtitle("Median Order Size, 2013")

p.order_size.cust <- ggplot(aes(x=factor(month), y=gross_sales, group=customer_type), data=zp) +   
  geom_point(alpha=1/5, position=position_jitter(h=0), color="orange") +
  geom_line(stat="summary", fun.y=median, aes(color=customer_type)) +
  #geom_line(stat="summary", fun.y=quantile, prob=0.9, color="blue", linetype=2 ) +
  #geom_line(stat="summary", fun.y=mean, aes(color=customer_type), linetype=2 ) +
  ylab("order size") + xlab("month") +
  #coord_trans(y="log10") + 
  coord_cartesian(ylim=c(0, 3000)) + 
  ggtitle("Median Order Size by Customer Type, 2013")



p.order_size.mobile <- ggplot(aes(x=factor(month), y=gross_sales, group=factor(is_mobile)), data=zp) + 
  geom_line(stat="summary", fun.y=mean, aes(color=factor(is_mobile)), linetype=2) + 
  geom_point(alpha=1/5, position=position_jitter(h=0), color="orange") +
  geom_line(stat="summary", fun.y=median, aes(color=factor(is_mobile))) +
  ylab("order size") + xlab("month") +
  coord_cartesian(ylim=c(0, 40000)) + 
  ggtitle("Median Order Size by Platform, 2013") + 
  scale_colour_discrete(name="Platform Type",
                        breaks=c(0, 1),
                        labels=c("Desktop", "Mobile"))

#######################################
######### Custoner Type Dist###########
#######################################

p.cust_visits <- ggplot(data=zp, aes(x=factor(customer_type), y=visits)) + 
  geom_bar(stat="summary", fun.y=sum, fill="grey50", alpha=0.7) +
  scale_x_discrete("Customer Type") +
  theme_economist()  +
  scale_y_continuous(labels = comma) +
  ggtitle("Total number of visits by customer type")

ggplot(data=subset(zp, !is.na(gross_sales)), aes(x=factor(customer_type), y=gross_sales)) + 
  geom_bar(stat="summary", fun.y=sum, fill="grey50", alpha=0.7) +
  scale_x_discrete("Customer Type") +
  theme_economist() +
  scale_y_continuous(labels = comma) + 
  ggtitle("Total sales by customer type")

p.mobile_visits <- ggplot(data=zp, aes(x=factor(is_mobile), y=visits)) + 
  geom_bar(stat="summary", fun.y=sum, fill="grey50", alpha=0.7) +
  scale_x_discrete("Platform Type", labels=c("Desktop", "Mobile")) +
  theme_economist()  +
  scale_y_continuous(labels = comma) +
  ggtitle("Total number of visits distribution by platform")

ggplot(data=subset(zp, !is.na(gross_sales)), aes(x=factor(is_mobile), y=gross_sales)) + 
  geom_bar(stat="summary", fun.y=sum, fill="grey50", alpha=0.7) +
  scale_x_discrete("Platform Type", labels=c("Desktop", "Mobile")) +
  theme_economist() +
  scale_y_continuous(labels = comma) + 
  ggtitle("Total sales by platform")

## visits density, by customer type

p.cust_visits_dist_dens <- ggplot(data=zp, aes(visits, colour=customer_type)) + 
  geom_density(adjust=1/2) +
  theme_economist() +
  scale_y_continuous(labels = comma) + 
  scale_x_log10("Number of visits (log10)", breaks=c(10, 100, 1000, 10000)) +
  ggtitle("Density of visits by customer type")


ggplot(data=zp, aes(x=visits)) + 
  geom_density(adjust=1/2) +
  theme_economist() +
  scale_y_continuous(labels = comma) + 
  scale_x_log10("Number of visits (log10)", breaks=c(10, 100, 1000, 10000)) +
  ggtitle("Density of visits")


# mobile rate

zp.trend_pltfm_user <- zp %>%
  group_by(month) %>%
  summarise(
    mobile_rate = sum(is_mobile),
    n = n()) %>%
  arrange(month)
p.trend_pltfm <- ggplot(aes(x=factor(month), y=mobile_rate/n, group=factor(is_mobile)), data=zp.trend_pltfm_user) + 
  geom_line() + geom_point(size=3, color="grey16") +
  scale_y_continuous("% mobile unique users", labels = percent, limits=c(0,1)) +
  theme_economist() +
  ggtitle("Ratio of Mobile Unique Users, Trend in 2013")

zp.trend_pltfm <- zp %>%
  group_by(month, is_mobile) %>%
  summarise(
    pltfm_visits = sum(visits)) %>%
  arrange(month)

zp.trend_pltfm.wide <- dcast(zp.trend_pltfm, month ~ is_mobile, value.var = "pltfm_visits")
colnames(zp.trend_pltfm.wide) <- c("month", "Desktop", "Mobile")
p.trend_pltfm <- ggplot(aes(x=factor(month), y=Mobile/Desktop, group=1), data=zp.trend_pltfm.wide) + 
  geom_line() + geom_point(size=3, color="grey16") +
  theme_economist() +
  scale_y_continuous("% mobile visits", labels = percent, limits=c(0,1)) +
  ggtitle("Ratio of Mobile Visits, Trend in 2013")
  

#######################################
######### Conversion Rate   ###########
#######################################

zp.conv <- zp %>%
  group_by(day) %>%
  summarise(
    conv_rate = sum(orders)/sum(visits),
    n_order = sum(if_ordered),
    n = n()) %>%
  arrange(day)

p.conv_rate <- ggplot(aes(x=day, y=conv_rate, group=1), data=zp.conv) + 
  geom_point(size=3, color="red") + geom_smooth() +
  theme_economist() +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) +
  scale_y_continuous("daily conversion rate", labels = percent) +
  ggtitle("Conversion Rate Trend, 2013")

ggplot(aes(x=day, y=conv_rate, group=1), data=zp.conv) + 
  geom_point(size=3, color="red") + geom_smooth() +
  #theme_economist() +
  scale_x_date(breaks = date_breaks("weeks"), limits = c(as.Date("2013-9-28"), as.Date("2013-12-31"))) +
  scale_y_continuous("daily conversion rate", labels = percent) +
  ggtitle("Conversion Rate Trend, 2013")


#### order rate at different platform 

zp.order_pltfm <- zp %>%
  group_by(platform) %>%
  summarise(
    order_rate_by_pltfm = sum(if_ordered),
    is_mobile = mean(is_mobile),
    n = n()) %>%
  arrange(platform)

p.order_pltfm <- ggplot(aes(x=platform, y=order_rate_by_pltfm/n), data=zp.order_pltfm) + 
  geom_bar(stat="identity", aes(fill=factor(is_mobile))) +
  scale_y_continuous("% users making order(s)", labels = percent) + 
  ggtitle("Order Size Comparison Between Platforms") + 
  scale_fill_discrete(name="Platform Type",
                        breaks=c(0, 1),
                        labels=c("Desktop", "Mobile"))

#######################################
######### Product Page Views   ########
#######################################

p.page_v_platform_poly <- ggplot(data=zp, aes(x=product_page_views, y=..count../sum(..count..))) +
  geom_freqpoly(aes(color=factor(is_mobile)), binwidth=1) +
  scale_x_continuous(limits=c(0,200), breaks=seq(0, 200, 10)) +
  scale_y_continuous(labels=percent) + 
  theme(plot.title = element_text(size = 25)) + 
  xlab('Product page views') +
  ylab('Percentage of visitors with that product page views count') +
  scale_colour_discrete(name="Platform Type",
                        breaks=c(0, 1),
                        labels=c("Desktop", "Mobile")) +
  ggtitle("Percentage of users with that product page views")



p.page_s_platform_poly <- ggplot(data=zp, aes(x=search_page_views, y=..count../sum(..count..))) +
  geom_freqpoly(aes(color=factor(is_mobile)), binwidth=1) +
  scale_x_continuous(limits=c(0,300), breaks=seq(0, 300, 10)) +
  scale_y_continuous(labels=percent) + 
  theme(plot.title = element_text(size = 25)) + 
  xlab('Search page views') +
  ylab('Percentage of visitors with that search page views count') +
  scale_colour_discrete(name="Platform Type",
                        breaks=c(0, 1),
                        labels=c("Desktop", "Mobile")) +
  ggtitle("Percentage of users with that search page views")

with(zp, cor.test(search_page_views, product_page_views))
# cor: 0.9756209 

#######################################
######### Abandon Rate  ###############
#######################################


zp.cart_order.wide.site <- zp %>%
  filter(add_to_cart > 0) %>%
  group_by(day, site) %>%
  summarise(
    add_to_cart_freq = n(),
    order_freq = sum(if_ordered)) %>%
  arrange(day)

zp.cart_order.wide.site$abandon_rate <- 1 - zp.cart_order.wide.site$order_freq/zp.cart_order.wide.site$add_to_cart_freq
p.abandon_rate.site <- ggplot(data=zp.cart_order.wide.site, aes(x=day, y=abandon_rate, group=site, colour=factor(site))) + 
  geom_point(size=3, alpha=1/2) +
  #theme_economist() +
  scale_colour_brewer(name="site", palette="Set1") +
  scale_y_continuous(labels=percent) + 
  ggtitle("Abandon rate on different sites")


ggplot(aes(x=day, y=1-(orders/add_to_cart)), data=subset(zp, add_to_cart != 0)) + 
  geom_point(size=3, color="red", alpha=1/5) +
  theme_economist() +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) +
  scale_y_continuous("abandon rate", labels = percent) +
  ggtitle("Abandon Rate Trend, 2013")
  



#######################################
######### Clustering    ###############
#######################################

library(cluster) 
library(fpc)
source("pair.fun.R")
cdata <- subset(zp, select=c("visits", "distinct_sessions", "orders", "gross_sales","bounces", "add_to_cart", "product_page_views", "search_page_views"))
cdata[is.na(cdata)] <- 0
#cdata <- scale(cdata)

#### spot multicollinearity
pairs(cdata, lower.panel=panel.smooth, upper.panel=panel.cor,
      diag.panel=panel.hist)

#### rearrange the data
cdata.new <- subset(cdata, select=c("orders", "bounces", "add_to_cart", "product_page_views"))
pairs(cdata.new, lower.panel=panel.smooth, upper.panel=panel.cor,
      diag.panel=panel.hist)

cdata.new.s <- scale(cdata.new)

wssplot <- function(data, nc=15, seed=1234){
                      wss <- (nrow(data)-1)*sum(apply(data,2,var))
                      for (i in 2:nc){
                          set.seed(seed)
                          wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                          plot(1:nc, wss, type="b", xlab="Number of Clusters",
                          ylab="Within groups sum of squares")}

wssplot(cdata.new.s)

# choosing nubmer of clustering 4 using Elbow Method

iterkmeans <- function(data, n=15, centers = 6, seed=1234) {
                btrm <- 0
                fit <- NA
                for (i in 1:n){
                  set.seed(seed)
                  fit.i <- kmeans(data, centers=centers)
                  btr <- fit.i$betweenss/(fit.i$betweenss + fit.i$tot.withinss)
                  if (btr > btrm) {
                    fit <- fit.i
                  }
                }
                return(fit) }

# K-Means Cluster Analysis
#cfit.0 <- kmeans(cdata.new.s, centers=3) 
cfit.0 <- iterkmeans(cdata.new.s, centers=4)
#cfit.1 <- kmeans(cdata.new.s, centers=4)
# get cluster means
aggregate(cdata.new,by=list(cfit.0$cluster),FUN=mean)
#aggregate(cdata.new,by=list(cfit.1$cluster),FUN=mean)
# append cluster assignment
cdata.new.fit <- data.frame(cdata.new, cfit.0$cluster) 
clusplot(cdata.new.s, cfit.0$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=1)

def.par <- par(no.readonly = TRUE) # save default, for resetting...
layout(t(1:4)) # 4 plots in one
for(i in 1:4) barplot(cfit.0$centers[i,], ylim=c(-1,8), main=paste("Cluster", i))
layout(def.par)

'''
# let look into group 1..
cdata.g1 <- subset(cdata.new.fit, cfit.0.cluster == 1, select=1:4)
cdata.g1.s <- scale(cdata.g1)
wssplot(cdata.g1.s)
# choose nc = 3
cfit.g1 <- kmeans(cdata.g1.s, centers=6) 
aggregate(cdata.g1,by=list(cfit.g1$cluster),FUN=mean)
clusplot(cdata.g1, cfit.g1$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=1)
'''

#######################################
######### Regression    ###############
#######################################


library(MASS)
library(sjPlot)

table(zp$platform)
fit <- with(data=subset(subset(zp, customer_type != "neither"), platform != ""), lm(orders ~ visits + site + customer_type + platform))
sjp.setTheme(axis.title.size = 1, axis.textsize = 1, legend.size = .85, geom.label.size = 5)
sjp.lm(fit,
       axisTitle.x="different platform, site, and customer type's impact on number of orders",
       breakLabelsAt = 20
)


#######################################
######### Decision Tree ###############
#######################################

order.rpart.t2 <- rpart(formula = if_ordered ~ site + platform, data=subset(zp, platform != ""), 
                        method="class", parms=list(split="information"), 
                        control=list(minsplit=150, minbucket=50), cp=0.001)
plot(order.rpart.t2)
text(order.rpart.t2, cex=0.75)
printcp(order.rpart.t2)

# Absolute cross-validated error rate: 0.57669 * 0.4363 = 0.2516098



cols <- ifelse(order.rpart.t2$frame$yval == 1, "indianred3", "green4")
# green if ordered

p.tree <- prp(order.rpart.t2, main="Decion Tree: Predicting If A Customer Will Make Order(s)",
              extra=103, #  misclassification rate at the node, expressed as the number of incorrect classifications and the number of observations in the node; also display the percentage of observations in the node. 
              #nn=TRUE, # display the node numbers
              under = TRUE,
              type = 3,
              fallen.leaves=TRUE, # put the leaves on the bottom of the page
              branch=1, # change angle of branch lines
              faclen=0, # do not abbreviate factor levels
              trace=1, # print the automatically calculated cex
              #shadow.col="gray", # shadows under the leaves
              cex = 0.8, # node size
              under.cex = 1.5,
              branch.col="gray20", 
              branch.lty = 3,
              branch.lwd = 3,
              split.cex=1.05, # make the split text larger than the node text
              split.prefix="is ", # put "is " before split text
              split.suffix="?", # put "?" after split text
              box.col=cols, border.col=cols, # green if survived
              split.col = "gray20",
              #split.box.col="lightgray", # lightgray split boxes (default is white)
              #split.border.col="darkgray", # darkgray border on split boxes
              #split.round=.5 # round the split box corners a tad
) 




