setwd('/Users/xuhong/Documents/Duke/JOB HUNTING/Job Interviews/Zappos')
zp <- read.csv("Analytics Challenge Data 2.csv", sep=";")
zp <- subset(zp, select=-c(X, X.1, X.2))
zp$day <- as.Date(as.POSIXct(strptime(zp$day, "%y-%m-%d %H:%M")))



'
data.frame:  21061 obs. of  12 variables:
  $ day               : Factor w/ 268 levels "13-1-1 0:00",..: 1 1 1 1 1 1 1 1 1 1 ...
$ site              : Factor w/ 6 levels "Acme","Botly",..: 1 1 4 1 2 1 4 4 1 1 ...
$ new_customer      : int  1 1 1 1 1 1 1 1 0 0 ...
$ platform          : Factor w/ 15 levels "","Android","BlackBerry",..: 2 3 6 14 2 9 2 14 8 7 ...
$ visits            : int  24 0 0 922 11 384 14 1 41 448 ...
$ distinct_sessions : int  16 0 0 520 10 214 10 0 27 368 ...
$ orders            : int  14 0 0 527 11 213 4 0 6 36 ...
$ gross_sales       : int  1287 13 98 60753 1090 28129 432 31 705 4637 ...
$ bounces           : int  4 0 0 149 0 65 4 0 6 80 ...
$ add_to_cart       : int  16 0 0 610 11 245 7 0 12 79 .d..
$ product_page_views: int  104 1 0 3914 4 1783 33 2 130 722 ...
$ search_page_views : int  192 0 0 7367 19 3255 52 2 272 1073 ...
'
'
# first weed out outliers
> quantile(zp$visits, 0.95)
95% 
9260 
'

mobiles <- c("Android", "BlackBerry", "iPad", "iOS", "iPhone", "SymbianOS", "WindowsPhone")
qplot(data=zp, x=distinct_sessions, xlim=c(0, 90000))
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


zp.sales <- zp %>%
  group_by(day) %>%
  summarise(
    daily_sales = sum(gross_sales, na.rm=T),
    n = n()) %>%
  arrange(day)

p.sales_byday <- ggplot(data=zp.sales, aes(x=day, y=daily_sales)) + geom_point(aes(color=daily_sales)) +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) +
  ggtitle("Daily sales in 2013") + 
  theme(plot.title = element_text(size = 25)) + 
  scale_colour_continuous(name="Daily Sales",
                          breaks=c(400000, 800000, 1200000, 1600000),
                          low = "blue",
                          high = "red",
                          labels=c("0.4M", "0.8M", "1.2M", "1.6M")) +
  ylab("Daily Sales")

p.sales_weeks <- ggplot(data=zp.sales, aes(x=day, y=daily_sales)) + geom_point(aes(color=daily_sales)) +
  scale_x_date(breaks = date_breaks("weeks"), limits = c(as.Date("2013-9-28"), as.Date("2013-12-31")))+
  ggtitle("Daily sales from Sept to Dec 2013")  +
  theme(plot.title = element_text(size = 25)) + 
  scale_colour_continuous(name="Daily Sales",
                          breaks=c(400000, 800000, 1200000, 1600000),
                          low = "blue",
                          high = "red",
                          labels=c("0.4M", "0.8M", "1.2M", "1.6M")) +
  ylab("Daily Sales")


