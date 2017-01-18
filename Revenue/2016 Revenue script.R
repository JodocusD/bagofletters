
#experiment using readr
# library(readr)
# revplay <- read_csv("/Users/joost/Desktop/R/Revenue/revenuetmp2.csv", col_names = TRUE, na = c("0"))

#experiment using readxl
library(readxl)
rev2016raw <- read_excel("/Users/joost/Desktop/R/Revenue/revenuetmp2.xlsx", col_names = F, na = '0', skip = 2)
rev2015raw <- read_excel("/Users/joost/Desktop/R/Revenue/revenuetmp3.xlsx", col_names = F, na = '0', skip = 16)

colselect1 <- c(1, 2, 4, 5)
colnames1 <- c('date', 'totrev', 'pageview', 'ecpmEuro')

rev2016 <- rev2016raw[, colselect1]
colnames(rev2016) <- colnames1
rev2016 <- rev2016[1:366,]

rev2015 <- rev2015raw[, colselect1]
colnames(rev2015) <- colnames1
rev2015 <- rev2015[1:457,]

#add month column
library(lubridate)
rev2016$date <- as.POSIXct(rev2016$date)
rev2016$month <- month(rev2016$date, label = TRUE)
rev2016$monthnum <- month(rev2016$date, label = F)
rev2016$year <- year(rev2016$date)

rev2015$date <- as.POSIXct(rev2015$date)
rev2015$month <- month(rev2015$date, label = TRUE)
rev2015$monthnum <- month(rev2015$date, label = F)
rev2015$year <- year(rev2015$date)

#organising data by month
library(dplyr)
#bymonth16 <- rev2016 %>% group_by(monthnum) %>% summarise(funs(mean))
bymonth16 <- rev2016  %>% group_by(monthnum) %>% summarise(revenue = sum(totrev))
bymonth16$year <- as.factor(2016)
levels(bymonth16$year) <- c('2016', '2015', '2014')

bymonth15 <- rev2015  %>% group_by(year, monthnum) %>% summarise(revenue = sum(totrev))
bymonth15$year <- as.factor(bymonth15$year)
levels(bymonth15$year) <- c('2014', '2015', '2016')
bymonth15$year[1:3] <- as.factor(2014)
bymonth15$year[-(1:3)] <- as.factor(2015)

#bymonthwide <- full_join(bymonth16, bymonth15, by = 'monthnum')
bymonthlong <- bind_rows(bymonth15, bymonth16)

library(ggplot2)
library(RColorBrewer)
ggplot(bymonthlong, aes(monthnum, revenue, colour = year)) + 
  labs( title = " Monthly Revenue by year")  +  
  geom_point(size = 4, alpha = 0.7) + 
  scale_colour_brewer(palette = 'Dark2') +
  scale_x_continuous(name = 'Month', breaks = seq(1, 12, by = 1),
                     limits = c(1, 12)) +
  scale_y_continuous(name = 'Revenue (Euro)', breaks = seq(70000, 170000, by = 10000),
                     limits = c(75000, 170000)) 

 
  