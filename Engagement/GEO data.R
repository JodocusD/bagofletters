# Plotting GEO data

#import GEO data

# dbConnect() call
library(RMySQL)
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = 'rijk9_imkserver', 
                 host = "qs3990.pair.com", 
                 port = 0000,
                 user = "rijk9_12",
                 password = "efiYjhFV")
#inspect tables
#dbListTables(con)


# NOTE; via dbGetQuery()  direct SQL queries can be directed at the DB

library(DBI)
geoallday <- dbGetQuery(con, "SELECT  `date`, `country`, `channel_id`, `count` FROM `statistics_geo` WHERE `country` IN (
'GH', 'US', 'EU', 'NO', 'GB', 'CA', 'DE', 'IT', 'NL', 'FR') AND `date` > '2016-01-01' AND `publisher_id` = 2 ")
geoallday$date <- as.Date(geoallday$date)
geoallday$count <- as.numeric(geoallday$count)    
geoallday$country <- factor(geoallday$country, levels = c('GH', 'US', 'EU', 'NO', 'GB', 'CA', 'DE', 'IT', 'NL', 'FR'))  

#disconnect DB
dbDisconnect(con)


#remove outliers  (method just copied)  
geoclean <- geoallday %>% group_by(channel_id, country) %>% filter(!(abs(count - median(count)) > 2*sd(count)))
geoclean$medium <- ifelse(geoclean$channel_id ==1, 'Desktop', 'Mobile')

#Load libaries
library(dplyr)
library(ggplot2)
library(RColorBrewer)


colsgeo <- brewer.pal(8 ,"Set1")
# countrylist <- geoallday %>% group_by(country) %>% summarise(avg = mean(count))

#Plot Total per GEO
ggplot(geoclean, aes(country, count)) + 
  geom_boxplot() +
  theme_light() +
  labs(title = 'Daily Pageviews per GEO', y = "pageviews")  +
  facet_grid(. ~ medium) 

#Plot GEO per Day
ggplot(filter(geoclean, country %in% c('GH', 'US', 'EU', 'NO')), aes(date, count, col = country)) + 
  geom_point(alpha = .7) +
  theme_light() +
  labs(title = 'Daily Pageviews per GEO', y = "pageviews")  +
  scale_color_manual(values = colsgeo) +
  facet_grid(country ~ medium) 

ggplot(filter(geoclean, country %in% c('GB', 'CA', 'DE', 'IT', 'NL', 'FR')), aes(date, count, col = country)) + 
  geom_point(alpha = .7) +
  #geom_line(alpha = .8) +
  theme_light() +
  labs(title = 'Daily Pageviews per GEO', y = "pageviews")  +
  scale_color_manual(values = colsgeo) +
  facet_grid(country ~ medium) 
                                                                                                                                           