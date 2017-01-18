# Plotting section data

#import section data

# dbConnect() call
library(RMySQL)
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = 'rijk3_imkserver', 
                 host = "qs3853.pair.com", 
                 port = 0000,
                 user = "rijk3_101",
                 password = "QeCKJnSa")
#inspect tables
#dbListTables(con)


# NOTE; via dbGetQuery()  direct SQL queries can be directed at the DB
#qryfor1day <- c("SELECT *
#   FROM `statistics_slots`
# WHERE `publisher_id` =2
# AND `ad_slot_id` =2 AND `date` = 2016-03-01")
# ORDER BY `statistics_slots`.`date` DESC
# LIMIT 240 , 30

library(DBI)
sectallday <- dbGetQuery(con, "SELECT  `date`, `ad_slot_id`, `channel_id`, `count` FROM `statistics_slots` WHERE `ad_slot_id` IN( '2',  '13',  '22',  '33', '381', '377', '380', '378', '379',  '42',  '52',  '66',  '63',  '61', '317', '144', '234', '240', '242', '246', '244', '248', '250', '216', '313', '318', '230', '210') AND `date` > '2016-01-01' AND `publisher_id` = 2 ")
sectallday$date <- as.Date(sectallday$date)
sectallday$ad_slot_id <- as.numeric(sectallday$ad_slot_id)

#disconnect DB
dbDisconnect(con)

#work with AdSlotKey file for labeling sections
library(readr)
read_csv("AdSlotKey.csv") -> slotkey
slotkey$channel_id <- NULL
slotkey$ad_slot_id <- as.numeric(slotkey$ad_slot_id)
slotkey$section <- factor(slotkey$section, levels = c('HomePage', 'News', 'Entertainment', 'Sports', 'World', 'Business', 'Opinions', 'Members', 'Country', 'Jobs', 'Real Estate', 'Telephone Directory', 'Used Cars', 'Trucks')) 

#label data with sectionkey and organise
library(dplyr)
sectjoin <- left_join(sectallday, slotkey, by = ('ad_slot_id'))
sectjoin$medium <- ifelse(sectjoin$channel_id ==1, 'Desktop', 'Mobile')
sectjoin$count <- as.numeric(sectjoin$count)


#remove outliers  (method just copied)  
sectclean <- sectjoin %>% group_by(ad_slot_id) %>% filter(!(abs(count - median(count)) > 2*sd(count)))

#Top sections
library(data.table)
secttopsect <- as.data.table(filter(sectclean, section %in% c('HomePage', 'News', 'Sports', 'Entertainment')))
       secttopsect[(ad_slot_id == 317 & date < '2016-04-14'), count:=count/1.5] 
       secttopsect$section <- factor(secttopsect$section, levels = c('HomePage', 'News', 'Entertainment', 'Sports', 'World', 'Business', 'Opinions', 'Members', 'Country', 'Jobs', 'Real Estate', 'Telephone Directory', 'Used Cars', 'Trucks')) 
       
#2nd Tier sections
sect2ndsect <- filter(sectclean, section %in% c('Business', 'World', 'Opinions', 'Members')) 
sect2ndsect$section <- factor(sect2ndsect$section, levels = c('HomePage', 'News', 'Entertainment', 'Sports', 'World', 'Business', 'Opinions', 'Members', 'Country', 'Jobs', 'Real Estate', 'Telephone Directory', 'Used Cars', 'Trucks')) 

#Lesser sections
sectlowsect <- filter(sectclean, !(section %in% c('HomePage', 'News', 'Sports', 'Entertainment', 'Business', 'World', 'Opinions', 'Members'))) 
sectlowsect$section <- factor(sectlowsect$section, levels = c('HomePage', 'News', 'Entertainment', 'Sports', 'World', 'Business', 'Opinions', 'Members', 'Country', 'Jobs', 'Real Estate', 'Telephone Directory', 'Used Cars', 'Trucks')) 

#Section Plots

#testplots based on sectallday
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
cols <- brewer.pal(8 ,"Set1")

#PLOT top and bottom sections

# #Low Indiv. y-axis
# ggplot(data = sectlowsect, aes(date, count, col = section)) +  
#   geom_point(alpha = 1) + 
#   labs(title ='Daily PageViews for Low Volume Sections: Individual y-axis', y = "pageviews")  +
#   theme_light() +
#   scale_color_manual(values = cols) +
#   facet_grid(section ~ medium, scales = 'free_y') 

#High Indiv y-axis
# ggplot(data = secttopsect, aes(date, count, col = section)) + 
#   geom_point(alpha = 1) + 
#   labs(title ='Daily PageViews for High Volume Sections: Individual y-axis', y = "pageviews")  +
#   theme_light() +
#   scale_color_manual(values = cols) +
#   facet_grid(section ~ medium, scales = 'free_y') 

#High Shared y-axis
ggplot(data = secttopsect, aes(date, count, col = section)) + 
  geom_point(alpha = .8) + 
  labs(title = 'Daily PageViews for High Volume Sections', y = 'pageviews')  +
  scale_y_continuous(limits = c(0, 1000000)) +
  theme_light() +
  scale_color_manual(values = cols) +
  facet_grid(section ~ medium) 

#Middle Shared y-axis
ggplot(data = sect2ndsect, aes(date, count, col = section)) + 
  geom_point(alpha = .8) + 
  labs(title = 'Daily PageViews for Middle Volume Sections', y = "pageviews")  +
  scale_y_continuous(limits = c(0, 75000)) +
  theme_light() +
  scale_color_manual(values = cols) +
  facet_grid(section ~ medium) 

#Low shared y-axis
ggplot(data = sectlowsect, aes(date, count, col = section)) +  
  geom_point(alpha = .8) + 
  labs(title = 'Daily PageViews for Low Volume Sections', y = "pageviews")  +
  theme_light() +
  scale_y_continuous(limits = c(0, 15000)) + scale_color_manual(values = cols) +
  facet_grid(section ~ medium) 


