# Plotting section data per publisher, for specific data range


#import section data

# dbConnect() call
library(RMySQL)
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = 'rijk9_imkserver', 
                 host = "qs3990.pair.com", 
                 port = 0000,
                 user = "rijk9_12",
                 password = "efiYjhFV")

#dbListTables(con)
library(DBI)

publisher <- 2
startdate <- c('2016-12-31')
enddate <- as.character(Sys.Date()) #insert date AFTER last day of reporting

myquery <- paste("SELECT  `date`, `ad_slot_id`, `channel_id`, `count` FROM `statistics_slots` WHERE `ad_slot_id` IN( '2',  '13',  '22',  '33', '381', '377', '380', '378', '379',  '42',  '52',  '66',  '63',  '61', '317', '144', '234', '240', '242', '246', '244', '248', '250', '216', '313', '318', '230', '210') AND `date` > '", startdate, "' AND `date` < '", enddate, "' AND `publisher_id` = ", publisher , sep ='')
sectallday <- dbGetQuery(con, myquery)
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


##!!!remove outliers  (method just copied). Select 1 of 2 lines. Top = filter, bottom is NO FILTER
#sectclean <- sectjoin %>% group_by(ad_slot_id) %>% filter(!(abs(count - median(count)) > 3*sd(count)))
sectclean <- sectjoin

#Top sections
library(data.table)
secttopsect <- as.data.table(filter(sectclean, section %in% c('HomePage', 'News', 'Sports', 'Entertainment')))
       secttopsect[(ad_slot_id == 317 & date < '2016-04-14'), count := count/1.5] # issues with double counting on mobile homepage
       secttopsect[(ad_slot_id == 317 & date < '2017-01-06' & date > '2016-12-31'), count := NA] #issues with too much mobile homepage hits
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
plotI <- ggplot(data = secttopsect, aes(date, count, col = section)) + 
  geom_point(alpha = .8) + 
  labs(title = 'Daily PageViews; panel 1', y = 'pageviews')  +
  scale_y_continuous(limits = c(0, 1000000), expand = c(0, 0)) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90), legend.position="none") +
  scale_color_manual(values = cols) +
  facet_grid(section ~ medium) 

#Middle Shared y-axis
plotII <- ggplot(data = sect2ndsect, aes(date, count, col = section)) + 
  geom_point(alpha = .8) + 
  labs(title = 'Daily PageViews; panel 2', y = "pageviews")  +
  scale_y_continuous(limits = c(0, 100000), expand = c(0, 0)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90), legend.position="none") +
  scale_color_manual(values = cols) +
  facet_grid(section ~ medium) 

#Low shared y-axis
plotIII <- ggplot(data = sectlowsect, aes(date, count, col = section)) +  
  geom_point(alpha = .8) + 
  labs(title = 'Daily PageViews; panel 3', y = "pageviews")  +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90), legend.position="none") +
  scale_y_continuous(limits = c(0, 15000), expand = c(0, 0)) + 
  scale_color_manual(values = cols) +
  facet_grid(section ~ medium) 


##Plots for a single section
##first fill in section of interest below
sectionofinterest <- c('News')

singlesect <- as.data.table(filter(sectclean, section %in% sectionofinterest))
singlesect$section <- factor(singlesect$section, levels = sectionofinterest)

maxyax_singlsect <- signif(max(singlesect$count)*1.1, 2)
ticksyax_singlsect <- floor(round(max(singlesect$count)*1.1))

singleplot <- ggplot(data = singlesect, aes(date, count)) +
  geom_point(alpha = .8, size = 2, col = 'blue') +
  labs(title = paste('Daily PageViews for', sectionofinterest, sep = ' '), y = "pageviews")  +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90), legend.position="none") +
  scale_y_continuous(breaks = seq(0, maxyax_singlsect, by = 200000), limits = c(0, maxyax_singlsect), expand = c(0, 0)) +
  scale_color_manual(values = cols) +
  facet_grid(section ~ medium)
singleplot



