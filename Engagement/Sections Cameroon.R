# Plotting section data for Cameroonweb

#import section data
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

library(DBI)
cutofdate = c('2016-12-31') #not in use
publisher <- 3  #imk publisher number; 3 = cameroonweb
startdate <- c('2016-12-31') #insert date before startdate
enddate <- as.character(Sys.Date()) #insert date AFTER last day of reporting

myquery <- paste("SELECT  `date`, `ad_slot_id`, `channel_id`, `count` FROM `statistics_slots` WHERE `ad_slot_id` IN( '2',  '13',  '22',  '33', '381', '377', '380', '378', '379',  '42',  '52',  '66',  '63',  '61', '317', '144', '234', '240', '242', '246', '244', '248', '250', '216', '313', '318', '230', '210') AND `date` > '", startdate, "' AND `date` < '", enddate, "' AND `publisher_id` = ", publisher , sep ='')
cmrallday <- dbGetQuery(con, myquery)
cmrallday$date <- as.Date(cmrallday$date)
cmrallday$ad_slot_id <- as.numeric(cmrallday$ad_slot_id)

#disconnect DB
dbDisconnect(con)

#work with AdSlotKey file for labeling sections
library(readr)
read_csv("AdSlotKey.csv") -> slotkey
slotkey$channel_id <- NULL
slotkey$ad_slot_id <- as.numeric(slotkey$ad_slot_id)
sectionlist <- c('HomePage', 'News', 'Entertainment', 'Sports', 'World', 'Business', 'Opinions', 'Members', 'Country', 'Jobs', 'Real Estate', 'Telephone Directory', 'Used Cars', 'Trucks')
slotkey$section <- factor(slotkey$section, levels = sectionlist) 

#label data with sectionkey and organise
library(dplyr)
sectjoin <- left_join(cmrallday, slotkey, by = ('ad_slot_id'))
sectjoin$medium <- ifelse(sectjoin$channel_id == 1, 'Desktop', 'Mobile')
sectjoin$count <- as.numeric(sectjoin$count)

#Make a table of yesterdays data
cmrdesksect <- sectjoin %>% 
  filter(medium == 'Desktop', date == as.Date(Sys.Date() -1)) %>% 
  select(c(1,4,5)) %>% rename(desktop = count)
cmrmobisect <- sectjoin %>% 
  filter(medium == 'Mobile', date == as.Date(Sys.Date() -1)) %>% 
  select(c(1,4,5)) %>% rename(mobile = count)
yestertable <- full_join(cmrdesksect, cmrmobisect, by = c('date', 'section'))
yestertable <- yestertable[c(1,3,2,4)] %>%  select(2:4)
yestertabletotal <- summarise(yestertable, desktop = sum(desktop), mobile = sum(mobile, na.rm = TRUE))
    row.names(yestertabletotal) <- c('Total')
  
##!!!remove outliers  (method just copied). Select 1 of 2 lines. Top = filter, bottom is NO FILTER
#sectclean <- sectjoin %>% group_by(ad_slot_id) %>% filter(!(abs(count - median(count)) > 3*sd(count)))
sectclean <- sectjoin


#Top sections
library(data.table)
sect1 <- as.data.table(filter(sectclean, section %in% c('HomePage', 'Sports', 'Entertainment', 'Business', 'World')))
#sect1[(ad_slot_id == 317 & date < '2016-04-14'), count:=count/1.5] 
sect1$section <- factor(sect1$section, levels = c('HomePage', 'News', 'Entertainment', 'Sports', 'World', 'Business', 'Opinions', 'Members', 'Country', 'Jobs', 'Real Estate', 'Telephone Directory', 'Used Cars', 'Trucks')) 

#2nd Tier sections
sect2 <- filter(sectclean, section %in% c('Opinions', 'Members', 'Country', 'Jobs', 'Real Estate', 'Telephone Directory')) 
sect2$section <- factor(sect2$section, levels = c('HomePage', 'News', 'Entertainment', 'Sports', 'World', 'Business', 'Opinions', 'Members', 'Country', 'Jobs', 'Real Estate', 'Telephone Directory', 'Used Cars', 'Trucks')) 

#Section Plots

#testplots based on cmrallday
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
cols <- brewer.pal(8, 'Set1')

#PLOT top and bottom sections

# #Low Indiv. y-axis
# ggplot(data = sectlowsect, aes(date, count, col = section)) +  
#   geom_point(alpha = 1) + 
#   labs(title ='Daily PageViews for Low Volume Sections: Individual y-axis', y = 'pageviews')  +
#   theme_light() +
#   scale_color_manual(values = cols) +
#   facet_grid(section ~ medium, scales = 'free_y') 

#High Indiv y-axis
# ggplot(data = secttopsect, aes(date, count, col = section)) + 
#   geom_point(alpha = 1) + 
#   labs(title ='Daily PageViews for High Volume Sections: Individual y-axis', y = 'pageviews')  +
#   theme_light() +
#   scale_color_manual(values = cols) +
#   facet_grid(section ~ medium, scales = 'free_y') 

#High Shared y-axis
maxyax_sect1 <- signif(max(sect1$count)*1.1, 2)
cmrp2 <- ggplot(data = sect1, aes(date, count, col = section)) + 
  geom_point(alpha = .8, size = 2) + 
  labs(title = 'Daily PageViews part I', y = 'pageviews')  +
  scale_y_continuous(breaks = seq(0, maxyax_sect1, by = maxyax_sect1/5), limits = c(0, maxyax_sect1), expand = c(0, 0)) +
  theme_light() + guides(col=FALSE) + 
  scale_color_manual(values = cols) +
  facet_grid(section ~ medium) 

#Middle Shared y-axis
maxyax_sect2 <- signif(max(sect2$count)*1.1, 2)
cmrp3 <- ggplot(data = sect2, aes(date, count, col = section)) + 
  geom_point(alpha = .8, size = 2) + 
  labs(title = 'Daily PageViews part II', y = 'pageviews')  +
  scale_y_continuous(breaks = seq(0, maxyax_sect2, by = maxyax_sect2/5), limits = c(0, maxyax_sect2), expand = c(0, 0)) +
  theme_light() + guides(col=FALSE) + 
  scale_color_manual(values = cols) +
  facet_grid(section ~ medium) 


##Plots for a single section
##first fill in section of interest below
sectionofinterest <- c('News')

singlesect <- as.data.table(filter(sectclean, section %in% sectionofinterest))
singlesect$section <- factor(singlesect$section, levels = sectionofinterest)

maxyax_singlsect <- signif(max(singlesect$count)*1.1, 2)
ticksyax_singlsect <- floor(round(max(singlesect$count)*1.1))

cmrp1 <- ggplot(data = singlesect, aes(date, count, col = section)) +
  geom_point(alpha = .8, size = 2) +
  labs(title = paste('Daily PageViews for', sectionofinterest, sep = ' '), y = "pageviews") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90), legend.position="none") +
  scale_y_continuous(breaks = seq(0, maxyax_singlsect, by = maxyax_singlsect/5), limits = c(0, maxyax_singlsect), expand = c(0, 0)) +
  scale_color_manual(values = cols) +
  facet_grid(section ~ medium)



