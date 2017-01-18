# Daily revenues

#libraries in use
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)

#Read data using readxl,  and data prep
day2016raw <- read_excel("/Users/joost/Documents/Data and Revenue/Daily analysis/Daily revenue GhanaWeb PartII.xlsx", sheet=1, col_names = F, na = '0', skip = 2)

colselect2 <- c(1, 2, 3, 4, 6, 7, 9, 10, 13)
colnames2 <- c('datum', 'totrev', 'pageview', 'ecpmEuro', 'Adsense', 'OpenX', 'Outbrain', 'AdX', 'Mobile_Total')

dayrev2016 <- day2016raw[, colselect2]
colnames(dayrev2016) <- colnames2
dayrev2016 <- dayrev2016[1:366,]

#calcualte DAILY desktop revenue and ratio between desktop and mobile
cutofday <- 4000
dayrev2016ext <- dayrev2016 %>% mutate(desktoprev = totrev - Mobile_Total,  ratio_desktop_mobile = desktoprev / Mobile_Total, totrevcap = ifelse(totrev > cutofday, cutofday, totrev))

#organise data by week
weekrev2016 <- dayrev2016 %>% group_by(week = cut(datum, "week")) %>% summarize(totrev = sum(totrev), Mobile_Total = sum(Mobile_Total))
cutoffweek = 35000 
weekrev2016ext <- weekrev2016 %>% mutate(desktoprev = totrev - Mobile_Total,  ratio_desktop_mobile = desktoprev / Mobile_Total, totrevcap = ifelse(totrev > cutoffweek, cutoffweek, totrev), desktopcap = ifelse(desktoprev > cutoffweek, cutoffweek, desktoprev))
names(weekrev2016ext)[1] <- c('datum')
weekrev2016ext$datum <- as.Date(weekrev2016ext$datum)

#plot 2016 total desktop and mobile revenue by DAY
p <- ggplot(dayrev2016ext)

p1 <- p +
  geom_area(aes(datum, totrevcap, fill = 'darkgreen'), alpha = 0.8) +
  scale_y_continuous(name = 'Revenue (Euro)', breaks = seq(0, 4000, by = 1000), limits = c(0, 4400), expand = c(0, 0)) +
  geom_area(aes(datum, desktoprev, fill = 'yellow'), alpha = 0.8) +
  geom_area(aes(datum, Mobile_Total, fill = 'blue'), alpha = 0.8) +
  scale_fill_identity(name = 'Source', guide = 'legend',labels = c('Mobile', 'Total', 'Desktop')) 

#plot ratio between desktop and mobile
p2 <- p+ geom_line(aes(x = datum, y = ratio_desktop_mobile, col = 'black')) + 
  scale_y_continuous(name = 'Desktop/Mobile', breaks = seq(1, 2, by = 0.5), limits = c(1, 2), expand = c(0, 0)) + 
  geom_smooth(aes(x = datum, y = ratio_desktop_mobile), method ='lm', formula=y~x, col = 'red') + 
  scale_colour_identity(name = 'Ratio', guide = 'legend',labels = c('')) 

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

#plot total desktop and mobile revenue by WEEK
pw <- ggplot(weekrev2016ext)

pw1 <- pw +
  geom_area(aes(datum, totrevcap, fill = 'darkgreen'), alpha = 0.8) +
  scale_y_continuous(name = 'Revenue (Euro)', breaks = seq(0, 35000, by = 10000), limits = c(0, 35000), expand = c(0, 0)) +
  geom_area(aes(datum, desktopcap, fill = 'yellow'), alpha = 0.8) +
  geom_area(aes(datum, Mobile_Total, fill = 'blue'), alpha = 0.8) +
  scale_fill_identity(name = 'Source', guide = 'legend', labels = c('Mobile', 'Total', 'Desktop'))

#plot ratio between desktop and mobile
pw2 <- pw + 
  geom_line(aes(x = datum, y = ratio_desktop_mobile, col = 'black')) + 
  scale_y_continuous(name = 'Desktop/Mobile', breaks = seq(1, 2, by = 0.5), limits = c(1, 2), expand = c(0, 0)) + 
  geom_smooth(aes(x = datum, y = ratio_desktop_mobile), method = 'lm', formula=y~x, col = 'red') + 
  scale_colour_identity(name = 'Ratio', guide = 'legend',labels = c('')) 

grid.newpage()
grid.draw(rbind(ggplotGrob(pw1), ggplotGrob(pw2), size = "last"))

# Revenue by Source per week
weeksource2016 <- dayrev2016 %>% group_by(week = cut(datum, "week")) %>% summarize_each(funs(sum))
weeksource2016$datum <- NULL
names(weeksource2016)[1] <- c('datum')
cutoffweeksource = 12500 
weeksource2016ext <-  as.data.frame(lapply(weeksource2016[5:8], function(x) {ifelse( x > cutoffweeksource, cutoffweeksource, x)}))
weeksource2016ext$datum <- weeksource2016$datum
weeksource2016ext$datum <- as.Date(weeksource2016ext$datum)

weeksource2016long <- weeksource2016ext %>% gather('source', 'revenue', 1:4)

pwsl <- ggplot(weeksource2016long, aes(datum, revenue, col = source)) +
  geom_line(size = 1.5, alpha = 0.8) +
  labs(title = 'Revenue per Source per Week', y = 'Weekly Revenue (EURO)', x = '') +
  scale_y_continuous(breaks = seq(0, cutoffweeksource, by = cutoffweeksource/5), limits = c(0, cutoffweeksource), expand = c(0, 0))
pwsl


# A look into OpenX data only
colselectOX <- c(1, 26, 28)
colnamesOX <- c('datum', 'OpenX_Desktop', 'OpenX_Mobile')

dayOX <- day2016raw[, colselectOX]
dayOX <- dayOX[1:366,]
colnames(dayOX) <- colnamesOX
weekOX <-  dayOX %>% group_by(week = cut(datum, "week")) %>%  summarize(OpenX_Desktop = sum(OpenX_Desktop), OpenX_Mobile = sum(OpenX_Mobile))
weekOX$week <- as.Date(weekOX$week)

cutofOX <- 9000
weekOXext <- weekOX %>% mutate(OXratio = OpenX_Desktop / OpenX_Mobile, OX_Deskcap = ifelse(OpenX_Desktop > cutofOX, cutofOX, OpenX_Desktop))

weekOXlong <- weekOXext %>% select(c(1,3,5)) %>%  gather('OX', 'revenue',  2:3)

pwox <- ggplot(weekOXlong, aes(week, revenue, col = OX)) +
  geom_line(size = 1.5) +
  scale_y_continuous(breaks = seq(0, cutofOX, by = 3000), limits = c(0, cutofOX), expand = c(0, 0))
  
prox <- ggplot(weekOXext, aes(week, OXratio, col = "black")) +
  geom_line(size = 1.5) +
  scale_colour_identity(name = 'Ratio', guide = 'legend',labels = c('')) 

grid.newpage()
grid.draw(rbind(ggplotGrob(pwox), ggplotGrob(prox), size = "last"))

