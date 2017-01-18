# Devetraco analysis

library(readxl)
#load Devtraco data
devtraco_raw <- read_excel('/Users/joost/Downloads/Devtraco.xls', sheet = 2)

names(devtraco_raw) <- c('LineItem', 'Site', 'Date', 'Impressions', 'Clicks', 'CTR')

devtr <- data.frame(devtraco_raw)
devtr$LineItem <- as.factor(devtr$LineItem)
devtr$Site <- as.factor(devtr$Site)
#devtr$Creative <- as.factor(devtr$Creative)
devtr$Date <- as.Date(devtr$Date)
devtr <- devtr %>% filter(Date > '2016-10-12')
levels(devtr$LineItem) <- c('Desktop_GH', 'Destkop_USUKCAN', 'Mobile_GH')
devtr$LineItem <- ordered(devtr$LineItem, levels = c('Destkop_USUKCAN', 'Desktop_GH','Mobile_GH'))
#devtr$Creative <- devtr$Creative %>% (function(x) ifelse(x == 'Niiyo Final phase 2', "Niiyo Final phase 2", x))
#levels(devtr$Creative) <- c('A', 'B', 'B', 'C')


#summary per day
devtrperday <- devtr %>% group_by(Date) %>%  
  summarise(total_clicks = sum(Clicks), total_imp = sum(Impressions)) %>%
  mutate(CTR = (total_clicks/total_imp)*100 ) %>% 
  ungroup()


p1 <- ggplot(devtr, aes(Date, Impressions, col = LineItem)) + 
  geom_point(size = 2, alpha = 0.7) + 
  theme(legend.position="bottom")

p2 <- ggplot(devtr, aes(Date, Clicks, col = LineItem)) + 
  geom_point(size = 2, alpha = 0.7) +
  theme(legend.position="bottom")

p3 <- ggplot(devtr, aes(Date, CTR, col = LineItem)) +
  geom_point(size = 2, alpha = 0.7) +
  theme(legend.position="bottom")

ggplot(devtr, aes(Date, Impressions, col = CTR)) + 
  geom_point(size = 2, alpha = 0.7) + 
  theme(legend.position="bottom") +
  facet_wrap(~LineItem)

ggplot(devtr, aes(Date, CTR, col = Impressions)) + 
  geom_point(size = 2, alpha = 0.7) + 
  theme(legend.position="bottom") +
  scale_colour_gradient(low = "red", high = "green") +
  geom_vline(xintercept = as.numeric(as.Date('2016-11-12'))) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  facet_wrap(~LineItem)

ggplot(devtr, aes(Date, Clicks, col = Impressions)) + 
  geom_point(size = 2, alpha = 0.7) + 
  theme(legend.position="bottom") +
  geom_vline(xintercept = as.numeric(as.Date('2016-11-12'))) +
  scale_colour_gradient(low = "red", high = "green") +
  #scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  facet_wrap(~LineItem)

ggplot(devtr, aes(LineItem, CTR, fill = Site)) + 
  geom_boxplot() + 
  theme(legend.position="bottom")

ggplot(devtrperday, aes(Date, total_clicks, col = CTR)) +
  scale_colour_gradient(low = "red", high = "green") +
  geom_point(size = 2, alpha = 0.7)
