#Engagement analysis

#Load packages
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)


# Import Excel File
engag16 <- read_excel("/Users/joost/Documents/Data and Revenue/Daily analysis/Daily revenue GhanaWeb PartII.xlsx", sheet=3, col_names = F, na = '0', skip = 2)

# Clean Data
#colselect2 <- c(1, 2, 4, 5)
colnames2 <- c('date', 'alexa_pv', 'uniques1000x', 'pages_per_unique', 'nr_articles', 'comments', 'page_per_visit', 'time_per_visit', 'comments_visitor', 'comments_article', 'visits_per_visitor')

colnames(engag16) <- colnames2
engag16 <- engag16[1:366,]
engag16 <- as.data.table(engag16)

#add column for weekdays and weekends

engage16v2 <- engag16 %>% mutate(wday = weekdays(date, abbreviate = TRUE), weekend = ifelse(wday %in% c('Sat', 'Sun'), 'yes', 'no'))

##!!!remove outliers  (method just copied). Select 1 of 2 lines. Top = filter, bottom is NO FILTER
engage16or <- engage16v2 %>% filter(!(alexa_pv > 4e6))
#engage16or <- engage16v2 %>% group_by(ad_slot_id) %>% filter(!(abs(count - median(count)) > 3*sd(count)))
#engage16or[ , c("alexa_pv", "uniques1000x", "pages_per_unique", "comments_article")] <- lapply(engage16or[ , c("alexa_pv", "uniques1000x", "pages_per_unique", "comments_article")], function(x) {ifelse((abs(x-mean(x)) > 3 * sd(x)), 'NA', x)})

engage16or[ , c("alexa_pv", "uniques1000x", "pages_per_unique", "comments_article")] <- lapply(engage16or[ , c("alexa_pv", "uniques1000x", "pages_per_unique", "comments_article")], function(x) {ifelse((abs(x-mean(x)) > 3 * sd(x)), NA, x)})

  
  # group_by(element) %>%
  # filter(!(abs(value - median(value)) > 2*sd(value))) %>%
  # summarise_each(funs(mean), value)


### Multiple pairwise comparison Plots
library(lattice)
splom(~engage16v2[2:9])

# ggcorplot(
#   data = engage16v2[2:6],
#   var_text_size = 5,
#   cor_text_limits = c(5,10))

library(GGally)
ggpairs(engage16or, columns=c('alexa_pv', 'uniques1000x', 'pages_per_unique', 'nr_articles'),
        diag=list(continuous="density",   discrete="bar"), axisLabels="show")

library(PerformanceAnalytics)
chart.Correlation(engage16or[2:10], 
                  method="spearman",
                  histogram=TRUE,
                  pch=16)    

library(corrr)
x <- engage16or %>% select(2:11) %>% 
  correlate() %>%    # Create correlation data frame (cor_df)
#  focus(-date, -wday, -weekend, mirror = TRUE) %>%  # Focus on cor_df without 'cyl' and 'vs'
  rearrange() %>%  # rearrange by correlations
  shave() # Shave off the upper triangle for a clean result
fashion(x)
rplot(x) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


engage16or %>% select(2:11) %>% 
  correlate() %>% network_plot(min_cor = .2, legend = TRUE)


# Compare Weekend versus Weekdays
ggplot(engage16or, aes(page_per_visit, fill = weekend)) + geom_bar() 
ggplot(engage16or, aes(weekend, page_per_visit)) + geom_boxplot()
ggplot(engage16or, aes(weekend, alexa_pv)) + geom_boxplot()


#weekend boxplot for one variable with outliers removed
alexaWeekendor <- engage16v2 %>% select(c(2, 13)) %>%  
    group_by(weekend) %>%  
    filter(!abs(alexa_pv - median(alexa_pv)) > 2*sd(alexa_pv)) %>% ungroup() %>% 
    ggplot(aes(weekend, alexa_pv)) + geom_boxplot()
alexaWeekendor

alexaWdayor <- engage16v2 %>% select(c(2, 12)) %>%  
  group_by(wday) %>%  
  filter(!abs(alexa_pv - median(alexa_pv)) > 2*sd(alexa_pv)) %>% ungroup() %>% 
  ggplot(aes(wday, alexa_pv)) + geom_boxplot()
alexaWdayor


#Data per day
ggplot(engage16or, aes(date, uniques1000x, col = weekend)) + geom_point(alpha =.8) + ylim(0, 350)

ggplot(engage16or, aes(date, pages_per_unique, col = weekend)) + geom_point(alpha =.8)


# Various analysis not based on dates
ggplot(engage16or, aes(comments, comments_article, col= day)) + 
  geom_point(alpha = 0.8) + stat_smooth(method = "lm")

ggplot(engage16or, aes(nr_articles, comments, col= day)) + 
  geom_point(alpha = 0.8) + stat_smooth(method = "lm")

ggplot(engage16or, aes(nr_articles, pages_per_unique, col= day)) + 
  geom_point(alpha = 0.8) + stat_smooth(method = "lm")

ggplot(engage16or, aes(alexa_pv, pages_per_unique, col= day)) + 
  geom_point(alpha = 0.8) + stat_smooth(method = "lm")



