
avgart <- Engclean[,.(avgArticles = round(mean(Articles))), by=wday]
avgart %>% ggvis(~wday, ~avgArticles, fill=~wday) %>% layer_points()

avgvisit <- Engclean[,.(avgVisit = round(mean(Alexa))), by=wday]
avgvisit %>% ggvis(~wday, ~avgVisit, fill=~wday) %>% layer_points()