

Engclean %>%
  ggvis(x= ~`Page/visit`, 
        y= ~`Time/visit`,
        fill= ~wday,
        size= ~Articles
        ) %>% 
  layer_points %>%
  #layer_smooths %>% 
  add_legend(c("size", "fill"))


Engclean %>%
  ggvis(~wday, ~Alexa) %>% 
  layer_boxplots()

Engclean %>%
  ggvis(~wday, ~Articles) %>% 
  layer_boxplots()

Engclean %>%
  ggvis(~wday, ~`Uniqes 1000x `) %>% 
  layer_boxplots()

