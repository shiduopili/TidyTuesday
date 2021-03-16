library(ggbump)
youtube <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv'
  )

youtube %>% select(year, funny:use_sex) %>% 
gather(type, value, funny:use_sex) %>% 
filter(value =="TRUE") %>% 
count(year, type) %>% 
ggplot(aes(factor(year), n, color = type)) +
  geom_bump(size = 2, smooth = 6) + geom_point(size = 4, shape = 1) + 
theme(
    panel.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "white")
  ) + xlab("") + 
ylab("") + 
scale_color_discrete(
    labels = c(
      "Animals",
      "Celebrity",
      "Danger",
      "Funny",
      "Patriotic",
      "Show Product Quickly",
      "Sex"
    )
  ) + 
ylim(0, 15) + 
guides(col = guide_legend(nrow = 1)) + 
labs(title = "Changes in Types of Superbowl Ads over Years", 
     caption = "@shiduopili\nData Source: fivethirtyeight") 
