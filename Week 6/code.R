hbcu_all <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv'
  )

hbcu_all %>% gather(gender, value, Males:Females) %>% select(Year, `Total enrollment`, gender, value) %>% mutate(percent =
                                                                                                                   value / `Total enrollment`) %>% ggplot(aes(x = factor(Year), y = percent, fill =
                                                                                                                                                                gender)) + geom_bar(stat = "identity") + coord_polar()

hbcu_all %>% ggplot(aes(x = factor(Year), y = Females)) + geom_bar(stat = "identity") +
  coord_polar()

hbcu_all %>% gather(type,
                    value,
                    `4-year - Public`,
                    `4-year - Private`,
                    `2-year - Public`,
                    `2-year - Private`) %>% select(Year, `Total enrollment`, type, value) %>% mutate(percent =
                                                                                                       value / `Total enrollment`) %>% ggplot(aes(x = factor(Year), y = percent, fill =
                                                                                                                                                    type)) + geom_bar(stat = "identity") + coord_polar() + theme(
                                                                                                                                                      axis.text.y = element_blank(),
                                                                                                                                                      axis.text.x = element_text(size = 12, face = "bold"),
                                                                                                                                                      panel.grid.major.x = element_blank(),
                                                                                                                                                      panel.grid.minor.x = element_blank(),
                                                                                                                                                      panel.grid.major.y = element_blank(),
                                                                                                                                                      panel.grid.minor.y = element_blank(),
                                                                                                                                                      legend.text = element_text(size = 12),
                                                                                                                                                      legend.title = element_text(size = 14),
                                                                                                                                                      legend.position = "bottom",
                                                                                                                                                      panel.background = element_rect(fill = "transparent"),
                                                                                                                                                      axis.ticks = element_blank()
                                                                                                                                                    ) + labs(title = "Percentage of Students Enrolling \nin Different Types of Schools (1976-2015)", caption = "@shiduopili\nData Source: The HBCU Foundation") +
  ylab("") + xlab("") + scale_fill_discrete(
    name = "Type of School",
    labels = c(
      "2-year Private",
      "2-year Public",
      "4-year Private",
      "4-year Public"
    )
  ) + theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold")) + theme(
    plot.background = element_rect(fill = "grey"),
    panel.background = element_blank(),
    legend.background = element_blank()
  )
