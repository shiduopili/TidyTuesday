georgia_pop <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv'
  )
library(dplyr)
library(tidyverse)
library(ggpubr)
georgia_pop %>% gather(type, value, Colored:White) %>% ggplot(aes(factor(Year), value, group =
                                                                    type)) + geom_line(aes(linetype = type)) + coord_flip() + scale_y_reverse(breaks = seq(0, 100, by = 5)) +
  ylab("") + xlab("") + grids(
    axis = c("xy"),
    color = "red",
    size = NULL,
    linetype = NULL
  ) + labs(title = "COMPARATIVE INCREASE OF WHITE AND COLORED\n POPULATION OF GEORGIA.", caption = "@shiduopili\nData Source: #DuBoisChallenge") + theme(plot.title = element_text(
    size = 14,
    hjust = 0.5,
    face = "bold",
    margin = margin(0.5, 0, 0, 0, unit = "line")
  )) + theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(fill = "peachpuff2"),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "salmon2", size = 0.5),
    panel.ontop = TRUE,
    legend.background = element_blank(),
    plot.subtitle = element_text(margin = margin(1.5, 0, 0, 0, unit = "line")),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5
    ),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.spacing.x = unit(1.0, 'cm')
  )

