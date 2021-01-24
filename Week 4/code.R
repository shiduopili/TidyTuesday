households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')
install.packages("tidytuesdayR")
library(tidytuesdayR)
remotes::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus)
library(tidyverse)

households %>% filter(County != "Kenya") %>% ggplot(aes(x = reorder(County, AverageHouseholdSize), y =
                                                          AverageHouseholdSize)) +
  geom_point(
    size = 3.1,
    color = "green4",
    fill = alpha("orange", 0.3),
    alpha = 0.7,
    shape = 21,
    stroke = 2
  ) +
  geom_segment(
    aes(
      x = County,
      xend = County,
      y = 0,
      yend = AverageHouseholdSize
    ),
    color = "tomato",
    alpha = 0.4
  ) + geom_text(
    color = "black",
    size = 1.9,
    hjust = 0.5,
    aes(label = sprintf(
      "%0.1f", round(AverageHouseholdSize, digits = 1)
    ))
  ) + ggtitle("Average Household Size in Kenya by County") + theme_ridges() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    plot.title = element_text(size = 20, margin = margin(t = 10, b = -20)),
    plot.caption = element_text(size = 10)
  ) + ylab("") + xlab("") +
  geom_text(
    label = "Kenya average = 3.9",
    size = 4,
    x = 16,
    y = 3.9,
    vjust = -1,
    color = "black"
  ) + labs(caption = "@shiduopili \nData: rKenyaCensus")