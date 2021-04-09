raw_bechdel <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv'
  )
movies <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv'
  )


install.packages("gggibbous")
install.packages("wesanderson")
install.packages("ggtext")
install.packages("extrafont")

library(gggibbous)
library(dplyr)
library(ggplot2)
library(sjmisc)
library(wesanderson)
library(ggtext)
library(extrafont)

raw_bechdel %>%
  filter(year >= 1900 & year < 2020) %>%
  mutate(year_group = group_var(year, size = 10)) %>%
  mutate(fail = ifelse(rating == 3, "Pass", "Fail")) %>%
  group_by(year_group, fail) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n)) %>%
  mutate(ratio = n / total,
         right = if_else(fail == "Pass", TRUE, FALSE)) %>%
  ungroup() %>%
  mutate(year_group = as.factor(year_group)) %>%
  ggplot(aes(
    year_group,
    y = 0,
    ratio = ratio,
    right = right,
    fill = right,
    size = total
  )) +
  geom_moon() + coord_fixed(ylim = c(-1, 1)) + scale_size(range = c(15, 40)) +
  scale_x_discrete(
    "Year",
    labels = c(
      "1900-1909",
      "1910-1919",
      "1920-1929",
      "1930-1939",
      "1940-1949",
      "1950-1959",
      "1960-1969",
      "1970-1979",
      "1980-1989",
      "1990-1999",
      "2000-2009",
      "2010-2019"
    )
  ) + labs(title = "**Bechdel Test over Years**",
           subtitle = "Pass criteria: (1) has at least **two named women** (2) who **talk with each other** (3) about **anything but a man**",
           caption = "Large circles indicate more data points\n@shiduopili\nData Source: fivethirtyeight") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.title = element_markdown(
      hjust = 0.5,
      size = 18,
      margin = margin(0, 30, 0, 0)
    ),
    plot.subtitle = element_markdown(
      hjust = 0.5,
      vjust = 2,
      size = 12,
      margin = margin(50, 30, 0, 0)
    ),
  ) + guides(color = guide_legend("right"), size = "none") + scale_fill_manual(
    name = "",
    labels = c("Fail", "Pass"),
    values = wes_palette("Darjeeling1")
  )
