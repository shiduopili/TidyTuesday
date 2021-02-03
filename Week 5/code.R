library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(png)
library(grid)
library(ggimage)
plastics <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv'
  )

img <- readPNG("HK.png")

plastics %>% gather(type, value, hdpe:ps) %>% filter(parent_company == "Grand Total") %>% select(country, year, type, value) %>% filter(type !=
                                                                                                                                          "o") %>% ggplot(aes(type, value, fill = factor(type))) +
  annotation_custom(rasterGrob(img,
                               width = unit(1, "npc"),
                               height = unit(1, "npc")),-Inf, Inf,-Inf, Inf) +
  geom_violin(width = 1, size = 1, trim = FALSE) + scale_fill_discrete(
    name = "Type of Plastics",
    labels = c(
      "High density polyethylene",
      "Low density polyethylene",
      "Polyester plastic",
      "Polypropylene",
      "Polystyrene"
    )
  ) + ylab("Count") + xlab("") + theme(
    legend.position = c(0.8, 0.8),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18)
  ) + labs(title = "Distribution of Different Types of Plastics in 2019", caption = "@shiduopili\nData Source: Break Free from Plastic") +
  theme(plot.title = element_text(
    margin = margin(b = -35),
    size = 36,
    colour  = "red",
    hjust = 0.5,
    face = "bold"
  )) + theme(
    plot.background = element_rect(fill = "grey85"),
    panel.background = element_rect(fill = "white"),
    legend.background = element_blank()
  )
