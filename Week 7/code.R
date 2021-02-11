retirement <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv'
  )

library(tidyverse)
empty_bar <- 4
to_add <-
  data.frame(matrix(NA, empty_bar * nlevels(factor(retirement$race)), ncol(retirement)))
colnames(to_add) <- colnames(retirement)
to_add$race <- rep(levels(factor(retirement$race)), each = empty_bar)
retirement <- rbind(retirement, to_add)
retirement <- retirement %>% arrange(race)
retirement$id <- seq(1, nrow(retirement))

label_data <- retirement
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 0, 1)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

base_data <- retirement %>%
  group_by(race) %>%
  summarize(start = min(id), end = max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title = mean(c(start, end)))

grid_data <- base_data
grid_data$end <-
  grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1, ]

library(wesanderson)
ggplot(retirement, aes(x = as.factor(id), y = retirement, fill = race)) +
  geom_bar(stat = "identity",
           alpha = 0.6,
           width = 1) +
  theme_minimal() + ylim(-50000, 190000) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  geom_segment(
    data = grid_data,
    aes(
      x = end,
      y = 160000,
      xend = start,
      yend = 160000
    ),
    colour = "grey",
    alpha = 1,
    size = 0.3 ,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = grid_data,
    aes(
      x = end,
      y = 120000,
      xend = start,
      yend = 120000
    ),
    colour = "grey",
    alpha = 1,
    size = 0.3 ,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = grid_data,
    aes(
      x = end,
      y = 80000,
      xend = start,
      yend = 80000
    ),
    colour = "grey",
    alpha = 1,
    size = 0.3 ,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = grid_data,
    aes(
      x = end,
      y = 40000,
      xend = start,
      yend = 40000
    ),
    colour = "grey",
    alpha = 1,
    size = 0.3 ,
    inherit.aes = FALSE
  ) + annotate(
    "text",
    x = rep(max(retirement$id), 4),
    y = c(40000, 80000, 120000, 160000),
    label = c("40000", "80000", "120000", "160000") ,
    color = "grey",
    size = 3 ,
    angle = 0,
    fontface = "bold",
    hjust = 1
  ) +
  coord_polar() +
  geom_text(
    data = label_data,
    aes(
      x = id,
      y = retirement + 30000,
      label = year,
      hjust = hjust
    ),
    color = "black",
    fontface = "bold",
    alpha = 0.6,
    size = 2.5,
    angle = label_data$angle,
    inherit.aes = FALSE
  ) + theme(
    legend.title = element_blank(),
    legend.position = c(0.65, 0.25),
    legend.direction = "horizontal"
  ) + scale_fill_manual(values = wes_palette("Darjeeling1", n = 3)) + 
  labs(title = "Average Family Liquid Retirement \nSavings Over Years by Race",
  subtitle = "Besides the fact that White families have much higher savings \nat retirement than Hispanic and Black families, there has been a \nsteadily growing trend over years for the White, but not for the \nother two racial groups.",
  caption = "@shiduopili\nData Source: Urban Institute & U.S. Census") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5,face = "bold", 
  margin = margin(0.5, 0, 0, 0, unit = "line"))) + 
  theme(plot.background = element_rect(fill = "white"),
  panel.background = element_blank(), 
  legend.background = element_blank(),
  plot.subtitle = element_text(margin = margin(1.5, 0, 0, 0, unit = "line")) )
