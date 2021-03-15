earn <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv'
  )

library(zoo)
apply(earn, 2, table)
earn$yearq <- paste(earn$year, earn$quarter, sep = "q")
earn$quarteryear <- as.yearqtr(earn$yearq, format = "%Yq%q")
earn$qvar <- as.Date(earn$quarteryear)
earn %>% filter(age == "55 years and over", race != "All Races", sex != "Both Sexes") %>% 
ggplot(aes(qvar, median_weekly_earn, group =race)) + 
geom_line(aes(linetype = race, col = sex)) + 
facet_grid(. ~ sex) + 
scale_x_date(date_labels = "%Y %b",limits = c(as.Date("2010-01-01"), as.Date("2020-10-01")),
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "Top",
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 0.5
    )
  ) + ylab("Median Weekly Earn  (dollars)") + xlab("Time")
