##build Shiny App

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(stringr)
library(scales)

games <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv'
  )
games <-
  games %>% mutate(date = ym(paste(year, month, sep = " ")))  %>% mutate(avg_peak_perc =
                                                                           as.numeric(str_sub(avg_peak_perc, end = -2))) 

#define ui
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  #Application title
  titlePanel("Games' Trend"),
  
  sidebarLayout(
    position = "right",
    #Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput(
        inputId = "type",
        label = strong("Select Game"),
        choices = unique(games$gamename),
        selected = "Dota 2"
      ),
      #select data range to be plotted
      dateRangeInput(
        "date",
        strong("Date Range"),
        start = min(games$date),
        end = max(games$date),
        min = min(games$date),
        max = max(games$date)
      ),
      #   #select whether to overlay smooth trend line
      #   checkboxGroupInput(inputId = "smoother",label = strong("Overlay smooth trend line")),
      #   #display only if the smoother is checked
      #   conditionalPanel(condition = "input.smoother==TRUE",
      #                    sliderInput(inputId = "f",
      #                                label = "Smoother span:",min = 0.01,max = 1,value = 0.67,step = 0.01,animate = animationOptions(interval = 100)),
      #                    HTML("Higher values give more smoothness."))
    ),
    
    #output: lineplot
    mainPanel(plotOutput(outputId = "lineplot", height = "300px"))
  )
)



#define server function
server <- function(input, output) {
  datasetInput <- reactive({
    # req(input$date)
    # validate(need(!is.na(input$date[1]) & !is.na(input$date[2]),"Error:please both a start and an end date."))
    # validate(need(input$date[1]<input$date[2],"Error: Start date should be earlier than end date."))
    games %>%
      filter(
        gamename == input$type,
        date > as.POSIXct(input$date[1]) &
          date < as.POSIXct(input$date[2])
      )
  })
  
  
  #Create scatterplot object the plotOutput function is expecting
  
  output$lineplot <- renderPlot({
    dataset <- datasetInput()
    p1 <- ggplot(
      dataset %>% select(gamename, date, avg, peak, gain) %>% gather(type, value,-gamename,-date),
      aes(x = date, y = value, color = type)
    ) + geom_line(size = 2) + xlab("") + ylab("Value") + scale_color_discrete(labels =
                                                                                c("Average", "Gains", "Peak")) + theme_dark() + theme(
                                                                                  legend.title = element_blank(),
                                                                                  legend.position = "bottom",
                                                                                  plot.background = element_rect(fill = "grey")
                                                                                ) + scale_x_date(labels = date_format("%m-%Y"))
    
    p2 <-
      ggplot(dataset, aes(x = date, y = as.numeric(avg_peak_perc))) + geom_line(size =
                                                                                  2) + theme_dark() + ylab("Percentage of Average in Peak") + scale_x_date(labels = date_format("%m-%Y"))
    grid.arrange(p1, p2, ncol = 2)
    
    # color = "#434343"
    # par(mar = c(4, 4, 1, 1))
    # plot(x = selected_trends()$date, y = selected_trends()$gain, type = "l",
    #      xlab = "Date", ylab = "Gain", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    # if(input$smoother){
    #   smooth_curve <- lowess(x = as.numeric(datasetInput()$date), y = datasetInput()$gain, f = input$f)
    #   lines(smooth_curve, col = "#E6553A", lwd = 3)
    # }
  })
}




shinyApp(ui = ui, server = server)
