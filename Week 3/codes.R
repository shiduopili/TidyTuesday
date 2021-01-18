artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")


install.packages("wesanderson")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(wesanderson)
##read the variable list to see what we got here

##Question: 
##at what age did the artist made the work

##join the dataset
art<-artwork %>% left_join(artists %>% rename(artistId=id),by="artistId")

##clean the dataset a little bit
art<-art %>% select(-dateText,-dimensions:-url.x,-dates,-url.y) %>% filter(artistRole=="artist")


#calculate the age of publishing and time between make and acquisition
art<-art %>% mutate(age_work=year-yearOfBirth)

art <- art %>% 
  mutate(
    year_group = cut(acquisitionYear, breaks = seq(1819.5, 2020.5, 20),
                     labels = paste0(seq(1820, 2010, 20), "'s"),
                     ordered_result = TRUE)
  )

art %>% filter(artist!="Turner, Joseph Mallord William",!is.na(gender),!is.na(age_work),!is.na(year_group)) %>% ggplot(aes(age_work,y=year_group,fill=gender)) +geom_density_ridges(scale=3,color = "white", size=0.4,alpha=0.6) +
  labs(title = 'The age at which the art works were created*',
    subtitle = "The plot demonstrates the density of the ages at which the artists \ncreated their works that were acquired by TATE over years. While the \nTATE started to collect more works from female artists, the ages of \nfemale artists seem to be a bit younger than the males.",
    caption = "*Works by JMW Turner (1775–1851) were excluded\nData from github.com/tategallery/collection",
    x = NULL
  ) + scale_fill_manual(values = wes_palette("Royal1"))+scale_x_continuous(breaks=seq(0,100,10))+scale_y_discrete(labels=c("1820-1839","1840-1859","1860-1879","1880-1899","1900-1919","1920-1939","1940-1959","1960-1979","1980-1999","after 2000"))+theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",legend.title  = element_blank(),legend.text = element_text(size=10,color = "#f8f8f2"),legend.margin = margin(t=-10),
    plot.background = element_rect(fill = wes_palette("Rushmore"), color = wes_palette("Rushmore")),
    plot.title = element_text(color = "#f8f8f2", size = 20, margin = margin(0, 0, 0.8, 0, unit = "line")),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "#f8f8f2", size = 14, lineheight = 1.2, margin = margin(0, 0, 0.8, 0, unit = "line")),
    plot.caption = element_text(margin = margin(0.5, 0, 1, 0, unit = "line"),color = "#f8f8f2",size = 9, lineheight = 1.1),
    plot.margin = margin(1.5, 1.0, 1, 1.0, unit = "line"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(),
    axis.text.x = element_text(margin = margin(-1, 0, 1, 0, unit = "line"))
  )+ coord_fixed(ratio=9)