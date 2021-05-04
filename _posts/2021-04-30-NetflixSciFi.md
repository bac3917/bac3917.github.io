---
layout: post
title: Netflix and Sci-fi
bigimg:
---


Not sure whether it is Netflix or the Saturn Awards that is overlooking better sci-fi from Asia, but given the number of films created in Asia, I think something has to change!  For instance, one IMDB list includes over 400 sci-fi films from <a href="https://www.imdb.com/search/title/?countries=kr&genres=sci_fi&sort=moviemeter">South Korea</a>. Another Wikipedia list includes 114 sci-fi films from <a href="https://en.wikipedia.org/wiki/Category:Japanese_science_fiction_films">Japan</a>. What are Vietnamese, Chinese, Thai or other Asian film makers creating? How can they be recognized?

Using some recent data archived on Kaggle, and a recent <a href="https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-20/readme.md">Tidy Tuesday</a> post showing how to utilize it, I plotted a sample of Netflix releases along with information from the Satturn Awards about the films nominated for their quality.  The scatterplot shows that the intersection in Asian countries is not representative.

![netflix_scifi](/img/netflix_scifi.jpeg){: .center-block :}      

You can reproduce the graph using the code below, along with <a href="https://raw.githubusercontent.com/bac3917/Cauldron/master/saturnWinners.csv">these data</a> from the Saturn Awards.

```{r}
rm(list=ls())
library(lubridate)
library(dplyr);library(ggplot2);library(scales);
library(ggdark);library(stringr)

# Data
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-20/readme.md

nf <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
cr <- readr::read_csv("https://raw.githubusercontent.com/bac3917/Cauldron/master/countries_regions.csv")
cr$country<-as.character(cr$country)
cr$country2<-ifelse(cr$country=="Korea, Democratic People's Republic of","South Korea",
                  ifelse(cr$country=="Russian Federation", "Russia",
                    cr$country))

nf$country2<- str_extract(nf$country, "[^,]+")

sat<-readr::read_csv('https://raw.githubusercontent.com/bac3917/Cauldron/master/saturnWinners.csv')
satList<-as.list(sat$CleanTitle)

nf<-left_join(nf,cr,by="country2")
summary(nf)

ls(nf)
nf$g_scifi<-ifelse(str_detect(nf$listed_in,"Sci-Fi"),1,0)
nf$date_added2<-as.Date(nf$date_added, format="%b %d, %Y")
nf$yearadded<-year(nf$date_added2 )
nf$minutes<-ifelse(str_detect(nf$duration,"min"),
                   as.numeric(gsub("min", "", nf$duration)),
                   NA)
nf$minsize=ifelse(nf$minutes>88,10,ifelse(nf$minutes>150,20,5))

nf$country<-as.factor(nf$country)


nf$satwinner<-ifelse(nf$title %in% satList,1,0)
nf$satLabel<-ifelse(nf$satwinner==1,nf$title,NA)
nf$satLabel<- str_sub(nf$satLabel,1,19)
nf %>% forcats::fct_lump(nf$country, 7,other_level="Other Country")
nf$Rating<-as.factor(ifelse(str_detect(nf$rating,"R") | nf$rating=="TV-MA","TV-MA/R/NR",
                   ifelse(str_detect(nf$rating,"Y") | nf$rating=="G" | str_detect(nf$rating,"TV-G"),"TV-Y/G", 
                          ifelse(str_detect(nf$rating,"PG") |str_detect(nf$rating,"14"),"TV-PG/13/14",
                          nf$rating )))
)
nf$Rating<-factor(nf$Rating,levels=c("TV-Y/G", "TV-PG/13/14","TV-MA/R/NR"),ordered = T)
temp<-nf %>% filter(g_scifi==1) %>% filter(!is.na(Region))

# not sure if netflix or the Saturn Awards are overlooking the better sci-fi from Asia, but something has to change!
ggplot(temp, aes(x=date_added2,y=Region, color=Rating)) +
  geom_jitter(alpha=.3,height = .25,size=3)+
  scale_color_manual(values=c("green", "yellow","red"))+
  ylab("Production Region")+xlab("Year Added")+
  scale_x_date(date_breaks = "1 year", 
               labels=date_format("%Y"),
               limits = as.Date(c('2013-01-01','2022-01-01')))+
  labs(title="Netflix Releases of Sci-Fi Movies by Region",subtitle = "Saturn Award Nominees Labeled")+
  ggrepel::geom_text_repel(min.segment.length = .5,
                           nudge_y =ifelse(temp$rating2=="TV-MA/R/NR", .75,-.7), 
                           nudge_x =ifelse(year(temp$date_added2)>2018, 3,-.2), size=3, direction='both',
                           max.overlaps = Inf, label=temp$satLabel,
                           show.legend = FALSE)+
  ggdark::dark_mode()


```
