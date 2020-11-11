
---
layout: post
title: Basic COVID Plots - Method
bigimg: 
---

To create my COVID trend maps I use R 3.6.1 and various packages, including `rvest` and `ggplot` to download data and visualize it, respectively.  My code for the county trend plots is below.


```
library(dplyr);library(rvest);library(ggplot2)
library(ggthemes);library(lubridate);library(ggdark);library(directlabels)
library(RCurl);library(grid)

usa<-getURL('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')
usa<-read.csv(text=usa)

# choose subset of states
usa2<-usa %>% dplyr::filter(state=="Pennsylvania" |  state=="Illinois"|state=="New York" |state=="Florida"| state=="Texas")

# county level analysis...
usa2<-usa2%>% arrange(state,county,date)

# format variables
usa2$date2<-as.Date(usa2$date,format="%Y-%m-%d")  # this is a county level file
usa2$caseincreaseCOUNTY<- ifelse(usa2$state==lag(usa2$state,1) & usa2$county==lag(usa2$county,1), usa2$cases-lag(usa2$cases,1),NA)
usa2$newcases<-ifelse(usa2$state==lag(usa2$state,1) ,
  usa2$cases-lag(usa2$cases,1),NA)

usa2$newdeaths<-ifelse(usa2$state==lag(usa2$state,1) ,
                      usa2$deaths-lag(usa2$deaths,1),NA)

covidpa<-usa2 %>%  filter(state=="Pennsylvania")
covidpa<-covidpa %>% arrange(county,date2) %>%
mutate(cases7daysago=lag(cases,7))

covidpa$casesPctchg<-ifelse(covidpa$county==lag(covidpa$county,7),
100*(covidpa$cases-covidpa$cases7daysago)/covidpa$cases7daysago,NA )

covidpa$pfill=ifelse(covidpa$casesPctchg>50,"red","white")
cc<-c("York", "Lancaster","Union",  "Cumberland","Dauphin")

temp<-subset(covidpa, covidpa$county %in% cc)
p<-ggplot(temp, aes(date2,as.numeric(caseincreaseCOUNTY)))+
  geom_line(aes(x=date2,y=rollmean(caseincreaseCOUNTY,7,na.pad=TRUE),
                color=factor(county)),size=.85)+
#  scale_fill_colorblind()+
  geom_vline(xintercept = as.Date("2020-05-30"),linetype="dashed",color='blue')+
  geom_vline(xintercept = as.Date("2020-07-04"),linetype="dashed",color='blue')+
  #theme_grey()+  scale_color_calc()+
  dark_theme_bw()+
  geom_dl(aes(label = county), 
          method = list(dl.trans(x = x + .013), "last.bumpup"), cex = 0.6) +   
  ylab("Number of New Cases (7 day ave)")+
  xlab(paste("Analysis by @bac3917\nData Source: NY Times COVID database as of ",Sys.Date()))+
  ggtitle("New Cases of COVID19 in Pennsylvania (7-day ave)")+
  theme(axis.text.x =element_text(size=9),axis.title.x=element_text(size=9),axis.title.y = element_text(size=9))+
  labs(color="County")

gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
```
