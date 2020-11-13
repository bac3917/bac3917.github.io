---
layout: post
title: COVID and College in the Commonwealth
bigimg:
---


![COVIDscatterplot](/img/covidcollege1.jpg){: .center-block :}      


```
library(RCurl)

x<-getURL("https://raw.githubusercontent.com/bac3917/Cauldron/master/covidIHE.csv")
x<-read.csv(text=x)
x$infectionsPerEnroll<-round((x$infections/x$EFYTOTLT),digits=3)
x$SECTOR<-factor(x$SECTOR,levels=c(1,2),labels=c("Public","Private"))

#shorten names
x$inst<-str_replace(x$inst,"University","U.")
x$inst<-str_replace(x$inst,"Pennsylvania","Penna.")

# PLOT 1
x$INSTNM2<-str_replace(x$INSTNM,"University","U")
x2<-x %>% filter(infectionsPerEnroll>.084 | infectionsPerEnroll<.019)
p1<-ggplot(x2, aes(reorder(INSTNM2, infectionsPerEnroll),infectionsPerEnroll,fill=factor(SECTOR)))+
  geom_bar(stat="identity")+ylab("COVID19 Infections per FT Enrollment\nSources: New York Times and NCES/IPEDS")+
  scale_fill_calc()+xlab("Institution")+
  theme_minimal()+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size=7))+
  coord_flip()+ggtitle("Top and Bottom Quartile COVID Infection Rates \nin Pennsylvania IHEs")
p1
```


```
#PLOT 2 - 
library(ggrepel)
x$label1<-ifelse(x$infectionsPerEnroll>.042,paste0(x$inst,"; Rate=",x$infectionsPerEnroll),NA)
summary(x$infectionsPerEnroll)

# All labels should be to the right of 3.
x_limits <- c(11000, NA)
y_limits <- c(400, NA)


p2<-ggplot(x, aes(EFYTOTLT,infections,color=SECTOR))+
  geom_point(alpha=.4)+geom_smooth(se=FALSE,method="lm")+
  ylab("Number of COVID19 Infections")+
  xlab("Full-time Enrollment (EFYTOTLT)")+
  scale_color_calc()+
  geom_label_repel(data=x,label=x$label1,size=2,
                   segment.size = .2,direction = "y",
                   xlim = x_limits, ylim = y_limits)+ # put labels in specific area
  theme_minimal()+
  #annotate(geom="text",x=25000,y=200,label="Names shown for schools with \nrates greater 2SDs over mean.",
  #           hjust=0, size=3)+
  annotate(geom="text",x=33000,y=10, label="Sources: New York Times and NCES/IPEDS   Analysis by @bac3917",size=2.5)+
  theme(legend.title = element_blank(),
        axis.title.x =  element_text(size=11))+
  #  facet_wrap(~SECTOR)+
  ggtitle("Correlation of Campus COVID-19 Cases and \nEnrollment in Pennsylvania by IHE Sector")
p2
```
