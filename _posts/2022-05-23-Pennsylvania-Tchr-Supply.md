---
layout: post
title: "PA Teacher Turnover"
---


## Background

Teacher supply is a major issue facing the education system.[^1] This report focuses on teacher supply components in Pennsylvania.

## Methods


### Data
The Pennsylvania Department of Education (PDE) provides public data files that include one records for each teacher, along with information about the LEA, and school the teacher was employed, certification area, etc.  

Data files are available from the PDE on their <a href="https://www.education.pa.gov/DataAndReporting/ProfSupPers/Pages/ProfPersIndStaff.aspx">Data and Reporting page</a>. Documentation for these data are available in PDE's <a href="https://www.education.pa.gov/Documents/Teachers-Administrators/PIMS/PIMS%20Manuals/2021-2022%20PIMS%20Manual%20Vol%201.pdf">PIMS Manual</a>.

In many cases, an individual teacher is included multiple times in the same year to reflect their assignments in different schools within a school district. Positions such as alternative education, computer science, art, ESL, and special education staff may teach in separate buildings in districts that need to allocate them as such. Only a teacher's 'primary assignment' record is included in the analysis file.

Individual teacher records for each of the years 2016-17, 2017-18, 2018-19, 2019-20 and 2020-21 were stacked into one data file holding nearly one million records. This file was filtered to include **teachers only**, thus, staff designated as 'Subcontracted,' 'Other Employee,' and 'Substitute' staff were not included in the analysis file. 

Many teachers appear more than once in a given year because both their "Primary" and other assignments are included. To ensure that a teacher appears only once per year, the analysis file includes only teachers' primary assignment,[^2] constituting about 80 percent of records. 

## Data Munging

```{r readFiles}
t16 <- read.csv("//filesvr01/yourpath/PDE/PubSchoolStaff/2016-17 Professional Personnel Individual Staff Report.csv")
colnames(t16)<-str_replace_all(colnames(t16)," ","")
colnames(t16)<-str_replace_all(colnames(t16),"[:punct:]","")
t16$year<-'16_17'
t16$PublicID<-as.character(t16$PublicID)
t16$PositionRecode<-ifelse(str_detect(t16$Position,"Teacher"),"Teacher",
                           ifelse(str_detect(t16$Position,"Administ"),"Admin",
                           t16$Position))
t16$YearsInED<-t16$YearsInEd
t16$PublicID<-as.numeric(t16$PublicID)  # subsequent years are numeric format
t16$AUN<-as.character(t16$AUN)
t16 %>% dplyr::filter(FirstName=="KAREN" & LastName=="KUNSA") %>%
  select(LastName,PublicID)

### 2017-18
t17 <- read_excel("Z:/yourpath/2017-18 Professional Personnel Individual Staff Report.xlsx")
colnames(t17)<-str_replace_all(colnames(t17)," ","")
colnames(t17)<-str_replace_all(colnames(t17),"-","")
t17$year<-'17_18'
myvars<-c("AnnualSalary","YearsInEd","YearsInLEA")
t17[myvars]<-makenum(t17,myvars) # convert to numeric format
t17$PositionDescription<-t17$PositionDescription2017
t17$PositionRecode<-ifelse(str_detect(t17$PositionDescription,"Teacher"),"Teacher",
                           ifelse(str_detect(t17$PositionDescription,"Administ"),"Admin",
                                  t17$PositionDescription))
t17$YearsInED<-t17$YearsInEd
t17$AUN<-as.character(t17$AUN)

### 2018-19
t18 <- read_excel("Z:/yourpath/2018-19 Professional Personnel Individual Staff Report.xlsx")
colnames(t18)<-str_replace_all(colnames(t18)," ","")
colnames(t18)<-str_replace_all(colnames(t18),"-","")
t18$year<-'18_19'
myvars<-c("AnnualSalary","YearsInED","YearsInLEA")
t18[myvars]<-makenum(t18,myvars)
t18$PositionDescription<-t18$PositionDescription2017
t18$PositionRecode<-ifelse(str_detect(t18$PositionDescription,"Teacher"),"Teacher",
                           ifelse(str_detect(t18$PositionDescription,"Administ"),"Admin",
                                  t18$PositionDescription))

t18$AUN<-as.character(t18$AUN)

### 2019-20
t19 <- read_excel("Z:/yourpath/2019-20 Professional Personnel Individual Staff Report.xlsx")
colnames(t19)<-str_replace_all(colnames(t19)," ","")
colnames(t19)<-str_replace_all(colnames(t19),"-","")
t19$year<-'19_20'
myvars<-c("AnnualSalary","YearsInED","YearsInLEA")
t19[myvars]<-makenum(t19,myvars)  # convert to numeric format
t19$PositionDescription<-t19$PositionDescription2017
t19$PositionRecode<-ifelse(str_detect(t19$PositionDescription,"Teacher"),"Teacher",
                           ifelse(str_detect(t19$PositionDescription,"Administ"),"Admin",
                                  t19$PositionDescription))


### more data
t20 <- read_excel("Z:/yourpath/2020-21 Professional Personnel Individual Staff Report.xlsx")
colnames(t20)<-str_replace_all(colnames(t20)," ","")
colnames(t20)<-str_replace_all(colnames(t20),"-","")
t20$year<-'20_21'
myvars<-c("AnnualSalary","YearsInED","YearsInLEA")
t20[myvars]<-makenum(t20,myvars) # convert to numeric format
t20$PositionDescription<-t20$PositionDescription2017
t20$PositionRecode<-ifelse(str_detect(t20$PositionDescription,"Teacher"),"Teacher",
                           ifelse(str_detect(t20$PositionDescription,"Administ"),"Admin",
                                  t20$PositionDescription))

# JobClass
# PE professional employee (tenured)
# TPE temporary professional employee (not tenured)
# SP Substitute
# SC Subcontracted 
# OE Other employee


```


## Stack Annual Files Into One with Selected Variables

```{r}
myvars<-c(
  "year", "FirstName","LastName","PublicID","YearsInED",
  "YearsInLEA","AUN","School",
  "PositionRecode","PrimaryAssignment","Status",  # not on earlier files
  "LEACountyCd", "JobClass"     )

# Primary Assignment is used once per teacher per LEA
# use to obtain one teacher per LEA
# t13 and t14 dont appear to have Primary Assignment
#t13a<-t13[myvars];t14a<-t14[myvars];t15a<-t15[myvars];
t16a<-t16[myvars];
t17a<-t17[myvars];
t18a<-t18[myvars];t19a<-t19[myvars];t20a<-t20[myvars]

# bind most recent years of data
df<-rbind(t16a,t17a,t18a,t19a,t20a);  rm(t13a,t14a,t15a,t16a,t17a,t18a,t19a,t20a)

df<-df %>% arrange(PublicID,year)  # 20 seconds

# clean Job Class
df$JobClass2<-
  ifelse(df$JobClass=="TPE","Temporary Professional Employee",
         ifelse(df$JobClass=="OE","Other Employee",
                ifelse(df$JobClass=="PE","Professional Employee",
                       ifelse(df$JobClass=="SC","Subcontracted Employee",
                              ifelse(df$JobClass=="",NA,
                                     df$JobClass)))))

df$YearsInLEA_cat<-ifelse(df$YearsInLEA==1,"One Year in LEA",
                          ifelse(df$YearsInLEA<5,"2-5 Years",
                                 ifelse(df$YearsInLEA<10,"5-10 Years",
                                        "10+ Years")))
df$YearsInLEA_cat<-factor(df$YearsInLEA_cat,
                          levels=c("One Year in LEA", "2-5 Years","5-10 Years","10+ Years"),ordered = T)


write.csv(df,"Z:/yourpath/paEducStaffRaw.csv")
```

The `paEducStaffRaw` file was also merged with district characteristics data from the National Center for Education Statistics using AUN.


## Analysis
This preliminary analysis begins with a simple question: *How many teachers remain in the staffing file from one year to the next?*


```{r}
fre(df$year) %>% 
  set_caption(tabtitle("Number of Teachers by Year")) %>% 
   htmlTable(., css.cell = c("width: 250px", # first column width
                              rep("width: 50px", ncol(.) - 1)) # other columns width
    )
```

To answer this question, I  queried whether a set of unique teacher IDs in one year appeared in a subsequent year. Specifically, the analysis below creates a set of `PublicID` values for each year and counts the number of teachers in that set is in a subsequent year. In 2016-17, there were just over nnn,000 teachers in the analysis file. How many of them continued in (Pennsylvania) in the second year?


```{r TrackUniqueIDs, echo=T}
# for each year, how many IDs disappear by the following year?
ID1617<-df %>% dplyr::filter(year=="16_17") %>% pull(PublicID) # create set of IDs for 1617
ID1718<-df %>% dplyr::filter(year=="17_18") %>% pull(PublicID) # create set of IDs for 1718
ID1819<-df %>% dplyr::filter(year=="18_19") %>% pull(PublicID)
ID1920<-df %>% dplyr::filter(year=="19_20") %>% pull(PublicID)

y1<-df %>% dplyr::filter(year=="16_17") # Cases for 1617
y2<-df %>% dplyr::filter(year=="17_18") # Cases for 1718 
y3<-df %>% dplyr::filter(year=="18_19") 
y4<-df %>% dplyr::filter(year=="19_20") 
y5<-df %>% dplyr::filter(year=="20_21") 

# for each year, how many IDs are new?
#table(ID1617 %in% y2$PublicID) # how many IDs from 1617 in 2017-18?
#table(ID1617 %in% y3$PublicID) # how many IDs from 1617 in 2018-19?
#table(ID1617 %in% y4$PublicID) # how many IDs from 1617 in 2019-20?

pct1617<-table(ID1617 %in% y5$PublicID) # how many IDs from 1617 in 2020-21?
pct1617<-(pct1617[1]/pct1617[2])*100 # pct in 1617 not in 2021
pct1718<-table(ID1718 %in% y5$PublicID) # how many IDs from 1718 appear in 2020-21?
pct1718<-(pct1718[1]/pct1718[2])*100 # pct in 1718 not in 2021
pct1819<-table(ID1819 %in% y5$PublicID) # how many IDs from 1819 in 2020-21?
pct1819<-(pct1819[1]/pct1819[2])*100 # pct in 1819 not in 2021
pct1920<-table(ID1920 %in% y5$PublicID) # how many IDs from 1920 in 2020-21?
pct1920<-(pct1920[1]/pct1920[2])*100 # pct in 1718 not in 2021

```


```{r, TchrMovementIndicators}
library(tidyverse);library(expss)
df<-read.csv("paEducStaffRaw_SortedTchrsOnly_Merge.csv")

df$sameID_sameNextAUN<-
  ifelse(df$PublicID==data.table::shift(df$PublicID,n=1,type='lead') &
           df$AUN==data.table::shift(df$AUN,n=1,type='lead'), "Stayer",
  ifelse(df$PublicID==data.table::shift(df$PublicID,n=1,type='lead') &
                  df$AUN!=data.table::shift(df$AUN,n=1,type='lead'), "Mover",
  ifelse(df$PublicID!=data.table::shift(df$PublicID,n=1,type='lead'),"Different Teacher (NA)",
         NA )))

df$StayMove_or_NA<-
  ifelse(df$PublicID==data.table::shift(df$PublicID,n=1,type='lead') & #next record must be same teacher
         df$AUN==data.table::shift(df$AUN,n=1,type='lead')  , 'Stayed in LEA',
         
  ifelse(df$PublicID==data.table::shift(df$PublicID,n=1,type='lead') &
         df$AUN!=data.table::shift(df$AUN,n=1,type='lead')  , 'Moved LEA',
  
  ifelse(df$PublicID!=data.table::shift(df$PublicID,n=1,type='lead'),"NA: Next Record Diff Tchr",
         NA)))

df$leaver<-
  ifelse(df$year=="2016_17" & df$sequence<5,"Left within 4 yrs",
         ifelse(df$year=="2017_18" & df$sequence<4,"Left within 3 yrs",     
         ifelse(df$year=="2018_19" & df$sequence<3,"Left within 2 yrs",
                ifelse(df$year=="2019_20" & df$sequence<2,"Left within 1 yrs","Other"))))

df=apply_labels(df,
                year="School Year",
                StayMove_or_NA="Teacher Status in Subsequent Year",
                charter="Charter School Status",hipov="High Poverty Status",
                 sameLEA2="Employment Status", YearsInLEA_cat="Yrs in LEA",
                changedLEA="Staff Changed LEA (look behind)", noReturn_paLEA="Staff Did Not Return (look ahead)")

```


I also created measures of whether an individual teacher moved to another district. Results to be reported subsequently.
