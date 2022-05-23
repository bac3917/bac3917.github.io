PA Teacher Turnover
================

-   [Background](#background)
-   [Methods](#methods)
-   [Data Munging](#data-munging)
-   [Analysis](#analysis)

## Background

Teacher supply is a major issue facing the education system.\[^1\] This
report focuses on teacher supply components in Pennsylvania.

## Methods

### Data

The Pennsylvania Department of Education (PDE) provides public data
files that include one records for each teacher, along with information
about the LEA, and school the teacher was employed, certification area,
etc.

Data files are available from the PDE on their
<a href="https://www.education.pa.gov/DataAndReporting/ProfSupPers/Pages/ProfPersIndStaff.aspx">Data
and Reporting page</a>. Documentation for these data are available in
PDE’s
<a href="https://www.education.pa.gov/Documents/Teachers-Administrators/PIMS/PIMS%20Manuals/2021-2022%20PIMS%20Manual%20Vol%201.pdf">PIMS
Manual</a>.

In many cases, an individual teacher is included multiple times in the
same year to reflect their assignments in different schools within a
school district. Positions such as alternative education, computer
science, art, ESL, and special education staff may teach in separate
buildings in districts that need to allocate them as such. Only a
teacher’s ‘primary assignment’ record is included in the analysis file.
\[^2\]

Individual teacher records for each of the years 2016-17, 2017-18,
2018-19, 2019-20 and 2020-21 were stacked into one data file holding
nearly one million records. This file was filtered to include **teachers
only**, thus, staff designated as ‘Subcontracted,’ ‘Other Employee,’ and
‘Substitute’ staff were not included in the analysis file.

## Data Munging

The code below does the following:

1.  Read each year of PDE staffing data and format variables
    consistently in each year
2.  Stack each year of data into one file
3.  Save the file as CSV for analysis

**Read data files** (two examples provided)

``` r
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
```

 

**Stack annual files** A subset of variables is included in the stacked,
multiyear file.

``` r
myvars<-c(
  "year", "FirstName","LastName","PublicID","YearsInED",
  "YearsInLEA","AUN","School",
  "PositionRecode","PrimaryAssignment","Status",  # not on earlier files
  "LEACountyCd", "JobClass"     )

t16a<-t16[myvars];
t17a<-t17[myvars];
t18a<-t18[myvars];t19a<-t19[myvars];t20a<-t20[myvars]

# bind most recent years of data
df<-rbind(t16a,t17a,t18a,t19a,t20a);  rm(t13a,t14a,t15a,t16a,t17a,t18a,t19a,t20a)

df<-df %>% arrange(PublicID,year)  # sort order is very important
```

 

**Create analytic variables** that classify teachers’ movement or
attrition from an LEA.

``` r
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
```

 

The `paEducStaffRaw` file was also merged with district characteristics
data from the National Center for Education Statistics using AUN.

## Analysis

This preliminary analysis begins with a simple question: *How many
teachers remain in the staffing file from one year to the next?*

Examine the number of teachers in the analysis file by year:

    ## [1] "Use %strin% function to search for partial matches"
    ## [1] "Use rstat function to create inline stats"
    ## [1] "The makenum function converts a list of vectors to numeric"
    ## [1] "schstat just needs a partial LEA name for input"
    ## [1] "The ief function recodes 6-point agreement scale"

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<td colspan="6" style="text-align: left;">
Table 1: Number of Teachers by Year
</td>
</tr>
<tr>
<th style="border-bottom: 1px solid grey; font-weight: 900; border-top: 2px solid grey; width: 250px; text-align: center;">
df$year
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
 Count 
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
 Valid percent 
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
 Percent 
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
 Responses, % 
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
 Cumulative responses, % 
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="width: 250px; text-align: left;">
 16_17 
</td>
<td style="width: 50px; text-align: right;">
170855
</td>
<td style="width: 50px; text-align: right;">
19.8
</td>
<td style="width: 50px; text-align: right;">
19.8
</td>
<td style="width: 50px; text-align: right;">
19.8
</td>
<td style="width: 50px; text-align: right;">
19.8
</td>
</tr>
<tr>
<td style="width: 250px; text-align: left;">
 17_18 
</td>
<td style="width: 50px; text-align: right;">
169403
</td>
<td style="width: 50px; text-align: right;">
19.7
</td>
<td style="width: 50px; text-align: right;">
19.7
</td>
<td style="width: 50px; text-align: right;">
19.7
</td>
<td style="width: 50px; text-align: right;">
39.5
</td>
</tr>
<tr>
<td style="width: 250px; text-align: left;">
 18_19 
</td>
<td style="width: 50px; text-align: right;">
170801
</td>
<td style="width: 50px; text-align: right;">
19.8
</td>
<td style="width: 50px; text-align: right;">
19.8
</td>
<td style="width: 50px; text-align: right;">
19.8
</td>
<td style="width: 50px; text-align: right;">
59.4
</td>
</tr>
<tr>
<td style="width: 250px; text-align: left;">
 19_20 
</td>
<td style="width: 50px; text-align: right;">
173716
</td>
<td style="width: 50px; text-align: right;">
20.2
</td>
<td style="width: 50px; text-align: right;">
20.2
</td>
<td style="width: 50px; text-align: right;">
20.2
</td>
<td style="width: 50px; text-align: right;">
79.5
</td>
</tr>
<tr>
<td style="width: 250px; text-align: left;">
 20_21 
</td>
<td style="width: 50px; text-align: right;">
176281
</td>
<td style="width: 50px; text-align: right;">
20.5
</td>
<td style="width: 50px; text-align: right;">
20.5
</td>
<td style="width: 50px; text-align: right;">
20.5
</td>
<td style="width: 50px; text-align: right;">
100.0
</td>
</tr>
<tr>
<td style="width: 250px; text-align: left;">
 #Total 
</td>
<td style="width: 50px; text-align: right;">
861056
</td>
<td style="width: 50px; text-align: right;">
100
</td>
<td style="width: 50px; text-align: right;">
100
</td>
<td style="width: 50px; text-align: right;">
100
</td>
<td style="width: 50px; text-align: right;">
</td>
</tr>
<tr>
<td style="width: 250px; border-bottom: 2px solid grey; text-align: left;">
 \<NA\> 
</td>
<td style="width: 50px; border-bottom: 2px solid grey; text-align: right;">
0
</td>
<td style="width: 50px; border-bottom: 2px solid grey; text-align: right;">
</td>
<td style="width: 50px; border-bottom: 2px solid grey; text-align: right;">
0.0
</td>
<td style="width: 50px; border-bottom: 2px solid grey; text-align: right;">
</td>
<td style="width: 50px; border-bottom: 2px solid grey; text-align: right;">
</td>
</tr>
</tbody>
</table>

To answer this question, I queried whether a set of unique teacher IDs
in one year appeared in a subsequent year. Specifically, the analysis
below creates a set of `PublicID` values for each year and counts the
number of teachers in that set is in a subsequent year. In 2016-17,
there were just over nnn,000 teachers in the analysis file. How many of
them continued in (Pennsylvania) in the second year?

``` r
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

t<-rbind(pct1617,pct1718,pct1819,pct1920)
```

|    Year     | % Not Continuing |
|:-----------:|------------------|
| **pct1617** | 19.24            |
| **pct1718** | 14.25            |
| **pct1819** | 9.702            |
| **pct1920** | 5.053            |

I also created measures of whether an individual teacher moved to
another district. Results to be reported subsequently.

    ## Warning in apply_labels.list(data, ...): Some names don't exist in `data`:
    ## charter, hipov, sameLEA2, changedLEA, noReturn_paLEA
