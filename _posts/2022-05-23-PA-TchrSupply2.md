type: post
title: PA Teacher Turnover
================

## Background

Teacher supply is a major issue facing the education system.\[^1\] This
report focuses on teacher supply components in Pennsylvania.

## Methods

### Data

The Pennsylvania Department of Education (PDE) provides public data
files that list teachers in public schools across the state. Because
these teachers are provided with a unique identifier, and the data are
available across multiple years, it is possible to examine teacher
turnover of the Pennsylvania public teacher workforce.

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

## Data Preparation

The code below does the following:

1.  Read each year of PDE staffing data and format variables
    consistently in each year
2.  Stack each year of data into one file
3.  Save the file as CSV for analysis

The `paEducStaffRaw` file was also merged with district characteristics
data from the National Center for Education Statistics using AUN.

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

 

### Analytic Variables

The analysis uses variables that classify teachers’ movement or
attrition from an LEA. I used definitions from the National Center for
Education Statistics to create key analytic variables for whether a
teacher was a “leaver,” a “mover,” or a “stayer.” Specifically, from the
<a href="https://ies.ed.gov/ncee/edlabs/regions/northeast/pdf/REL_2021080.pdf">,report</a>
*Analyzing Teacher Mobility and Retention: Guidance and Considerations
Report 1* these definitions are:

-   Leavers. Teachers who left their initial LEA between any two years
    and did not enter another LEA during this timeframe.
-   Movers. Teachers who moved from their initial LEA to a different LEA
    in any two year period. The LEA (the LEA a teacher is employed
    within) that determines whether a teacher is a mover might differ
    from the one used to determine whether a teacher is a leaver . For
    example, a mover might be defined as a teacher who moves between
    LEAs, whereas a leaver might be defined as a teacher who is no
    longer teaching in the state. Note that a shortcoming of this
    analysis is that teachers who move to another state are not
    accounted for.
-   Stayers. Teachers who remained teaching in their initial LEA in any
    two year period. *Further work needed on this variable!*

``` r
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

    ## Warning in apply_labels.list(data, ...): Some names don't exist in `data`:
    ## charter, hipov, sameLEA2, changedLEA, noReturn_paLEA

 

## Analysis

This preliminary analysis begins with a simple question: *How many
teachers remain in the staffing file from one year to the next?*

The answer to this question is best addressed by first examining the
number of teachers in each year of the dataset.

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<td colspan="6" style="text-align: left;">
Table 1: Number of Teachers by Year
</td>
</tr>
<tr>
<th style="border-bottom: 1px solid grey; font-weight: 900; border-top: 2px solid grey; width: 250px; text-align: center;">
School Year
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

  

We can also see the mobility status for all the teachers in the
datafile:

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<td colspan="2" style="text-align: left;">
Table 2: Teachers’ Mobility Status by Year
</td>
</tr>
<tr>
<th style="border-top: 2px solid grey;">
</th>
<th colspan="1" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
 df$StayMove_or_NA 
</th>
</tr>
<tr>
<th style="border-bottom: 1px solid grey; font-weight: 900; width: 250px; text-align: center;">
</th>
<th style="font-weight: 900; border-bottom: 1px solid grey; text-align: center;">
 Moved LEA 
</th>
</tr>
</thead>
<tbody>
<tr>
<td colspan="2" style="width: 250px; font-weight: 900;">
 School Year 
</td>
</tr>
<tr>
<td style="width: 250px; text-align: left;">
   16_17 
</td>
<td style="width: 50px; text-align: right;">
160454
</td>
</tr>
<tr>
<td style="width: 250px; text-align: left;">
   17_18 
</td>
<td style="width: 50px; text-align: right;">
159029
</td>
</tr>
<tr>
<td style="width: 250px; text-align: left;">
   18_19 
</td>
<td style="width: 50px; text-align: right;">
159845
</td>
</tr>
<tr>
<td style="width: 250px; text-align: left;">
   19_20 
</td>
<td style="width: 50px; text-align: right;">
163401
</td>
</tr>
<tr>
<td style="width: 250px; text-align: left;">
   20_21 
</td>
<td style="width: 50px; text-align: right;">
29608
</td>
</tr>
<tr>
<td style="width: 250px; border-bottom: 2px solid grey; text-align: left;">
   #Total cases 
</td>
<td style="width: 50px; border-bottom: 2px solid grey; text-align: right;">
672337
</td>
</tr>
</tbody>
</table>

 

Next, I queried whether a set of unique teacher IDs in one year appeared
in a subsequent year. Specifically, the analysis below creates a set of
`PublicID` values for each year and counts the number of teachers in
that set to the values in a subsequent year using the following code:

`ID1617<-df %>% dplyr::filter(year=="16_17") %>% pull(PublicID) # create set of IDs for 1617`

`y2<-df %>% dplyr::filter(year=="17_18") # create the set of cases for 1718`

`table(ID1617 %in% y2$PublicID) # how many IDs from 1617 in 2017-18?`

 

For example, in 2016-17, there were 170855 teachers in the analysis file
and about 5.5390146 percent “left” teaching in Pennsylvania by 2017-18.

|    Year     | % Not Continuing |
|:-----------:|------------------|
| **pct1617** | 5.5390146        |
| **pct1718** | 5.1252296        |
| **pct1819** | 5.1730296        |
| **pct1920** | 5.0525819        |
