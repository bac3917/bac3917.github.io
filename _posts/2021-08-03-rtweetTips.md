---
layout: post
title: Quick Tips on Automated Twitter Analysis
---


Some colleagues recently wanted tips on how to automate analysis of one's Twitter data.  Read on for a basic 'how to'!

## Using rtweet to Download Twitter Data
The rtweet package is handy, although as free API, it limits a 7-day retrospective view on your activity.  The basic steps to follow are:

1. Create an app in your twitter account
2. Use the rtweet functions to download various data types
3. Format your data
4. Analyze at will!

```
# save data and tokens here:
library(tidyverse);library(dplyr);library(ROAuth);
library(rtweet)
library(expss);

mytoken<-create_token(
  app = "YOURAPPNAME",
  consumer_key = "YOURKEY ",
  consumer_secret = "YOURSECRET ",
  access_token = "YOURTOKEN",
  access_secret = "YOURACCESSSECRET")

mytoken

##  https://rtweet.info/   # handy info
```

Once you're authenticated you can use the variety of rtweet functions....

```
myfols <- get_followers("myaccountname")   # who follows me?

# if you want this to run automatically, this code/script needs to exist in a folder that Window TaskScheduler uses 
# later you will make a batch file (.bat) that TaskScheduler uses; I keep all of mine in this folder:
setwd("C:/R/batchfiles/TwitterDataArchive/")        

# what are your followers up do? returns 1000 users; of interest are retweets
tu<-lookup_users(myfols$user_id) 

# make a summary table/data frame
tu<-tu %>% group_by(screen_name) %>% 
  summarise(
    twDate=first(created_at), followers_count=mean(followers_count), 
    is_retweet=first(is_retweet), text=first(text),
    location=first(location)) 
  
# exact search for refs to your screen_name
bac_refs<-search_tweets("@bac")


# make a label for a subset of followers based on the mean followers value `fc`
fc<-mean(bac_refs$followers_count+0.5*sd(csc_refs$followers_count) )
bac_refs$label1<-
  ifelse(bac_refs$followers_count > fc,
         paste0("@",bac_refs$screen_name,": ", str_wrap(str_trunc(bac_refs$text, 50),20)),NA)

bac_refs$created_at<-as.Date(bac_refs$created_at)

library(ggrepel);library(scales)
p<-ggplot(bac_refs, aes(created_at, bac_refs$followers_count,label=label1))+
   geom_jitter(color='red')+
   theme_style1+  dark_theme_gray()+
   geom_text_repel(color='yellow',hjust=0, size=3, 
                  max.overlaps = Inf,
                  nudge_x = -2,nudge_y = -fc/4,
                  min.segment.length =0, segment.angle=30)+
  xlab("Date")+ylab("Account Holder Influence")+
  labs(title="Tweets in Last Seven Days Mentioning @bac",
       subtitle="Text shown for accounts with the most followers",
       caption=paste("Created:",Sys.time())  )

setwd("C:/R/batchfiles/")        # i want to save next file here
nn<-dim(bac_refs)[1] # number of records in file
if (nn>0) ggsave(plot=p,filename="RecentTweets.png") # save the file if there are data


setwd("C:/R/batchfiles/TwitterDataArchive/")        # this folder is to hold weekly tweet downloads
write_as_csv(bac_refs,paste("BACfollowers",format(Sys.time(), "%Y-%m-%d"), "csv", sep = "."),
             prepend_ids = TRUE,na='',fileEncoding = "UTF-8")
```

## Automating R Scripts

So, we created a Twitter app and used the login credentials to download our data. Once we've got this code working, we can automate its execution by doing the following:

1. Make a batch file 
2. Refer to batch file with Windows Task Scheduler


### Making a Batch File
A batch file is simply a text file with DOS commands. The batch files and the R-scripts you want to be automated should be in the same directory.

Here's a batch file that I use to automate the above script:

```
REM run the R script to download my Twitter data;
"C:\Program Files\R\R-4.0.4\bin\R.exe" CMD BATCH "C:\R\batchFiles\CSC_Twitter_Archiving.R"

```

What this means is that I made a text file, added the lines you see above, and saved it with a .bat  extension!
The REM is a command that let's us add comments
The second line of my batch file  (1) invokes R  and then (2) runs my script

Save the batch file in your selected directory.

### Using Task Scheduler
This is pretty straightforward, and I'll leave you with <a href="https://www.digitalcitizen.life/how-create-task-basic-task-wizard/">these directions.</a>
When you choose "Start a Program" in the steps, refer to your batch file.


```
##### META ANALYSIS ######
# After you've run the above at least twice, you can group your datasets 
myfiles<-list.files("C:/R/batchFiles/TwitterDataArchive",pattern = "CSCfo") # make a list of files
library(plyr)
setwd("C:/R/batchFiles/TwitterDataArchive")
dataset <- ldply(myfiles, read.table, header=TRUE, sep=",") # added manually and this checks out
dataset<-as.data.table(dataset) 
setkey(dataset,created_at)
detach(package:plyr) # important! otherwise you'll have conflicts with dplyr

library(ggrepel)
temp<-dataset %>% group_by(screen_name,created_at) %>% 
  summarise(n=n(),lasttext=last(text),nfollowers=as.numeric(max(followers_count)))

p2<-ggplot(temp,aes(created_at, n,label=screen_name))+
  geom_jitter(size=temp$nfollowers^.3,alpha=.4,width = .1,height = .1 )+
  geom_text_repel(color='yellow',nudge_y = -.25,size=3)+
  ylab("Number of mentions")+xlab("Date")+
  ylim(0,5)+
  labs(title="Twitter accounts mentioning @bac",
       subtitle="Point size is proportional to influence of account holder",
       caption="Some weeks missing if mentions are zero.")+
  theme_style1

setwd("C:/R/batchfiles/")        
ggsave(plot=p2,filename="TwitterAttention.png") # save the file if there are data

```
