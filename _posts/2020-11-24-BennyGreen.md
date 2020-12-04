---
layout: post
title: Benny Green and jazzviz
bigimg:
---

Time for more <a target="_blank" href="https://twitter.com/hashtag/jazzviz?src=hashtag_click">#jazzviz</a>.  This time I'm taking a look at pianist Benny Green and a subset of his network.  I define a musician's network as the individuals who are named as performing artists on their published albums. To gather those data, I use the discogs API and various R packages. Note that I do this imperfectly!  I haven't yet mastered lists and `purrr` to clean up my act.  This stands in contrast to Benny Green, who's act is fantastic! Just listen to his albums, or see him live when this pandemic ends.

![BennyGreenNetwork](/img/bennygreennetwork112420.jpeg){: .center-block :}      

How'd I create this network plot?  Basically, <a target="_blank" href="https://www.discogs.com/">discogs.com</a> permits you to download their massive music dataset which includes information about every album and all of the personnel on them. It gets a little more complicated, especially when you need to use a tiny bit of combinatorics to make a list of every possible album-personnel interaction. For all the details, see my R code below. And if you have improvements to offer, please let me know. I still can't seem to grab all the data I want from discogs (several Benny Green albums aren't referenced in the network plot), so I need to improve my technique.

```
library(jsonlite)
library(discogger);library(tidyverse)

# download all releases associated with Benny Green
dc<-discogs_artist_releases(96442) #Bennygreen
dc<-dc$content  # Take a look at the main data frame and clean it up
dc2<-dc %>%   filter(role=="Main" ) %>%  group_by(title) %>% 
  summarise(    release_code=first(main_release),    year=first(year),
    role=first(role),type=first(type),    label=first(label),    in_wantlist_sum=sum(stats.community.in_wantlist) )

rlist<-as.list(dc2$release_code)  # create a list of releases for this artist
rlist<-Filter(Negate(anyNA), rlist) # remove NAs from this list

# define a function that iteratively downloads release data
fullReleaseData<-list()  # make a blank list of full release data
fullRelease <- function(rlist){
  for(i in 1: length(rlist) ){
    frset <- fromJSON(paste0("https://api.discogs.com/releases/", as.character(rlist[i])))
    message("Retrieving page ", i)
    Sys.sleep(2) # deal with rate limit
    fullReleaseData[[i+1]] <- as.data.frame(cbind(frset$id, frset$title,frset$year,
                                                  frset$artists$name,frset$artists$id,
                                                  frset$extraartists$name,
                                                  frset$extraartists$role))    }
  return(fullReleaseData)
  message("Total rows=", dim(fullReleaseData[[2]])[1] )    }

# now use the `fullRelease` function!
albumdata<-fullRelease(rlist[13]) 

#save results as csv (improve this process)
lapply(albumdata[1:length(albumdata)], function(x) write.table( data.frame(x), 
    paste0('K:/...path.../jazzalbums_',albumdata[[2]][[4]][1],length(albumdata)-1,'.csv'), 
    append= T, sep=',' ))
```

The remainder of my code performs some data cleaning, and then applies the <a target="_blank" href="https://igraph.org/r/">igraph</a> package to make the network plot.
