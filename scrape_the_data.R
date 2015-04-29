##########################################################
### This is all inspired by
### https://susanetlinger.wordpress.com/2015/03/19/sxsw15-redux-what-happens-at-sx-spreads-everywhere/
##########################################################

##########################################################
### Open issues:
### No data for 2009, 2007, and prior.  
### I have some issues around the edges of what counts as a single 
###   "event" (right now it's based on unique title for a certain 
###   day, but there definitely are yoga classes offered twice a 
###   day, and 12 hour long things listed under a single title, and so on).  
### No data on attendance, or really even a reliable way to classify 
###   different types of events, as the type codes seem a little iffy to me.
### More efficient storage???
##########################################################


##########################################################
### Set up the environment
##########################################################

# get the relevant libraries, functions, and options
library(RCurl)
library(XML)
trim <- function (x) gsub("^\\s+|\\s+$", "",gsub("\\s+", " ", x)) 
options(stringsAsFactors = FALSE)

##########################################################
### Start with list of schedule page urls for each day
##########################################################

# load the list of urls for sxsw programs
day_sched_urls <- read.table("URL_list.csv", header=TRUE, sep=",") 
day_sched_urls$Date <- as.Date(day_sched_urls$Date,"%m/%d/%y")
day_sched_urls$Type <- as.character(day_sched_urls$Type)
day_sched_urls$URL <- as.character(day_sched_urls$URL)
day_sched_urls$Year <- as.character(day_sched_urls$Date, "%Y")

# select dates where they use simple formatting, id'd by type for year 
# (this is selecting pages not events, will still get events of all types)
day_sched_urls <- 
  day_sched_urls[which(day_sched_urls$Type == 'P'| day_sched_urls$Type == 'I'), ];

# check for duplication in URLs, expect TRUE
length(day_sched_urls$URL) == length(unique(day_sched_urls$URL))

# get the formatting metadata
data_formats <- read.table("year_format.csv", header=TRUE, sep=",") 
data_formats$Year <- as.character(data_formats$Year)

##########################################################
### Identify html table rows by event title and loop through 
### page/tables and title/rows to get data
###
### I'm keeping all types for now, because I don't see a way
### to really accurately separate out interactive from the
### others, and I just want to focus on the word counts by 
### year.
##########################################################

#set up the shell data.frame for events
events <- data.frame(date = as.Date(character(0)),
                     title = character(0),
                     desc = character(0) ,
                     time = character(0),
                     type = character(0) ) 

## loop through page/tables/days in the schedule
for (a in row.names(day_sched_urls) ) {
  # get the overall schedule page for that day and convert into a tree
  pagetree <- htmlTreeParse(day_sched_urls[a,2], useInternalNodes = TRUE)
  print(day_sched_urls[a,2])
    
  # get titles which we'll use as an id to grab the rest of the data
  titles <- trim(xpathSApply(pagetree, "//tr/td[@class='title']/a/node()[not(self::span)]", xmlValue))
  # i would like to do this, but then it won't match what's on the page, and 
  # things come back null.  but this way there are still some dupes showing up.
     #titles <- gsub("–","_",titles,fixed='TRUE')
     #titles <- gsub("-","_",titles,fixed='TRUE')
     #titles <- iconv(titles, "latin1", "ASCII", sub="")
  titles <- gsub("'", "_", unique(titles), fixed='TRUE')
  
  # get the rest of the details for each event/table row on that day
  for (b in 1:length(titles)) {
  #b<-8 #for code testing
    # TIME
    time <- xpathSApply(pagetree, 
                        paste0("//tr[contains(.,'",titles[b],"')]/td[@class='time']"),
                        xmlValue)
    time <- paste(unique(trim(gsub("\r\n", "", time, fixed='TRUE'))), collapse=' <p> ')
    # TYPE
    type <- xpathSApply(pagetree, 
                   paste0("//tr[contains(.,'",titles[b],"')]/td[@class='type']"),
                   xmlValue)
    type <- paste(sort(unique(trim(type))), collapse='; ')
    # DESCRIPTION
    desc <- unique(xpathSApply(pagetree, 
                   paste0("//tr[contains(.,'",titles[b],"')]/td/div[@class='sched-description']"), 
                   xmlValue))
    desc <- trim(iconv(desc, "latin1", "ASCII", sub=""))
    desc <- gsub("View official listing on sxsw.com","",desc,fixed="TRUE")
    desc <- paste(desc, collapse=' <p> ')      
    # PUT IT ALL TOGETHER (one event = unique day+title, though some events occur more than 
    # one time in a given day, and many events of the same title repeat across days)
    event <- data.frame(date = day_sched_urls[a,1],
                        title = titles[b],
                        desc = desc,
                        time = time,
                        type = type
                        )
    # add each event to the list
    events <- rbind(events, event)
  } # events on a day
} # all days

# clean-up blank types at the beginning
events$type <- gsub("^\\; ", "",events$type) 

##########################################################
### look at duplicated names
# "Live Music Black Swan Yoga Recovery Sessions Powered by Onnit" is 
# happening every day, multiple times per day on the site
### for purposes of this analysis, I'm counting unique 
# title per day as one event.  types and descriptions and
# so on are collapsed within the title and day
##########################################################

str(events)
length(events$title)
length(unique(events$title))

# clean-up
remove(a)
remove(b)
remove(pagetree)
remove(event)
remove(desc)
remove(type)
remove(titles)
remove(time)

##########################################################
### save
##########################################################

# save the workspace
save.image("~/Desktop/sxsw_analysis/scraped_sxsw_event_data.RData")

##########################################################
### Now let's look at some data!
##########################################################

# Frequencies http://www.statmethods.net/stats/frequencies.html
table(events$type, as.character(events$date,'%Y')) 
#prop.table(yearbytype) # column percentages 

# 10 random events
events[sample(nrow(events), 10, replace=FALSE, prob=NULL),c(1,4,2,5)]







