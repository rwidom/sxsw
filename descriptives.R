##########################################################
### Get some basic descriptives
##########################################################

##########################################################
### Set up the environment
##########################################################

# get the relevant libraries
library(tm)
library(RTextTools)
library(textir)
library(slam)
year <- function (x) as.integer(as.character(x,'%Y')) 

# load the dataset
load("scraped_sxsw_event_data.RData")
str(events)
length(events$title)
length(unique(events$title))

#####################################################################
### function to list documents with a particular word-stem, useful for 
### additional spot checking 
#####################################################################
documents <- function (keywords, m) {
  keywords_index <- vector(mode = "logical",length=length(m$dimnames$Terms))
  keywords_index[1:length(m$dimnames$Terms)] <- NA 
  keywords_index[match(keywords, m$dimnames$Terms)] <- 1
  keywords_docs <- rollup(m, 2L, keywords_index, FUN=sum, na.rm = TRUE, DROP = TRUE)
  keywords_docs <- unique(keywords_docs$i)
  return(keywords_docs)
}

#####################################################################
### some key words of interest for this project
#####################################################################
keywords <- sort(c("data","ethic","collabor","privaci","surveil","insight",
                   "civic","civil","machin","market","polit","metric",
                   "social","make","cloud","women","media"))
# removed: tech, social, truth, hack, crew, code, coder, companion, cloud
# removed: commun [used for communication as well as community]
# removed: women, sex, sister, mother, 
# removed: "commerci","committe","grid",

##########################################################
### YEAR MATRIX TO FIND FREQUENCIES BY YEAR
##########################################################
# rows are documents, and columns are words
# so if I transpose the word vector, and multiply by a vertical vector of 0s and 1s
# for each year, then I get a vector of the total for each word, for only those 
# documents for that year.  My twenty-years-after-linear-algebra notes to myself, forgive me.

# years
years <- unique(year(events$date))
# values for matrix of document id's by year (one long vector here)
year_vector <- vector(mode = "logical", length = 0)
for (x in 1:length(years)) {
  year_vector <- c(year_vector,year(events$date)==years[x])
}
rm(x)

# year matrix: column is the year, row is the doc
year_m <- Matrix(year_vector,
                 nrow = length(events$date),
                 ncol = length(years),
                 dimnames=list(row.names(events),years))
rm(year_vector)

##########################################################
### DENOMINATOR FOR EACH YEAR, # of occurrences of the
##########################################################
year_denom <- data.frame(title=rep(0,length(years)), 
                         desc=rep(0,length(years)),
                         row.names = years)
for (y in 1:length(years)) {
  year_denom$desc[y]  <- length(grep("\\<the\\>", 
                               events$desc[which(year(events$date)==years[y])]))
  year_denom$title[y] <- length(grep("\\<the\\>", 
                               events$title[which(year(events$date)==years[y])]))
}

##########################################################
### FIND THE TOP TERMS OVERALL FROM TITLES
##########################################################

# create a document term matrix of words based on TITLES
m <- create_matrix(events[,2], language="english", minDocFreq=1, maxDocFreq=Inf,
                   minWordLength=3, maxWordLength=Inf, ngramLength=1, 
                   originalMatrix=NULL,
                   removeNumbers=TRUE, removePunctuation=TRUE, removeStopwords=TRUE, 
                   stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE) 
str(m)

# find the top 20 terms overall (using 1 in apply is wrong, = longest title)
title_freq <- apply(m,2,sum)
title_freq <- sort(title_freq[which(title_freq>50)], decreasing=TRUE) 
length(title_freq)
rm(title_freq)

##########################################################
### FIND TERM FREQUENCY FOR Title BY YEAR
##########################################################
# rows are documents, and columns are words
# so if I transpose the word vector, and multiply by a vertical vector of 0s and 1s
# for each year, then I get a vector of the total for each word, for only those 
# documents for that year.  My twenty-years-after-linear-algebra notes to myself, forgive me.

# word totals by year
title_freq_by_year <- t(crossprod_simple_triplet_matrix(year_m,m))
table(year(events$date))
title_freq_by_year[keywords,]
title_freq_by_year[keywords,] / rev(year_denom$title)

# list the document titles, for a skim check of reasonableness
events[documents(keywords,m),2]

# clean-up
rm(m)

#####################################################################
### REPEAT WITH DESCRIPTIONS
#####################################################################

# create a document term matrix of words based on DESCRIPTIONS
m <- create_matrix(events[,3], language="english", 
                   minDocFreq=1, maxDocFreq=Inf,
                   minWordLength=4, maxWordLength=Inf, 
                   ngramLength=1, originalMatrix=NULL,
                   removeNumbers=TRUE, removePunctuation=TRUE, 
                   removeStopwords=TRUE, stemWords=TRUE, 
                   stripWhitespace=TRUE, toLower=TRUE) 
str(m)

# find the top 20 terms overall 
#  1 in apply would be longest title
#  rowSums from base is not for simple triplet matrix
desc_freq <- apply(m,2,sum) 
desc_freq <- sort(desc_freq, decreasing=TRUE) 
desc_freq[1:20]
desc_freq[keywords]

length(title_freq)
rm(title_freq)

# years and year_m exist from titles
desc_freq_by_year <- t(crossprod_simple_triplet_matrix(year_m,m))
desc_freq_by_year[keywords,]
desc_freq_by_year[keywords,] / rev(year_denom$desc)

events[c(documents("empath",m),1429),c(1:2)]

#####################################################################
### SAVE FREQUENCIES FOR GRAPHING
#####################################################################

object.size(title_freq_by_year)
object.size(desc_freq_by_year)

title_freq_by_year <- title_freq_by_year[which(rowSums(title_freq_by_year)>1),]
desc_freq_by_year <- desc_freq_by_year[which(rowSums(desc_freq_by_year)>5),]

object.size(title_freq_by_year)
object.size(desc_freq_by_year)

# save the workspace
save(list=c("title_freq_by_year","desc_freq_by_year","keywords","year_denom"),
     file="term_freqs_by_year.RData")




