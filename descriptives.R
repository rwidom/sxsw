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

# directory for the project
#DIR <- "/Users/rebeccawidom/Desktop/sxsw_analysis"
#setwd(DIR)

# load the dataset
load("scraped_sxsw_event_data.RData")
str(events)
length(events$title)
length(unique(events$title))

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
total_frequency <- apply(m,2,sum)
total_frequency <- sort(total_frequency[which(total_frequency>50)],
                        decreasing=TRUE) 
length(total_frequency)

##########################################################
### FIND TERM FREQUENCY BY YEAR
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
str(year_vector)
sum(year_vector)
remove(x)
# year matrix: column is the year, row is the doc
year_m <- Matrix(year_vector,
                 nrow = length(events$date),
                 ncol = length(years),
                 dimnames=list(row.names(events),years))
str(year_m)
sum(year_m)
colSums(year_m)
remove(year_vector)

# word totals by year
total_freq_by_year <- t(crossprod_simple_triplet_matrix(year_m,m))

#####################################################################
### further look at some key words
#####################################################################
keywords <- sort(c("data","ethic","collabor","privaci","surveil","insight",
              "civic","civil","machin","market","polit","metric",
              "social","make","cloud","women"))
# removed: tech, social, truth, hack, crew, code, coder, make, companion, cloud
# removed: commun [used for communication as well as community]
# removed: women, sex, sister, mother, 
# removed: "commerci","committe","grid",
table(year(events$date))
total_freq_by_year[keywords,]

# list the document titles, for a skim check of reasonableness
events[keywords_docs,2]

#####################################################################
### REPEAT WITH DESCRIPTIONS
#####################################################################

# create a document term matrix of words based on DESCRIPTIONS
m <- create_matrix(events[,3], language="english", minDocFreq=1, maxDocFreq=Inf,
                   minWordLength=3, maxWordLength=Inf, ngramLength=1, 
                   originalMatrix=NULL,
                   removeNumbers=TRUE, removePunctuation=TRUE, removeStopwords=TRUE, 
                   stemWords=TRUE, stripWhitespace=TRUE, toLower=TRUE) 
str(m)

# years and year_m exist from titles
total_freq_by_year_desc <- t(crossprod_simple_triplet_matrix(year_m,m))
total_freq_by_year_desc[keywords,]

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

events[c(documents("empath",m_desc),1429),c(1:2)]
