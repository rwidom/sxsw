##########################################################
### Graphs and Examples
##########################################################

library(wordcloud)
library(RColorBrewer)

load("term_freqs_by_year.RData")
load("scraped_sxsw_event_data.RData")
rm(data_formats)
rm(day_sched_urls)
rm(trim)

years <- as.integer(colnames(desc_freq_by_year))
year <- function(d) {as.character(d,"YYYY")}

# list results for keywords
# title_freq_by_year[keywords,] / rev(year_denom$title)
# desc_freq_by_year[keywords,] / rev(year_denom$desc)

# word clouds by year (title)
pal <- brewer.pal(6, "Blues")
pal <- pal[-(1:3)]
for (y in year(years)) {
  png(paste0("wordcloud_",y,"_title.png"), width=1280,height=800)
  d <- title_freq_by_year[which(title_freq_by_year[,y]/year_denom[y,1]>.1),y]/year_denom[y,1]
  d <- data.frame(word=names(d), freq=as.vector(d))
  wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
  dev.off()
}

# word clouds by year (title)
pal <- brewer.pal(6, "Greens")
pal <- pal[-(1:3)]
for (y in year(years)) {
  png(paste0("wordcloud_",y,"_desc.png"), width=1280, height=800)
  d <- desc_freq_by_year[which(desc_freq_by_year[,y]/year_denom[y,2]>.1),y]/year_denom[y,2]
  d <- data.frame(word=names(d), freq=as.vector(d))
  wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
  dev.off()
}
rm(y)

# line graphs for themed keywords
keywords <- c("collabor","data","ethic","privaci","surveil","women")

# make a bar plot
pal <- brewer.pal(6, "Dark2")
kw_freq <- desc_freq_by_year[keywords,] / rev(year_denom$desc)
barplot(kw_freq,
        main="Key Word Frequencies in Descriptions",
        ylab="Number of Mentions, adjusted by 'the'",
        legend = rownames(kw_freq), beside=TRUE,
        col = pal)

kw_freq <- title_freq_by_year[keywords,] / rev(year_denom$title)
barplot(kw_freq,
        main="Key Word Frequencies in Titles",
        ylab="Number of Mentions, adjusted by 'the'",
        legend = rownames(kw_freq), beside=TRUE,
        col = pal)

# line plots
x <- colnames(kw_freq)
par(mfrow=c(1,1))
mypch <- as.vector(1:length(rownames(t(kw_freq))))
mypch <- mypch + 13

kw_freq <- desc_freq_by_year[keywords,] / rev(year_denom$desc)
matplot(x, t(kw_freq), type="b",col=pal,lty=1,lwd=4,
        ylab="Number of Mentions in Descriptions, adjusted by 'the'",
        xlab="Year (Impute 2009)",
        pch = mypch)
legend("right", inset=.03, legend=rownames(kw_freq), 
       col=pal, lwd=4, horiz=FALSE, pch = mypch)

kw_freq <- title_freq_by_year[keywords,] / rev(year_denom$title)
matplot(x, t(kw_freq), type="b",col=pal,lty=1,lwd=4,
        ylab="Number of Mentions in Titles, adjusted by 'the'",
        xlab="Year (Impute 2009)",
        pch = mypch)
legend("right", inset=.03, legend=rownames(kw_freq), 
       col=pal, lwd=4, horiz=FALSE, pch = mypch)

