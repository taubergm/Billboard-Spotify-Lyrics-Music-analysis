# word blobs

if (!require(wordcloud)) {
  install.packages("wordcloud", repos="http://cran.us.r-project.org")
}
if (!require(tm)) {
  install.packages("tm", repos="http://cran.us.r-project.org")
}
if (!require(slam)) {
  install.packages("slam", repos="http://cran.us.r-project.org")
}
if (!require(SnowballC)) {
  install.packages("SnowballC", repos="http://cran.us.r-project.org")
}
if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}

library(slam)
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)

workingDir = '/Users/michaeltauberg/billboard'

#data_name = "songs_2000_2008"
#csvName = "songs_2000b_2008b.csv"

csvName = "songs_2009b_2017b.csv"
data_name = "2009-2017"

setwd(workingDir)

dt = read.csv(csvName)
dt$spotifyID = NULL
dt$spotifyLink = NULL
dt$videoLink = NULL

# get unique set of artist/songs
dt_uniq = dt[!duplicated(dt[,c('title','artist')], fromLast=FALSE),] #fromlast to get highest value in "weeks_on_list" field
dt_uniq = dt_uniq[order(dt_uniq$date),] 
#write.csv(dt_uniq, sprintf("%s_uniq.csv", data_name))

# get all instances where the weeks on the chart was the same (allows us to calculate total weeks on chart etc)
dt_weeks = dt[!duplicated(dt[,c('title','artist','weeks')], fromLast=FALSE),] #fromlast to get highest value in "weeks_on_list" field
dt_weeks$weeks = strtoi(dt_weeks$weeks)
dt_weeks = dt_weeks[order(dt_weeks$weeks, decreasing=TRUE),]
dt_weeks = dt_weeks[order(dt_weeks$title),]
dt_weeks_uniq = dt_weeks[!duplicated(dt_weeks[,c('title','artist')], fromLast=FALSE),]
head(dt_weeks_uniq, 10)

# this is a test
funk = dt_weeks[dt_weeks$title == "Uptown Funk!", ]  
funk = funk[funk$rank==1, ]
nrow(funk) # should be #1 for 14 weeks


# number ones -> this is flawed. need to capture from rank = 1, then calculate who has the most rows
ones = dt_weeks[dt_weeks$rank == 1,]
ones = ones[order(ones$title, decreasing=FALSE),]
ones_uniq = ones[ones$lastPos != '1', ]
ones_uniq = ones_uniq[ones_uniq$lastPos != '0', ]
ones_uniq = ones[!duplicated(ones[,c('title','artist','weeks')], fromLast=FALSE),]
#write.csv(ones_uniq, sprintf("%s_numberonehits.csv", data_name))

# Get how many weeks at number one by tabulating the ones songs
ones$title = droplevels(ones$title)
ones$artist = droplevels(ones$artist)
songs = table(unlist(ones$title))
write.csv(songs, sprintf("%s_numberonehits.csv", data_name))


# add number of words in the title (for analysis later)
dt_uniq_weeks$num_words_in_title = sapply(gregexpr("\\S+", dt_uniq_weeks$title), length)


GenerateWordClouds <- function(dt_uniq, data_name) {
  words = Corpus(VectorSource(dt_uniq$title)) 
  corpus <- tm_map(words, content_transformer(tolower))
  
  dtm <- DocumentTermMatrix(words)
  terms = findFreqTerms(dtm)
  print(sprintf("num words in titles is %s", length(terms)))
  common = findFreqTerms(dtm, 10)
  
  # Generate wordcloud only removing 'the', 'and', 'a'
  words = tm_map(words, stripWhitespace)
  words = tm_map(words, tolower)
  badwords = c("the", "and", "a")
  words = tm_map(words, removeWords, badwords)
  png(sprintf("%s_simple_wordcloud_you.png", data_name))
  #wordcloud(words, max.words = 120, random.order=FALSE, colors=brewer.pal(nrow(dt_uniq),"Dark2"))
  wordcloud(words, max.words = 120, random.order=FALSE)
  dev.off()
  
  # Generate wordcloud removing all stop words
  png(sprintf("%s_stopwords_wordcloud_100words_color.png", data_name),width=600,height=600)
  words = tm_map(words, removeWords, stopwords('english') )
  wordcloud(words, max.words = 100, random.order=FALSE, colors=brewer.pal(8,"Dark2"),width=1280,height=800)
  
  dev.off()
}

# Generate word clouds
GenerateWordClouds(dt_uniq, data_name)

# Count some popular words 

dt_uniq$weeks = strtoi(dt_uniq$weeks)
darks = dt_uniq[grep("\\bDark", dt_uniq$title, perl=TRUE), ]
girls = dt_uniq[grep("\\bGirl", dt_uniq$title, perl=TRUE), ]
loves = dt_uniq[grep("\\bLove", dt_uniq$title, perl=TRUE), ]
ones = dt_uniq[grepl("\\bOne", dt_uniq$title, perl=TRUE), ]


girl_weeks_on_list = sum(girls$weeks)
dark_weeks_on_list = sum(darks$weeks)


# order list to show the most popular books
dt_uniq = dt_uniq[order(dt_uniq$weeks, decreasing=TRUE),]
#head(dt_uniq)
dt_uniq$num_words_in_title = sapply(gregexpr("\\S+", dt_uniq$title), length)

number_one_hits = dt_uniq[dt_uniq$peakPos == 1,]

# create a histogram of the number of words in the title
hist(dt_uniq$num_words_in_title)
top_songs = head(dt_uniq, 25)
hist(top_songs$num_words_in_title)


p = ggplot(dt_uniq, aes(x=num_words_in_title, fill=num_words_in_title)) + geom_histogram(binwidth=0.5) 
p = p + ggtitle(sprintf("%s", data_name)) + theme(plot.title = element_text(size=14))
p = p + theme(axis.text.x=element_text(angle=75, hjust=1))
p = p + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Num Words in Song Title") + ylab("Num Songs") 
p = p + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
ggsave(filename = sprintf("./%s_numwords_histogram.png",data_name) , plot=p, width=4, height=4) 

# figure out how to plot the long tail of songs
dt_weeks_uniq = dt_weeks_uniq[order(dt_weeks_uniq$date, decreasing=TRUE),]
write.csv(dt_weeks_uniq, sprintf("%s_uniq.csv", data_name))

songtitle_weeks = aggregate(dt_weeks_uniq$weeks, by=list(title=dt_weeks_uniq$title), FUN=sum)
colnames(songtitle_weeks) = c("title", "weeks")
songtitle_weeks = songtitle_weeks[order(songtitle_weeks$weeks, decreasing=TRUE),]
songtitle_weeks$title = factor(songtitle_weeks$title, levels = songtitle_weeks$title[order(songtitle_weeks$weeks, decreasing=TRUE)]) 

# plot top 30
songtitle_weeks = songtitle_weeks[1:30,]
p = ggplot(songtitle_weeks, aes(x=title, y=weeks)) + geom_bar(stat="identity") 
p = p + ggtitle(sprintf("%s Top 30 songs curve", data_name))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Song") + ylab("Weeks on charts") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_song_weeks_top30_histogram.png",data_name) , plot=p, width=15, height=10)

dt_weeks_uniq = dt_weeks_uniq[order(dt_weeks_uniq$weeks, decreasing=TRUE),]
dt_weeks_uniq$title=seq(1,nrow(dt_weeks_uniq)) # get ridd of song titles for easy plotting
p = ggplot(dt_weeks_uniq, aes(x=title, y=weeks)) + geom_bar(stat="identity") 
p = p + ggtitle(sprintf("%s", data_name)) + theme(plot.title = element_text(size=24))
#p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Song") + ylab("Weeks on charts") 
p = p + scale_y_continuous(limits = c(0, 80)) + scale_x_continuous(limits = c(0, 4000))
ggsave(filename = sprintf("./%s_song_weeks_histogram.png",data_name) , plot=p, width=15, height=10) 

one_week_songs = dt_weeks_uniq[dt_weeks_uniq$weeks==1,]
print(nrow(one_week_songs))

# remove "featuring xxxx" from artists 
#also maybe think about cases like "Jay-Z, Rihanna & Kanye West" or "Jason Aldean With Kelly Clarkson" 
dt_weeks_uniq$artist = tolower(dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub(" featuring.*", "", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub(" with .*", "", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub(" & .*", "", dt_weeks_uniq$artist)
#dt_weeks_uniq$artist = gsub(" and .*", "", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub(" \/ .*", "", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub(" x .*", "", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub(", .*", "", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub(" duet.*", "", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub(" co-starring.*", "", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub("travi$", "travis", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub("jay z", "jay-z", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub("\\\"misdemeanor\\\"", "misdemeanor", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub(" + .*", "", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub("jay-z +.*", "", dt_weeks_uniq$artist)
dt_weeks_uniq$artist = gsub(" vs.*", "", dt_weeks_uniq$artist)

dt_weeks_uniq$artist = factor(dt_weeks_uniq$artist)

artist_weeks = aggregate(dt_weeks_uniq$weeks, by=list(artist=dt_weeks_uniq$artist), FUN=sum)
colnames(artist_weeks) = c("artist", "weeks")
artist_weeks = artist_weeks[order(artist_weeks$weeks, decreasing=TRUE),]

artist_weeks$artist = factor(artist_weeks$artist, levels = artist_weeks$artist[order(artist_weeks$weeks, decreasing=TRUE)]) 

#p = ggplot(artist_weeks, aes(x=artist, y=weeks)) + geom_bar(stat="identity") 
#p = p + ggtitle(sprintf("%s artist curve", data_name)) + theme(plot.title = element_text(size=24))
#p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
#p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
#p = p + xlab("Artist") + ylab("Weeks on charts") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
#ggsave(filename = sprintf("./%s_artist_weeks_histogram.png",data_name) , plot=p, width=15, height=10) 

# plot top 20
top_artist_weeks = artist_weeks[1:20,]
top_artist_weeks$artist = factor(top_artist_weeks$artist, levels = artist_weeks$artist[order(artist_weeks$weeks, decreasing=TRUE)]) 
p = ggplot(top_artist_weeks, aes(x=artist, y=weeks)) + geom_bar(stat="identity") 
p = p + ggtitle(sprintf("Top 20 artists %s", data_name)) + theme(plot.title = element_text(size=24))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,size=18)) + theme(plot.title = element_text(size=24))
p = p + theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"))
p = p + xlab("Artist") + ylab("Weeks on charts") 
#p = p + scale_y_continuous(limits = c(0, 1200)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_artist_weeks_top20_histogram.png",data_name) , plot=p, width=15, height=10) 

artist_weeks$artist=seq(1,nrow(artist_weeks)) # get ridd of song titles for easy plotting
p = ggplot(artist_weeks, aes(x=artist, y=weeks)) + geom_bar(stat="identity") 
p = p + ggtitle(sprintf("%s artists", data_name)) + theme(plot.title = element_text(size=24))
p = p + theme(axis.text=element_text(size=7), axis.title=element_text(size=14,face="bold"))
p = p + xlab("Artist") + ylab("Weeks on charts") 
#p = p + scale_y_continuous(limits = c(0, 1100)) + scale_x_continuous(limits = c(0, 1000))
ggsave(filename = sprintf("./%s_artist_weeks_histogram.png",data_name) , plot=p, width=15, height=10) 

