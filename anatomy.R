
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

library(jsonlite)
library(slam)
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)
library(data.table)

workingDir = '/Users/michaeltauberg/billboard/'
csvName = "songs_18yrs_spotify_data_v5.csv"
data_name = "hits"
setwd(workingDir)

dt = read.csv(csvName, encoding="UTF-8")
# identify a very small number duplicate artist/song from bad billboard data
dt = dt[!duplicated(dt[,c('title','artist')], fromLast=FALSE),]
dt$duration_ms = as.numeric(as.character(dt$duration_ms))
dt$num_words_in_song_title = sapply(gregexpr("\\S+", dt$title), length)
dt$num_words_in_song = sapply(gregexpr("\\S+", dt$lyrics), length)
dt$num_words_in_song_title = as.numeric(as.character(dt$num_words_in_song_title))
dt$num_words_in_song = as.numeric(as.character(dt$num_words_in_song))
dt$danceability = as.numeric(as.character(dt$danceability))
dt$valence = as.numeric(as.character(dt$valence))
dt$energy = as.numeric(as.character(dt$energy))
dt$tempo = as.numeric(as.character(dt$tempo))
dt$duration = dt$duration_ms/1000/60


ones = dt[dt$peak_pos == 1,]
ones = ones[order(ones$artist, decreasing=TRUE),]

write.csv(ones, "hits.csv")

# fancy weeks pie chart - is this necessary or can I just use straigh percentages?
genres = c()
weeks_total = sum(dt_year$weeks)
  for (broad_genre in levels(droplevels(ones$broad_genre))) {
    ones_genre = ones[ones$broad_genre==broad_genre,]
    genre_weeks_total = sum(ones_genre$weeks)
    stats_row = c(broad_genre,genre_weeks_total,weeks_total,genre_weeks_total/weeks_total*100 )
    genres = rbind(genres, stats_row)
  }
genres = as.data.table(genres)
genres = as.data.frame(genres)
colnames(genres) = c("genre","weeks_genre","weeks","percent_genre")
genres$percent_genre=as.numeric(genres$percent_genre)
genres$genre[genres$genre == "unknown"] = "misc"

pop = ones[ones$broad_genre=="pop",]
rap = ones[ones$broad_genre=="rap",]
rock = ones[ones$broad_genre=="rock",]
rb = ones[ones$broad_genre=="r&b",]
country = ones[ones$broad_genre=="country",]
edm = ones[ones$broad_genre=="edm",]

genres = c()
total = nrow(ones)
for (broad_genre in levels(droplevels(ones$broad_genre))) {
  ones_genre = ones[ones$broad_genre==broad_genre,]
  genre_total = nrow(ones_genre)
  stats_row = c(broad_genre,genre_total,total,genre_total/total*100 )
  genres = rbind(genres, stats_row)
}
genres = as.data.table(genres)
genres = as.data.frame(genres)
colnames(genres) = c("genre","num_genre","total","percent_genre")
genres$percent_genre=as.numeric(genres$percent_genre)
genres$genre[genres$genre == "unknown"] = "misc"


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x=element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
p = ggplot(genres, aes(x="", y=percent_genre, fill=genre)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) + scale_fill_brewer(palette="Dark2") 
p = p + blank_theme 
p = p + ggtitle("Genre of Hit Songs")
ggsave(filename = "./plots/hits_genre_piechart.png", plot=p, width=6, height=6) 

p = ggplot(ones, aes(x="", y=time_signature, fill=time_signature)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) + scale_fill_brewer(palette="Dark2") 
p = p + blank_theme 
p = p + ggtitle("Time Signature of Hit Songs")
percent_4 = nrow(ones[ones$time_signature == 4,])/nrow(ones)*100
ggsave(filename = "./plots/hits_timesig_piechart.png", plot=p, width=6, height=6) 

dt_time = dt[dt$time_signature != "unknown",]
p = ggplot(dt_time, aes(x="", y=time_signature, fill=time_signature)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) + scale_fill_brewer(palette="Dark2") 
p = p + blank_theme 
p = p + ggtitle("Time Signature of All Billboard Songs")
percent_4 = nrow(dt_time[dt_time$time_signature == 4,])/nrow(dt_time)*100
ggsave(filename = "./plots/all_timesig_piechart.png", plot=p, width=6, height=6) 


ones$num_words_in_song_title = sapply(gregexpr("\\S+", ones$title), length)
ones$num_words_in_song = sapply(gregexpr("\\S+", ones$lyrics), length)
ones$num_words_in_song_title = as.numeric(as.character(ones$num_words_in_song_title))
ones$num_words_in_song = as.numeric(as.character(ones$num_words_in_song))
ones$duration_ms = as.numeric(as.character(ones$duration_ms))
ones$danceability = as.numeric(as.character(ones$danceability))
ones$valence = as.numeric(as.character(ones$valence))
ones$energy = as.numeric(as.character(ones$energy))
ones$tempo = as.numeric(as.character(ones$tempo))
ones$duration = ones$duration_ms/1000/60

hist(ones$num_words_in_song_title)
hist(ones$num_words_in_song)
# number of distinct words
p = ggplot(ones, aes(x=duration)) 
#p = p + geom_histogram(binwidth=5) 
p = p + geom_histogram(aes(y = ..density..), binwidth=0.1)
p = p + ggtitle("duration histogram") + theme(plot.title = element_text(size=14))
p = p + theme(axis.text.x=element_text(angle=75, hjust=1))
p = p + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
p = p + xlab("duration (minutes)") + ylab("density") 
#p = p + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
#p = p + stat_function(fun = dnorm, args = list(mean = mean(ones$duration), sd=sd(ones$duration)), colour = "red",)
#p = p + geom_vline(xintercept = mean(ones$duration, na.rm=TRUE), colour="red")
p = p + stat_density(geom="line", color="red")
ggsave(filename = sprintf("./plots/%s_duration_histogram.png",data_name) , plot=p, width=8, height=5) 

median(ones$duration) # 231s = 3.85 min

ones$valence = as.numeric(ones$valence)
p = ggplot(ones, aes(x=valence)) + geom_histogram() 
p = p + ggtitle("valence histogram") + theme(plot.title = element_text(size=14))
p = p + theme(axis.text.x=element_text(angle=75, hjust=1))
p = p + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
p = p + xlab("valence") + ylab("num songs") 
#p = p + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
p = p +  stat_density(geom="line", color="red")
ggsave(filename = sprintf("./plots/%s_valence_histogram.png",data_name) , plot=p, width=15, height=10) 

ones$energy = as.numeric(ones$energy)
p = ggplot(ones, aes(x=energy)) + geom_histogram() 
p = p + ggtitle("energy histogram") + theme(plot.title = element_text(size=14))
p = p + theme(axis.text.x=element_text(angle=75, hjust=1))
p = p + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
p = p + xlab("energy") + ylab("num songs") 
#p = p + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
p = p + stat_density(geom="line", color="red")
ggsave(filename = sprintf("./plots/%s_energy_histogram.png",data_name) , plot=p, width=15, height=10) 

#p = ggplot(dt, aes(x=duration)) 
#p = p + geom_histogram(data = ones, fill = "red", alpha = 0.2, binwidth=5, aes(y = ..density..)) 
#p = p + geom_histogram(data = dt, fill = "blue", alpha = 0.2, binwidth=5, aes(y = ..density..)) 
#p = p + stat_function(fun = dnorm, args = list(mean = mean(ones$duration, na.rm=TRUE), sd=sd(ones$duration,na.rm=TRUE)), colour = "red")
#p = p + stat_function(fun = dnorm, args = list(mean = mean(dt$duration,na.rm=TRUE), sd=sd(dt$duration,na.rm=TRUE)), colour = "blue")
#p = p + stat_density(geom="line", color="green")
#ggsave(filename = sprintf("./plots/%s_all_duration_histogram.png",data_name) , plot=p, width=15, height=10) 

p = ggplot(dt, aes(x=num_words_in_song)) 
p = p + geom_histogram(data = ones, fill = "red", alpha = 0.2, aes(y = ..density..), binwidth=20) 
p = p + geom_histogram(data = dt, fill = "blue", alpha = 0.2, aes(y = ..density..), binwidth=20) 
#geom_histogram(data = highf0, fill = "green", alpha = 0.2) +
#p = p + stat_function(fun = dnorm, args = list(mean = mean(ones$num_words_in_song, na.rm=TRUE), sd=sd(ones$num_words_in_song,na.rm=TRUE)), colour = "red")
#p = p + stat_function(fun = dnorm, args = list(mean = mean(dt$num_words_in_song,na.rm=TRUE), sd=sd(dt$num_words_in_song,na.rm=TRUE)), colour = "blue")
p = p + geom_density(data=ones, geom="line", color="red")
p = p + geom_density(data=dt, geom="line", color="blue")
ggsave(filename = sprintf("./plots/%s_all_num_words_in_song_histogram.png",data_name) , plot=p, width=15, height=10) 

p = ggplot(ones, aes(x=tempo)) 
km <- kmeans(ones$tempo,centers=3)
ones$tempo_cluster <- as.factor(km$cluster)
#p = p + geom_histogram(binwidth=5) 
#p = p + geom_histogram(aes(y = ..density..), binwidth=5, fill=tempo_clust) + stat_density(geom="line", color="red")
p = p + geom_histogram(aes(fill=tempo_cluster,y=..count../sum(..count..)),
                      binwidth=3, color="grey50") + stat_density(geom="line", color="red")
p = p + ggtitle("tempo histogram") + theme(plot.title = element_text(size=14))
p = p + theme(axis.text.x=element_text(angle=75, hjust=1))
p = p + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
p = p + xlab("tempo (beats per minute)") + ylab("songs density") 
p = p + scale_x_discrete(limits = seq(80,210,10))
ggsave(filename = sprintf("./plots/%s_tempo_histogram.png",data_name) , plot=p, width=15, height=10) 


p = ggplot(dt, aes(x=danceability)) 
p = p + geom_histogram(data = ones, alpha = 0.2, aes(fill="hits", y = ..density..)) 
p = p + geom_histogram(data = dt, alpha = 0.2, aes(fill="all songs", y = ..density..)) 
#geom_histogram(data = highf0, fill = "green", alpha = 0.2) +
#p = p + stat_function(fun = dnorm, args = list(mean = mean(ones$danceability, na.rm=TRUE), sd=sd(ones$danceability,na.rm=TRUE)), colour = "red")
#p = p + stat_function(fun = dnorm, args = list(mean = mean(dt$danceability,na.rm=TRUE), sd=sd(dt$danceability,na.rm=TRUE)), colour = "blue")
#p = p + geom_vline(xintercept = mean(ones$danceability, na.rm=TRUE), colour="red")
#p = p + geom_vline(xintercept = mean(dt$danceability, na.rm=TRUE), colour="blue")
#p = p + scale_x_continuous(breaks=c(0,1, mean(ones$danceability, na.rm=TRUE), mean(dt$danceability, na.rm=TRUE)))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + geom_density(data=ones, color="blue", show.legend=TRUE)
p = p + geom_density(data=dt, color="red", show.legend=TRUE)
p = p + theme(legend.title=element_blank())
p = p + ggtitle("Song Danceability - No 1 Hits vs. all Billboard ") + theme(plot.title = element_text(size=12))
#p = p + scale_fill_manual(values=c("red", "blue"), labels=c("hits", "all songs"), guide = guide_legend(nrow=1)) 
ggsave(filename = sprintf("./plots/%s_all_danceability_histogram.png",data_name) , plot=p, width=7, height=5) 

p = ggplot(dt, aes(x=loudness)) 
p = p + geom_histogram(data = ones, alpha = 0.2, aes(fill="hits", y = ..density..)) 
p = p + geom_histogram(data = dt, alpha = 0.2, aes(fill="all songs", y = ..density..)) 
#geom_histogram(data = highf0, fill = "green", alpha = 0.2) +
#p = p + stat_function(fun = dnorm, args = list(mean = mean(ones$loudness, na.rm=TRUE), sd=sd(ones$loudness,na.rm=TRUE)), colour = "red")
#p = p + stat_function(fun = dnorm, args = list(mean = mean(dt$loudness,na.rm=TRUE), sd=sd(dt$loudness,na.rm=TRUE)), colour = "blue")
#p = p + geom_vline(xintercept = mean(ones$loudness, na.rm=TRUE), colour="red")
#p = p + geom_vline(xintercept = mean(dt$loudness, na.rm=TRUE), colour="blue")
#p = p + scale_x_continuous(breaks=c(0,1, mean(ones$loudness, na.rm=TRUE), mean(dt$loudness, na.rm=TRUE)))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + geom_density(data=ones, color="blue", show.legend=TRUE)
p = p + geom_density(data=dt, color="red", show.legend=TRUE)
p = p + theme(legend.title=element_blank())
p = p + ggtitle("Song Danceability - No 1 Hits vs. all Billboard ") + theme(plot.title = element_text(size=12))
#p = p + scale_fill_manual(values=c("red", "blue"), labels=c("hits", "all songs"), guide = guide_legend(nrow=1)) 
ggsave(filename = sprintf("./plots/%s_all_loudness_histogram.png",data_name) , plot=p, width=7, height=5) 


p = ggplot(dt, aes(x=num_words_in_song)) 
p = p + geom_histogram(data = ones, alpha = 0.2, aes(fill="hits", y = ..density..)) 
p = p + geom_histogram(data = dt, alpha = 0.2, aes(fill="all songs", y = ..density..)) 
#geom_histogram(data = highf0, fill = "green", alpha = 0.2) +
#p = p + stat_function(fun = dnorm, args = list(mean = mean(ones$loudness, na.rm=TRUE), sd=sd(ones$loudness,na.rm=TRUE)), colour = "red")
#p = p + stat_function(fun = dnorm, args = list(mean = mean(dt$loudness,na.rm=TRUE), sd=sd(dt$loudness,na.rm=TRUE)), colour = "blue")
#p = p + geom_vline(xintercept = mean(ones$loudness, na.rm=TRUE), colour="red")
#p = p + geom_vline(xintercept = mean(dt$loudness, na.rm=TRUE), colour="blue")
#p = p + scale_x_continuous(breaks=c(0,1, mean(ones$loudness, na.rm=TRUE), mean(dt$loudness, na.rm=TRUE)))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + geom_density(data=ones, color="blue", show.legend=TRUE)
p = p + geom_density(data=dt, color="red", show.legend=TRUE)
p = p + theme(legend.title=element_blank())
p = p + ggtitle("Song Danceability - No 1 Hits vs. all Billboard ") + theme(plot.title = element_text(size=12))
#p = p + scale_fill_manual(values=c("red", "blue"), labels=c("hits", "all songs"), guide = guide_legend(nrow=1)) 
ggsave(filename = sprintf("./plots/%s_all_num_words_in_song_histogram.png",data_name) , plot=p, width=7, height=5) 


p = ggplot(dt, aes(x=energy)) 
p = p + geom_histogram(data = ones, alpha = 0.2, aes(fill="hits", y = ..density..)) 
p = p + geom_histogram(data = dt, alpha = 0.2, aes(fill="all songs", y = ..density..)) 
#geom_histogram(data = highf0, fill = "green", alpha = 0.2) +
#p = p + stat_function(fun = dnorm, args = list(mean = mean(ones$energy, na.rm=TRUE), sd=sd(ones$energy,na.rm=TRUE)), colour = "red")
#p = p + stat_function(fun = dnorm, args = list(mean = mean(dt$energy,na.rm=TRUE), sd=sd(dt$energy,na.rm=TRUE)), colour = "blue")
#p = p + geom_vline(xintercept = mean(ones$energy, na.rm=TRUE), colour="red")
#p = p + geom_vline(xintercept = mean(dt$energy, na.rm=TRUE), colour="blue")
#p = p + scale_x_continuous(breaks=c(0,1, mean(ones$energy, na.rm=TRUE), mean(dt$energy, na.rm=TRUE)))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + geom_density(data=ones, color="blue", show.legend=TRUE)
p = p + geom_density(data=dt, color="red", show.legend=TRUE)
p = p + theme(legend.title=element_blank())
p = p + ggtitle("Song Enrgy - No 1 Hits vs. all Billboard ") + theme(plot.title = element_text(size=12))
#p = p + scale_fill_manual(values=c("red", "blue"), labels=c("hits", "all songs"), guide = guide_legend(nrow=1)) 
ggsave(filename = sprintf("./plots/%s_all_energy_histogram.png",data_name) , plot=p, width=7, height=5) 


p = ggplot(dt, aes(x=valence)) 
p = p + geom_histogram(data = ones, alpha = 0.2, aes(fill="hits", y = ..density..)) 
p = p + geom_histogram(data = dt, alpha = 0.2, aes(fill="all songs", y = ..density..)) 
#geom_histogram(data = highf0, fill = "green", alpha = 0.2) +
#p = p + stat_function(fun = dnorm, args = list(mean = mean(ones$valence, na.rm=TRUE), sd=sd(ones$valence,na.rm=TRUE)), colour = "red")
#p = p + stat_function(fun = dnorm, args = list(mean = mean(dt$valence,na.rm=TRUE), sd=sd(dt$valence,na.rm=TRUE)), colour = "blue")
#p = p + geom_vline(xintercept = mean(ones$valence, na.rm=TRUE), colour="red")
#p = p + geom_vline(xintercept = mean(dt$valence, na.rm=TRUE), colour="blue")
#p = p + scale_x_continuous(breaks=c(0,1, mean(ones$valence, na.rm=TRUE), mean(dt$valence, na.rm=TRUE)))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1))
p = p + geom_density(data=ones, color="blue", show.legend=TRUE)
p = p + geom_density(data=dt, color="red", show.legend=TRUE)
p = p + theme(legend.title=element_blank())
p = p + ggtitle("Song Valence - No 1 Hits vs. all Billboard ") + theme(plot.title = element_text(size=12))
#p = p + scale_fill_manual(values=c("red", "blue"), labels=c("hits", "all songs"), guide = guide_legend(nrow=1)) 
ggsave(filename = sprintf("./plots/%s_all_valence_histogram.png",data_name) , plot=p, width=7, height=5) 


ones$lyrics <- sapply(ones$lyrics,function(row) iconv(row, "latin1", "ASCII", sub="")) # clean crap chars
library(slam)
library(tm)
library(wordcloud)
library(SnowballC)

data_name = "hit_songs"
# need to process the lyrics to remove crap
words = Corpus(VectorSource(ones$lyrics)) 
corpus <- tm_map(words, content_transformer(tolower))
  
dtm = DocumentTermMatrix(words)
terms = findFreqTerms(dtm)
print(sprintf("num uniq words in lyrics is %s", length(terms)))
common = findFreqTerms(dtm, 10)
  
# Generate wordcloud only removing 'the', 'and', 'a'
words = tm_map(words, stripWhitespace)
words = tm_map(words, tolower)
badwords = c("the", "and", "a")
words = tm_map(words, removeWords, badwords)
png(sprintf("./plots/%s_simple_wordcloud_you.png", data_name))
#wordcloud(words, max.words = 120, random.order=FALSE, colors=brewer.pal(nrow(dt_uniq),"Dark2"))
wordcloud(words, max.words = 30, random.order=FALSE)
dev.off()
  
# Generate wordcloud removing all stop words
png(sprintf("./plots/%s_stopwords_wordcloud_100words_color.png", data_name),width=400,height=400, res=80)
words = tm_map(words, removeWords, stopwords('english') )
wordcloud(words, max.words = 30, random.order=FALSE, colors=brewer.pal(8,"Dark2"),width=400,height=400)
  
dev.off()


# change over time?  
ones = ones[order(as.Date(ones$date, format="%m/%d/%Y")),]
ones$date = as.Date(ones$date, format="%m/%d/%Y")
dt = dt[order(as.Date(dt$date, format="%m/%d/%Y")),]
dt$date = as.Date(dt$date, format="%m/%d/%Y")
p = ggplot(ones, aes(x=date, y=tempo)) + geom_point() +  geom_smooth(method = "lm")
ggsave(filename = sprintf("./plots/%s_time_tempo.png",data_name) , plot=p, width=15, height=10) 
p = ggplot(ones, aes(x=date, y=duration)) + geom_point() +  geom_smooth(method = "lm")
p = ggplot(ones, aes(x=date, y=valence)) + geom_point() +  geom_smooth(method = "lm")
p = ggplot(ones, aes(x=date, y=energy)) + geom_point() +  geom_smooth(method = "lm")


# artists


artists = c()
for (main_artist in levels(droplevels(ones$main_artist))) {
  artist_info = ones[ones$main_artist==main_artist,]
  num_hit_songs = nrow(artist_info)
  stats_row = c(main_artist, num_hit_songs)
  artists = rbind(artists, stats_row)
}
artists = as.data.table(artists)
artists = as.data.frame(artists)
colnames(artists) = c("artist","num_hits")


artists$num_hits = as.numeric(as.character(artists$num_hits))
artists = artists[order(artists$num_hits, decreasing=TRUE),]
#artists$artist = factor(artists$artist, levels = artists$artist[order(artists$num_hits, decreasing=TRUE)]) 
top_artist = artists[1:25,]
top_artist$artist = factor(top_artist$artist, levels=top_artist$artist)
# create new df with artist, summing over how many songs they have
p = ggplot(top_artist, aes(x=artist, y=num_hits)) + geom_bar(stat="identity") 
p = p + ggtitle("Top 20 artists") + theme(plot.title = element_text(size=24))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,size=18)) + theme(plot.title = element_text(size=24))
p = p + theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"))
p = p + xlab("Artist") + ylab("Number of No1 Songs") 
p = p + scale_y_discrete(limits = c(0,1,2,3,4,5,6,7,8,9,10)) 
ggsave(filename = sprintf("./plots/%s_artist_weeks_top20_histogram.png",data_name) , plot=p, width=15, height=10) 


# men vs women
hits = read.csv("ones.csv", encoding="UTF-8")
men = hits[hits$gender=="man",]
women = hits[hits$gender=="woman",]

# Examine the changes in gender dominance over time
genders = c()
hits$year = as.factor(hits$year)
for (year in levels(droplevels(hits$year))) {
  hits_year = hits[hits$year==year,]
  hits_year_men = hits_year[hits_year$gender=="man",]
  hits_year_women = hits_year[hits_year$gender=="woman",]
  num_hit_songs = nrow(hits_year_men)
  percentage_women = nrow(hits_year_women)/nrow(hits_year)*100
  stats_row = c(year, nrow(hits_year_women), percentage_women)
  genders = rbind(genders, stats_row)
}
genders = as.data.table(genders)
genders = as.data.frame(genders)
colnames(genders) = c("year", "women_hits", "percentage_women")
genders$percentage_women = as.numeric(as.character(genders$percentage_women))
genders$women_hits = as.numeric(as.character(genders$women_hits))

p = ggplot(genders, aes(x=year, y=percentage_women, group = 1)) + geom_point() + geom_smooth(method = "lm")
p = p + ggtitle("Percentage Women") + theme(plot.title = element_text(size=12))
p = p + theme(axis.text.x=element_text(angle=90, hjust=1,size=10)) + theme(plot.title = element_text(size=12))
p = p + theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"))
p = p + xlab("Year") + ylab("Percentage Women Hit songs") 
ggsave(filename = sprintf("./plots/%s_percentage_women_hits.png",data_name) , plot=p, width=6, height=5) 


