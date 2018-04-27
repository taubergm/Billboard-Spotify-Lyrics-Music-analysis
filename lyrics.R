if (!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")
}

library(ggplot2)

workingDir = '/Users/michaeltauberg/billboard/'
csvName = "songs_18yrs_spotify_data_v5.csv"
data_name = "songs_18yrs_spotify_data_v5"
setwd(workingDir)

dt = read.csv(csvName)
# identify a very small number duplicate artist/song from bad billboard data
dt = dt[!duplicated(dt[,c('title','artist')], fromLast=FALSE),]

# weighing the data by weeks for more insight - use scaling formula x’ = (-1 + (x -xmin)(2)/xmax-xmin)*weeks
duration_ms = as.numeric(as.character(dt$duration_ms))
dt$weighted_duration_ms = (-1 + (duration_ms-min(duration_ms))*2/(max(duration_ms)-min(duration_ms)))*dt$weeks
valence = as.numeric(as.character(dt$valence))
dt$weighted_valence = (-1 + (valence-min(valence))*2/(max(valence)-min(valence)))*dt$weeks
energy = as.numeric(as.character(dt$energy))
dt$weighted_energy = (-1 + (energy-min(energy))*2/(max(energy)-min(energy)))*dt$weeks
tempo = as.numeric(as.character(dt$tempo))
dt$weighted_tempo = (-1 + (tempo-min(tempo))*2/(max(tempo)-min(tempo)))*dt$weeks

dt$duration = as.numeric(as.character(dt$duration_ms))/1000

# compute the number of words in things and add columns for plotting
dt$num_words_in_song_title = sapply(gregexpr("\\S+", dt$title), length)
dt$num_words_in_song = sapply(gregexpr("\\S+", dt$lyrics), length)
dt$lyric_density = as.numeric(as.character(dt$num_words_in_song))/as.numeric(as.character(dt$duration_ms))*1000

# cast the song characteristics to their proper type
dt$danceability = as.numeric(as.character(dt$danceability))
dt$valence = as.numeric(as.character(dt$valence))
dt$energy = as.numeric(as.character(dt$energy))
dt$tempo = as.numeric(as.character(dt$tempo))

# explore some stats
library(plyr)
duration_stats = ddply(dt, "year", summarise, 
                   mean=mean(strtoi(duration_ms),na.rm=TRUE), median=median(strtoi(duration_ms),na.rm=TRUE), min=min(strtoi(duration_ms),na.rm=TRUE), max=max(strtoi(duration_ms),na.rm=TRUE), sd=sd(strtoi(duration_ms),na.rm=TRUE))
p = ggplot(duration_stats, aes(x=year, y=mean)) + geom_line()

words_stats = ddply(dt, "year", summarise, 
                       mean=mean(strtoi(num_words_in_song_title),na.rm=TRUE), median=median(strtoi(num_words_in_song_title),na.rm=TRUE), min=min(strtoi(num_words_in_song_title),na.rm=TRUE), max=max(strtoi(num_words_in_song_title),na.rm=TRUE), sd=sd(strtoi(num_words_in_song_title),na.rm=TRUE))
p = ggplot(words_stats, aes(x=year, y=mean)) + geom_line()

lyrics_stats = ddply(dt, "year", summarise, 
                    mean=mean(strtoi(num_words_in_song),na.rm=TRUE), median=median(strtoi(num_words_in_song),na.rm=TRUE), min=min(strtoi(num_words_in_song),na.rm=TRUE), max=max(strtoi(num_words_in_song),na.rm=TRUE), sd=sd(strtoi(num_words_in_song)),na.rm=TRUE)
p = ggplot(lyrics_stats, aes(x=year, y=mean)) + geom_line()

danceability_stats = ddply(dt, "year", summarise, 
                       mean=mean(danceability,na.rm=TRUE), median=median(danceability,na.rm=TRUE), min=min(danceability,na.rm=TRUE), max=max(danceability,na.rm=TRUE), sd=sd(danceability,na.rm=TRUE))
p = ggplot(danceability_stats, aes(x=year, y=mean)) + geom_line()

energy_stats = ddply(dt, "year", summarise, 
                           mean=mean(energy,na.rm=TRUE), median=median(energy,na.rm=TRUE), min=min(energy,na.rm=TRUE), max=max(energy,na.rm=TRUE), sd=sd(energy,na.rm=TRUE))
p = ggplot(energy_stats, aes(x=year, y=mean)) + geom_line()

valence_stats = ddply(dt, "year", summarise, 
                     mean=mean(valence,na.rm=TRUE), median=median(valence,na.rm=TRUE), min=min(valence,na.rm=TRUE), max=max(valence,na.rm=TRUE), sd=sd(valence,na.rm=TRUE))
p = ggplot(valence_stats, aes(x=year, y=mean)) + geom_line()


# Now calculate statistics using weeks as a weight for the mean - this gives a better sense of the trends
library(data.table)
indx <- sapply(dt, is.factor)
#dt1[indx] <- lapply(dt[indx], function(x) as.numeric(x))
#dt1 = as.data.table(dt1)
dt1 = as.data.table(dt)
dt2 = dt1[,lapply(.SD,weighted.mean, na.rm=TRUE,w=weeks),by=year]
dt3 = dt1[,lapply(.SD,median),by=year]

# Point 1 - shortening trend
p = ggplot(dt2, aes(x=year, y=duration)) + geom_line() + ylab("Duration   (seconds)") 
ggsave(filename = "./plots/duration.png", plot=p, width=5, height=3.5) 
p = ggplot(dt2, aes(x=year, y=num_words_in_song_title)) + geom_line() + ylab("Number of Words in Song Title")
ggsave(filename = "./plots/num_words_in_song_title.png", plot=p, width=5, height=3.5) 
#p = ggplot(dt2, aes(x=year, y=num_words_in_song)) + geom_line()
p = ggplot(dt2, aes(x=year, y=lyric_density)) + geom_line() + ylab("Lyric Density   (words/second)")
ggsave(filename = "./plots/lyric_density.png", plot=p, width=5, height=3.5) 


# less energy and sadder , but tempo and danceability up since 2011
p = ggplot(dt2, aes(x=year, y=valence)) + geom_line() + ylab("Valence   (positivy)")
ggsave(filename = "./plots/valence.png", plot=p, width=5, height=3.5) 
p = ggplot(dt2, aes(x=year, y=energy)) + geom_line() + ylab("Energy")
ggsave(filename = "./plots/energy.png", plot=p, width=5, height=3.5) 
p = ggplot(dt2, aes(x=year, y=tempo)) + geom_line() + ylab("Avg Tempo   (beats per min)")
ggsave(filename = "./plots/tempo.png", plot=p, width=5, height=3.5) 
p = ggplot(dt2, aes(x=year, y=danceability)) + geom_line() + ylab("Daceability")
ggsave(filename = "./plots/danceability.png", plot=p, width=5, height=3.5) 


# Examine the changes in generes over time
genres = c()
dt$year = as.factor(dt$year)
for (year in levels(droplevels(dt$year))) {
  dt_year = dt[dt$year==year,]
  year_weeks_total = sum(dt_year$weeks)
  for (broad_genre in levels(droplevels(dt_year$broad_genre))) {
    dt_year_genre = dt_year[dt_year$broad_genre==broad_genre,]
    year_genre_weeks_total = sum(dt_year_genre$weeks)
    stats_row = c(year,broad_genre,year_genre_weeks_total,year_weeks_total,year_genre_weeks_total/year_weeks_total*100 )
    genres = rbind(genres, stats_row)
  }
}

genres = as.data.table(genres)
genres = as.data.frame(genres)
colnames(genres) = c("year","genre","weeks_genre","weeks_year","percent_genre")
genres$percent_genre=as.numeric(genres$percent_genre)
genres$genre[genres$genre == "unknown"] = "misc"


edm = genres[genres$genre=="edm",]
country = genres[genres$genre=="country",]
rap = genres[genres$genre=="rap",]
rb = genres[genres$genre=="r&b",]
rock = genres[genres$genre=="rock",]
pop = genres[genres$genre=="pop",]

edm_country = rbind(edm,country)
rock_rap_rb = rbind(rock,rap,rb)

p = ggplot(rock_rap_rb, aes(x=year, y=percent_genre, group=genre)) + geom_line(aes(colour=genre), size=1)
p = p + ggtitle("Rock / Rap / R&B Popularity   (percentage/time)")
p = p + xlab("Year") + ylab("%  of Weeks on the Hot100") # add axis labels
#p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p = p + theme(text = element_text(size=10), axis.text.x=element_text(angle=90, hjust=1))
p = p + scale_color_manual(values=c("#FF8333", "#1BB219", "#6B2EB6"))
ggsave(filename = "./plots/rock_rap_rnb.png", plot=p, width=7, height=5.5) 

p = ggplot(edm_country, aes(x=year, y=percent_genre, group=genre)) + geom_line(aes(colour=genre), size=1)
p = p + ggtitle("EDM / Country Popularity   (percentage/time)")
p = p + xlab("Year") + ylab("%  of Weeks on the Hot100") # add axis labels
#p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p = p + theme(text = element_text(size=10), axis.text.x=element_text(angle=90, hjust=1))
p = p + scale_color_manual(values=c("#FF6833", "#33E3FF", "#33FF6B"))
ggsave(filename = "./plots/edm_country.png", plot=p, width=7, height=5.5) 

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
#pie(genre_2017$percent_genre, labels = '%', main="2017 pie Chart of genres")
genre_2000 = genres[genres$year == "2000",]
p = ggplot(genre_2000, aes(x="", y=percent_genre, fill=genre)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) + scale_fill_brewer(palette="Dark2") 
p = p + blank_theme 
p = p + ggtitle("Popularity by Genre in 2000")
ggsave(filename = "./plots/2000_genre_piechart.png", plot=p, width=6, height=6) 

genre_2017 = genres[genres$year == "2017",]
p = ggplot(genre_2017, aes(x="", y=percent_genre, fill=genre)) + geom_bar(width = 1, stat = "identity")
p = p + coord_polar("y", start=0) + scale_fill_brewer(palette="Dark2") 
p = p + blank_theme 
p = p + ggtitle("Popularity by Genre in 2017")
ggsave(filename = "./plots/2017_genre_piechart.png", plot=p, width=6, height=6) 






