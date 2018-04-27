#!/usr/bin/python
import billboard
import json
import csv
import datetime

BILLBOARD_CHART = 'hot-100'

songs = []
num_days_to_get = 2 # you can use up to 1000 api calls a day so max=1000
output_csv = "songs.csv"

for i in range(0,num_days_to_get):
    #day = datetime.datetime.now() - datetime.timedelta(days=3360) - datetime.timedelta(days=i) 
    day = datetime.datetime.now() - datetime.timedelta(days=74) - datetime.timedelta(days=i) 
    date = str(day.date())
    print date
    

    chart = billboard.ChartData(BILLBOARD_CHART, date)

    # sometimes we get non-ascii characters from the beautiful soup scrape - try this

    with open(output_csv, 'a') as csv_file:
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(["date", "title", "artist", "peakPos", "lastPos", "weeks", "rank", "change", "spotifyLink", "spotifyID", "videoLink"])
        for song in chart:
            song.title = song.title.encode('utf-8').strip()
            song.artist = song.artist.encode('utf-8').strip()
            #song.peakPos = song.peakPos.encode('utf-8').strip()
            #song.lastPos = song.lastPos.encode('utf-8').strip()
            #song.weeks = song.weeks.encode('utf-8').strip()
            #song.rank = song.rank.encode('utf-8').strip()
            #song.change = song.change.change('utf-8').strip()
            song.spotifyLink = song.spotifyLink.encode('utf-8').strip()
            song.spotifyID = song.spotifyID.encode('utf-8').strip()
            song.videoLink = song.videoLink.encode('utf-8').strip()

            csv_writer.writerow([date, song.title, song.artist, song.peakPos, song.lastPos, song.weeks, song.rank, song.change, song.spotifyLink, song.spotifyID, song.videoLink])

