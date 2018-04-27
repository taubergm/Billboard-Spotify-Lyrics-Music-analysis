from bs4 import BeautifulSoup
import requests
import urllib
import time
import os
import sys
import re
import lyrics as minilyrics  # this script uses a modified version of SpotifyLYrics - https://github.com/fr31/spotifylyrics
import services as s
import csv
import json
import sqlite3



services_list1 = [s._minilyrics]

# Without Sync.
services_list2 = [s._wikia, s._musixmatch, s._songmeanings, s._songlyrics, s._genius, s._versuri]

artist = ""
song = ""
url = ""

'''
current_service is used to store the current index of the list.
Useful to change the lyrics with the button "Next Lyric" if
the service returned a wrong song
'''
current_service = -1



def load_lyrics(artist, song, sync=False):
    error = "Error: Could not find lyrics."
    global current_service

    if current_service == len(services_list2)-1: current_service = -1

    if sync == True:
        lyrics, url, timed = s._minilyrics(artist, song)
        current_service = -1

    if sync == True and lyrics == error or sync == False:
        timed = False
        for i in range (current_service+1, len(services_list2)):
            lyrics, url = services_list2[i](artist, song)
            current_service = i
            if lyrics != error:
                lyrics = lyrics.replace("&amp;", "&").replace("`", "'").strip()
                break

    #return "Error: Could not find lyrics."  if the for loop doens't find any lyrics
    return(lyrics, url, timed)


def getlyrics(songname, sync=False):
    global artist, song, url, current_service
    artist = ""
    song = ""
    url = ""
    current_service = -1

    if songname.count(" - ") == 1:
        artist, song = songname.rsplit(" - ", 1)
    if songname.count(" - ") == 2:
        artist, song, garbage = songname.rsplit(" - ", 2)
    if " / " in song:
        song, garbage = song.rsplit(" / ", 1)
    song = re.sub(' \(.*?\)', '', song, flags=re.DOTALL)

    return load_lyrics(artist, song, sync)


songs = []
i = 0

# write to raw text, json, csv, db
lyrics_raw  = open('songs_18yrs_spotify_data_v5_raw.txt', 'wb')
json_file = open('songs_18yrs_spotify_data_v5.json', 'w')
outCsv = open('songs_18yrs_spotify_data_v5.csv', 'wb')
fieldnames = [
                'date', 'year', 'title', 'simple_title', 'artist', 'main_artist', 'peak_pos',	'last_pos', 'weeks',	'rank',	'change', 'spotify_link', 'spotify_id', 'video_link', 'genre', 
                'analysis_url', 'energy', 'liveness', 'tempo', 'speechiness', 'acousticness', 'instrumentalness', 'time_signature', 'danceability', 'key', 'duration_ms',
                'loudness', 'valence', 'mode', 'lyrics'
             ]
writer = csv.DictWriter(outCsv, fieldnames=fieldnames)
writer.writeheader()
conn = sqlite3.connect('songs_18yrs_spotify_data_v5.sqlite')
conn.text_factory = str
cur = conn.cursor()
cur.executescript('''
DROP TABLE IF EXISTS Track;

CREATE TABLE Track (
    id  INTEGER NOT NULL PRIMARY KEY 
        AUTOINCREMENT UNIQUE,
    date TEXT ,
    year INTEGER,
    title  TEXT,
    simple_title TEXT,
    artist TEXT,
    main_artist TEXT,
    peak_pos INTEGER, last_pos INTEGER, weeks INTEGER, rank INTEGER, change INTEGER,
    spotify_link TEXT,
    spotify_id TEXT,
    video_link TEXT,
    genre TEXT,
    analysis_url TEXT,
    energy FLOAT,
    liveness FLOAT,
    tempo INTEGER,
    speechiness FLOAT,
    acousticness FLOAT,
    instrumentalness FLOAT,
    time_signature INTEGER,
    danceability FLOAT,
    key INTEGER,
    duration_ms INTEGER,
    loudness INTEGER,
    valence FLOAT,
    mode INTEGER,
    lyrics TEXT
  )
''')


csvFile = "songs_18yrs_spotify_data_v4.csv"
reload(sys)  
sys.setdefaultencoding('utf8')
with open(csvFile) as billboard:
    reader = csv.DictReader(billboard)
    for row in reader:
        
        search_str = "%s - %s" % (row['title'] , row['artist'])
        print search_str
        (lyrics, url, timed) = getlyrics(search_str)
    
    
        # remove extra crud from lyrics 
        # eg - 1,"(function(){var opts={artist:""Wham!"",song:""Careless Whisper [Featuring George Michael]"",adunit_id:100000251,div_id:""cf_async_""+Math.floor((Math.random()*999999999))};document.write('<div id=""'+opts.div_id+'""></div>');var c=function(){cf.showAsyncAd(opts)};if(typeof window.cf!=='undefined')c();else{cf_async=!0;var r=document.createElement(""script""),s=document.getElementsByTagName(""script"")[0];r.async=!0;r.src=""//srv.clickfuse.com/showads/showad.js"";r.readyState?r.onreadystatechange=function(){if(""loaded""==r.readyState||""complete""==r.readyState)r.onreadystatechange=null,c()}:r.onload=c;s.parentNode.insertBefore(r,s)};})();Time can never mend
        re.sub(r'\"\(function.*', '', lyrics)
        re.sub(r'\(function.*;', '', lyrics)
    
        try:
            lyrics = lyrics.encode('utf-8')
            row['date'] = row['date'] .encode('utf-8')
            row['title'] = row['title'] .encode('utf-8')
            row['simple_title'] = row['simple_title'] .encode('utf-8')
            row['artist'] = row['artist'] .encode('utf-8')
            row['main_artist'] = row['main_artist'] .encode('utf-8')
            row['spotify_link'] = row['spotify_link'] .encode('utf-8')
            row['spotify_id'] = row['spotify_id'] .encode('utf-8')
            row['video_link'] = row['video_link'] .encode('utf-8')
            row['genre'] = row['genre'] .encode('utf-8')
            row['analysis_url'] = row['analysis_url'] .encode('utf-8') 
        except:
            print url
            print lyrics
            print "fail"


        row['lyrics'] = lyrics
        lyrics_raw.write(lyrics)
        json.dump(row, json_file)
        writer.writerow(row)

        cur.execute('''INSERT INTO Track
        ('date', 'year', 'title', 'simple_title', 'artist', 'main_artist', 'peak_pos',	'last_pos', 'weeks',	'rank',	'change', 'spotify_link', 'spotify_id', 'video_link', 'genre', 
                'analysis_url', 'energy', 'liveness', 'tempo', 'speechiness', 'acousticness', 'instrumentalness', 'time_signature', 'danceability', 'key', 'duration_ms',
                'loudness', 'valence', 'mode', 'lyrics') 
        VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)''', ( row['date'], row['year'], row['title'], row['simple_title'], row['artist'], row['main_artist'], row['peak_pos'], row['last_pos'], row['weeks'], row['rank'], row['change'], row['spotify_link'], row['spotify_id'], row['video_link'], row['genre'], row['analysis_url'], row['energy'], row['liveness'], row['tempo'], row['speechiness'], row['acousticness'], row['instrumentalness'], row['time_signature'], row['danceability'], row['key'], row['duration_ms'], row['loudness'], row['valence'], row['mode'], row['lyrics'])) 
        
        conn.commit()

    
lyrics_raw.close()
print "processed %s songs" % i      


        







