import csv
import sys
#from __future__ import print_function    # (at top of module)
from spotipy.oauth2 import SpotifyClientCredentials
import json
import spotipy
import time
import sys
import pprint
import re

#client_credentials_manager = SpotifyClientCredentials()
client_credentials_manager = SpotifyClientCredentials(client_id="<your_client_id>", client_secret="<your_client_secret>")
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)
sp.trace=True


csvFile = "songs_18yrs.csv"


outCsv = open('songs_18yrs_spotify_data.csv', 'wb')
fieldnames = [
                'date', 'year', 'title', 'artist', 'peakPos',	'lastPos', 'weeks',	'rank',	'change', 'spotifyLink', 'spotifyID', 'videoLink', 'genre', 
                'analysis_url', 'energy', 'liveness', 'tempo', 'speechiness', 'acousticness', 'instrumentalness', 'time_signature', 'danceability', 'key', 'duration_ms',
                'loudness', 'valence', 'mode'
             ]
writer = csv.DictWriter(outCsv, fieldnames=fieldnames)
writer.writeheader()

trackId = ""
trackUri = ""
with open(csvFile) as billboard:
    reader = csv.DictReader(billboard)
    for row in reader:

        # get spotify Ids
        if (row['spotifyID'] == ""): # no spotifyId
            #try:
                search_str = "%s %s" % (row['artist'], row['title'])
                result = sp.search(search_str)
                #print result
                try:
                    trackUri = result['tracks']['items'][0]['uri']  # see if track found
                    trackId = result['tracks']['items'][0]['id'] 
                    row['spotifyID'] = trackId
                    row['spotifyLink'] = trackUri
                except IndexError:
                    print "bad track id from %s %s" % (row['artist'] , row['title'])
                    row['spotifyID'] = ""
                    row['spotifyLink'] = ""

        # get audio features from track id
        track_id = row['spotifyID'].encode("utf-8") 
        features = sp.audio_features(track_id)

        try:
            for feature in features:
                row['analysis_url'] = feature['analysis_url']
                row['energy'] = feature['energy']
                row['liveness'] = feature['liveness']
                row['tempo'] = feature['tempo']
                row['speechiness'] = feature['speechiness']
                row['acousticness'] = feature['acousticness']
                row['instrumentalness'] = feature['instrumentalness']
                row['time_signature'] = feature['time_signature']
                row['danceability'] = feature['danceability']
                row['key'] = feature['key']
                row['duration_ms'] = feature['duration_ms']
                row['loudness'] = feature['loudness']
                row['valence'] = feature['valence']
                row['mode'] = feature['mode']
        except:
                row['analysis_url'] = "unknown"
                row['energy'] = "unknown"
                row['liveness'] = "unknown"
                row['tempo'] = "unknown"
                row['speechiness'] = "unknown"
                row['acousticness'] = "unknown"
                row['instrumentalness'] = "unknown"
                row['time_signature'] = "unknown"
                row['danceability'] = "unknown"
                row['key'] = "unknown"
                row['duration_ms'] = "unknown"
                row['loudness'] = "unknown"
                row['valence'] = "unknown"
                row['mode'] = "unknown"


         # get genre from artist
        artist = row['artist'].lower()
        artist = re.sub(" featuring.*", '', artist )
        artist = re.sub(" with .*", '', artist )
        artist = re.sub(" & .*", '', artist )
        artist = re.sub(" \/ .*", '', artist )
        artist = re.sub(" x .*", '', artist )
        artist = re.sub(" duet.*", '', artist )
        artist = re.sub("travi$", "travis", artist )
        artist = re.sub("jay z", "jay-z", artist )
        artist = re.sub("\\\"misdemeanor\\\"", "misdemeanor",  artist )
        artist = re.sub(" + .*", '', artist )
        artist = re.sub(" vs.*", '', artist )
    
        result = sp.search(q=row['artist'], type='artist')
        try:
            artist_urn = result['artists']['items'][0]['uri']  
            genres = result['artists']['items'][0]['genres']  
            main_genre = result['artists']['items'][0]['genres'][-1:]
            row['genre'] = genres
        except:
            row['genre'] = "unknown"


        print row
        writer.writerow(row)
               
            





