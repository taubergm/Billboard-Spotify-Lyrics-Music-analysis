# Billboard-Spotify-Lyrics-Music-analysis

All the scripts and data used to generate the posts at 
  https://medium.com/@michaeltauberg/spotify-is-killing-song-titles-5f48b7827653
  https://medium.com/@michaeltauberg/music-and-our-attention-spans-are-getting-shorter-8be37b5c2d67
  https://medium.com/@michaeltauberg/women-are-dominating-popular-music-43c5ed83534b
  
get_hits.py - gets top billboard songs from the hot100 over a specified time interval. Prints data to csv, json, sqllite db
add_spotify_data.py - uses python spotipy lib to add spotify audio features to songs in a csv based on artist/title lookup
add_lyrics.py - used to get lyrics for a song based on artist/title search. Adds as column to csv

billboard.R - creates plots of artists/titles/music stats over time
lyrics.R - creates plots of genres and lyrics data 
anatomy.R - creates plots related to songs that were number one on billboard hot 100
