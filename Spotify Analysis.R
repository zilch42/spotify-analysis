# need to load own API key

library(spotifyr)
library(tidyverse)
library(httr)

artists <- get_artists('radiohead')
albums <- get_albums(artists$artist_uri[1])
radioheadSongs <- get_album_tracks(albums)
song <- filter(radioheadSongs, track_name == "Paranoid Android")

# get paranoid android track analysis
json <- GET(paste0("https://api.spotify.com/v1/audio-analysis/", 
                  song[3]), query = list(access_token = get_spotify_access_token())) %>% content

# transform data into frames
sections <- matrix(unlist(json$sections), nrow=length(unlist(json$sections[1]))) %>% t() %>% as.tibble()
names(sections) <- names(json$sections[[1]])
json$segments[[length(json$segments)]]$loudness_end <- NULL     #remove extra element as it gets in the way
segments <- matrix(unlist(json$segments, recursive = FALSE), nrow=length(json$segments), byrow = TRUE) %>% as.tibble()
names(segments) <- names(json$segments[[1]])

# investigate data
sections %>% ggplot(aes(x=start, y = confidence, fill = 1)) + geom_step()

                    