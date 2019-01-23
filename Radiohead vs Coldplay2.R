# details do these last for ever?
print(Sys.setenv(SPOTIFY_CLIENT_ID = "1def4f31ddf74364b3f215ca5ab2f0fb", 
                 SPOTIFY_CLIENT_SECRET = "101b5fd93c934b34baeff554eaf85bc4"))

library(tidyverse)
library(spotifyr)
library(reshape2)

# get radiohead songs
radiohead.raw <- get_artist_audio_features('Radiohead') 
radiohead <- radiohead.raw %>% mutate(artist = "Radiohead") %>% 
  filter(!(album_name %in% c("OK Computer OKNOTOK 1997 2017", "TKOL RMX 1234567", "I Might Be Wrong")))

# get coldplay songs
coldplay.raw <- get_artist_audio_features('Coldplay') 
coldplay <- coldplay.raw %>% mutate(artist = "Coldplay") %>% 
  filter(!(album_name %in% c("A Head Full Of Dreams Tour Edition", "Ghost Stories Live 2014", "Viva La Vida Or Death And All His Friends" )))

# combine dataframes for analysis
songs <- bind_rows(list(radiohead, coldplay)) %>% 
  select(-album_uri, -album_img, -album_release_date, -album_release_year, -track_uri) %>%
  as.tibble()

# plot some things
songs  %>% ggplot(aes(x=key, fill=artist)) + geom_bar(position = "dodge") 
  #coldplay much more willing to play in any key. Radiohead have facourites

songs %>% ggplot(aes(x=tempo, col=artist, fill=artist)) + geom_density(alpha=0.5)
