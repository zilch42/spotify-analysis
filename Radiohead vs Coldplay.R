# details do these last for ever?
print(Sys.setenv(SPOTIFY_CLIENT_ID = "1def4f31ddf74364b3f215ca5ab2f0fb", 
                 SPOTIFY_CLIENT_SECRET = "101b5fd93c934b34baeff554eaf85bc4"))

library(spotifyr)
library(tidyverse)
library(gridExtra)
library(randomForest)
library(caret)
library(kableExtra)

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
  mutate(artist = factor(artist), album_name = factor(album_name)) %>% 
  as.tibble()

# convert ms oto min
songs <- songs %>% mutate(duration = duration_ms/60000) %>% select(-duration_ms)

# convert minor keys to their relative major
minor.keys <- unique(songs$key) %>% sort() %>% paste(.,"minor")
rel.major.keys <- c("C",  "C#", "D",  "D#", "E",  "F",  "F#", "G",  "G#",  "A", "A#", "B" )
key.map <- tibble(key_mode = minor.keys, rel_major = rel.major.keys)
major <- tibble(key_mode = paste(rel.major.keys,"major"), rel_major = rel.major.keys)
key.map <- key.map %>% bind_rows(major)
songs <- songs %>% left_join(key.map)




# generate plots for grid
p <- songs %>% ggplot((aes(col = artist, fill = artist))) 
t <- theme(legend.position ="none")
s <- scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1))
p1 <- p + t + geom_density(aes(x=danceability), alpha=0.5)
p2 <- p + t + geom_density(aes(x=energy), alpha=0.5) + s
p3 <- p + t + geom_density(aes(x=loudness), alpha=0.5)
p4 <- p + t + geom_density(aes(x=speechiness), alpha=0.5)
p5 <- p + t + geom_density(aes(x=acousticness), alpha=0.5) + s
p6 <- p + t + geom_density(aes(x=instrumentalness), alpha=0.5) + s
p7 <- p + t + geom_density(aes(x=liveness), alpha=0.5)
p8 <- p + t + geom_density(aes(x=valence), alpha=0.5)
p9 <- p + t + geom_density(aes(x=tempo), alpha=0.5)
p10 <- p + t + geom_density(aes(x=duration), alpha=0.5)

# get a separate legend
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
l <- g_legend(p+geom_density(aes(x=danceability), alpha=0.5))

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, l, ncol = 4)

# generate bar charts
p11 <- p + t + geom_bar(aes(x=key), position = "dodge") + facet_wrap(~artist)
p12 <- p + t + geom_bar(aes(x=rel_major), position = "dodge") + facet_wrap(~artist)
p13 <- p + t + geom_bar(aes(x=mode), position = "dodge") + facet_wrap(~artist)
p14 <- p + t + geom_bar(aes(x=time_signature), position = "dodge") + facet_wrap(~artist)

grid.arrange(p11, p12, p13, p14, ncol = 2)

# popularity plot (lets not show this one maybe?)
pp <- p + geom_density(aes(x=track_popularity), alpha=0.5)

# clustering - PCA
predictors <- songs %>% 
  mutate(mode = ifelse(mode == "minor", 0, 1)) %>%
  select(-album_name, -album_popularity, -artist, -track_name, -track_popularity, -key, -key_mode, -rel_major, -time_signature) 
pca <- princomp(predictors, scores = TRUE)
songs %>% mutate(PC1 = pca$scores[,1], PC2 = pca$scores[,2]) %>% 
  ggplot(aes(PC1, PC2, col = artist)) + geom_point(size=2) + theme(legend.position="top")
# not a clue!

# logistic regression (stepwise)
predictors <- songs %>% mutate(time_signature = factor(time_signature), mode = factor(mode), artist = factor(artist), rel_major = factor(rel_major)) %>%
  select(-album_name, -album_popularity, -track_name, -track_popularity, -key, -key_mode) 
everything <- glm(artist ~ ., data = predictors, family = binomial())
nothing <- glm(artist ~ 1, data = predictors, family = binomial())
logistic <- step(everything, scope = list(lower=nothing), direction="backward")
# get fitted results
fitted <- songs %>% select(artist, album_name, track_name) %>% 
  mutate(prediction_probability = logistic$fitted.values) %>% 
  mutate(pred_artist = ifelse(prediction_probability> 0.5,'Radiohead', 'Coldplay'))
confusionMatrix(factor(fitted$pred_artist), fitted$artist)
# plot
fitted %>% mutate(prediction = round(prediction_probability)) %>% 
  ggplot(aes(x=prediction_probability, y=prediction, col = artist)) + geom_jitter(size=2) + 
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), legend.position="top")

# function: top miss-classified songs 
wrong_songs <- function() {
  # Most Coldplay-y Radiohead songs
  fitted %>% arrange(prediction_probability) %>%
    select(artist, album_name, track_name, prediction_probability, pred_artist) %>%
    filter(artist == 'Radiohead') %>% head(10) %>% kable()
  
  # Most Radiohead-y Coldplay songs
  fitted %>% arrange(desc(prediction_probability)) %>%
    select(artist, album_name, track_name, prediction_probability, pred_artist) %>%
    filter(artist == 'Coldplay') %>% head(10) %>% print()
}
#most poorly classified songs by logistic regression
wrong_songs()

# random forest
rf <- randomForest(artist ~ ., data = predictors, importance = TRUE)
# get fitted results
fitted <- songs %>% select(artist, album_name, track_name) %>% 
  mutate(prediction_probability = rf[["votes"]][,2]) %>% 
  mutate(pred_artist = ifelse(prediction_probability> 0.5,'Radiohead', 'Coldplay')) %>%
  mutate(artist = factor(artist))
confusionMatrix(factor(fitted$pred_artist), fitted$artist)
# plot
fitted %>% mutate(prediction = round(prediction_probability)) %>% ggplot(aes(x=prediction_probability, y=prediction, col = artist)) + geom_jitter()
# most poorly classified songs by RF
wrong_songs()

