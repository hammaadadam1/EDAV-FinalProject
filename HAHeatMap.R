Sys.setenv(SPOTIFY_CLIENT_ID = '40a6924532484d369b25a4a2e1be8388')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '183a0101898343a59d8c7193d5506db0')

library(spotifyr)

access_token <- get_spotify_access_token()

library(tidyverse)

data <- read.csv("ClassMusicData.csv")
class_names <- read.csv("Names.csv")

data = data %>% dplyr::inner_join(class_names, by = "user_id")

artists <- unique(data$artist_name)

subset(data, grepl("_", artist_name)==TRUE)$artist

song_data <- data %>% 
              select(-X, -playlist_name, -playlist_uri, 
                             -playlist_img, -playlist_tracks_url, 
                             -playlist_num_tracks, -snapshot_id,
                             -user_id, -location2017,
                             -nationality, -undergrad, -age, -useFrequency,
                             -Name, -Display) %>% 
                distinct(track_name, artist_name, .keep_all = TRUE)

# Can verify actually unique by running View(song_data_unique %>% count(track_name, sort = TRUE))

song_freq <- data %>% group_by(track_name, artist_name)  %>%
              summarise(Freq = n()) %>%
                arrange(desc(Freq))

artist_freq <- data %>% count(artist_name, sort = TRUE)
View(artist_freq)

artist_per_playlist <- data %>% distinct(Name, artist_name) %>% 
                        count(artist_name, sort = TRUE) %>% 
                          left_join(artist_freq, by="artist_name") %>% 
                              select(artist_name, nplaylists = n.x, nsongs = n.y)

scatterplot_n <- ggplot(artist_per_playlist, aes(x=nplaylists, y=nsongs)) + 
                  geom_point() + 
                    geom_abline(slope = 1)

scatterplot_n

artist_by_listener <- data %>% distinct(Name, artist_name, track_name) %>% 
                        group_by(artist_name, Name) %>% 
                          summarise(n = n())

song_user <- data %>% select(Name, track_name, artist_name) %>% cbind(1) %>% 
              rename(binaryid="1") %>% 
                distinct(Name, artist_name, track_name, .keep_all = TRUE) %>%
                  spread(Name, binaryid, fill = 0) %>%
                    group_by(track_name, artist_name) %>% 
                      summarise_all(sum)

nusers = length(song_user) - 2
song_intersection <- data.frame(matrix(nrow=nusers, ncol=nusers+1))
colnames(song_intersection) = append(c("Name"), names(song_user)[-c(1,2)])
song_intersection$Name = colnames(song_intersection)[-1]

for(i in 1:nusers){
  for(j in 2:(nusers+1)){
    if(i+1==j){
      song_intersection[i,j] = 0
    } else{
      song_intersection[i,j] = nrow(subset(song_user, song_user[,i+2]==1 & song_user[,j+1]==1))
    }
  }
}

song_intersection = song_intersection %>% gather(key = "sharer", value = "nshared", -Name)
  
theme_heat <- theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 60, hjust=0))

ggplot(song_intersection, aes(x = Name, y = fct_rev(sharer))) + 
  geom_tile(aes(fill = nshared), color = "white") + 
    coord_fixed() + theme_heat + scale_x_discrete(position = "top") +
      scale_fill_gradient2(low = "white", mid = "white", high = "darkblue", 
        name = "Number of Shared Songs") + xlab("") + ylab("")
