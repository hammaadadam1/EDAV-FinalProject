us_top_50 <- get_playlist_audio_features('spotifycharts', '37i9dQZEVXbLRQDuF5jeBp')
global_top_50 <- get_playlist_audio_features('spotifycharts','37i9dQZEVXbMDoHDwVN2tF')
top_2017 <- get_playlist_audio_features('spotify', '37i9dQZF1DX5nwnRMcdReF')

# Join genres to top playlists

top2017_artists <- unique(top_2017$artist_name)

top2017_artist_genres <- get_artists(top2017_artists[1])

for(i in 2:length(top2017_artists)){
  if(TRUE){
    top2017_artist_genres = rbind(top2017_artist_genres, get_artists(top2017_artists[i]))
    print(i)
  }
}

top2017_artist_genres = top2017_artist_genres %>% 
                        select(artist_name, artist_uri, artist_genres, artist_popularity, artist_num_followers) %>%
                          distinct(artist_name, .keep_all = TRUE)

top_2017 = top_2017 %>% left_join(top2017_artist_genres, by = "artist_name")

# Compute most popular genres for the Top playlist

top2017_genre_count <- matrix(nrow=0, ncol = 3)

top2017_genres_vector <- as.vector(top_2017$artist_genres)
top2017_genres <- unique(Reduce(c, top2017_genres_vector))
for(genre in top2017_genres){
  count = 0
  for(i in 1:nrow(top_2017)){
    if(genre %in% top_2017$artist_genres[i][[1]]){
      count = count + 1
    }
  }
  top2017_genre_count = rbind(top2017_genre_count, c("Top 2017", genre, count))
}

top2017_genre_count = data.frame(top2017_genre_count, stringsAsFactors=FALSE)
top2017_genre_count[,3] = as.numeric(top2017_genre_count[,3])
names(top2017_genre_count) = c("Name", "Genre", "Freq")

top2017_genre_count = top2017_genre_count %>% group_by(Name) %>% 
                        arrange(-Freq, .by_group = TRUE) %>% 
                          mutate(percent = Freq/nrow(top_2017)) 



# Compute DSI most popular genres

dsi_top_genres <- user_genre_count %>% group_by(Genre) %>% 
                    summarise(Freq=sum(Freq)) %>%
                      mutate(percent = Freq/length(unique(user_genre_count$Name))/100) %>% 
                        arrange(-percent) %>% 
                          mutate(Name= "DSI Top") %>% 
                            select(Name, Genre, Freq, percent)

names(dsi_top_genres) = names(top2017_genre_count)

top_genres = dsi_top_genres[1:20,] %>% left_join(top2017_genre_count, by = "Genre") %>%
              mutate(`DSI Top Songs`=percent.x, `2017 Top Songs`=percent.y) %>% ungroup() %>%
                select(Genre, `DSI Top Songs`, `2017 Top Songs`)

top_genres$`2017 Top Songs`[is.na(top_genres$`2017 Top Songs`)] = 0

top_genres = top_genres %>%
              arrange(-`DSI Top Songs`) %>% 
                mutate(Genre = factor(Genre, levels = .$Genre)) %>%
                  gather(key = "Name", value = "percent", -Genre)

theme_dotplot <- theme_bw(18) +
  theme(axis.text.y = element_text(size = rel(.75)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank())

ggplot(top_genres, aes(x=percent, 
                         y = fct_rev(Genre), color=Name)) +
  geom_point() + ylab("") + theme_dotplot +
    theme(legend.position="top") + 
      theme(legend.title=element_blank()) + 
        xlab("Percentage of Songs")

# Global Top 2017

top2017_artists <- unique(top_2017$artist_name)

top2017_artist_genres <- get_artists(top2017_artists[1])

for(i in 2:length(top2017_artists)){
  if(TRUE){
    top2017_artist_genres = rbind(top2017_artist_genres, get_artists(top2017_artists[i]))
    print(i)
  }
}

top2017_artist_genres = top2017_artist_genres %>% 
  select(artist_name, artist_uri, artist_genres, artist_popularity, artist_num_followers) %>%
  distinct(artist_name, .keep_all = TRUE)

top_2017 = top_2017 %>% left_join(top2017_artist_genres, by = "artist_name")

# Compute most popular genres for the Top playlist

top2017_genre_count <- matrix(nrow=0, ncol = 3)

top2017_genres_vector <- as.vector(top_2017$artist_genres)
top2017_genres <- unique(Reduce(c, top2017_genres_vector))
for(genre in top2017_genres){
  count = 0
  for(i in 1:nrow(top_2017)){
    if(genre %in% top_2017$artist_genres[i][[1]]){
      count = count + 1
    }
  }
  top2017_genre_count = rbind(top2017_genre_count, c("Top 2017", genre, count))
}

top2017_genre_count = data.frame(top2017_genre_count, stringsAsFactors=FALSE)
top2017_genre_count[,3] = as.numeric(top2017_genre_count[,3])
names(top2017_genre_count) = c("Name", "Genre", "Freq")

top2017_genre_count = top2017_genre_count %>% group_by(Name) %>% 
  arrange(-Freq, .by_group = TRUE) %>% 
  mutate(percent = Freq/nrow(top_2017)) 



# Compute DSI most popular genres

dsi_top_genres <- user_genre_count %>% group_by(Genre) %>% 
  summarise(Freq=sum(Freq)) %>%
  mutate(percent = Freq/length(unique(user_genre_count$Name))/100) %>% 
  arrange(-percent) %>% 
  mutate(Name= "DSI Top") %>% 
  select(Name, Genre, Freq, percent)

names(dsi_top_genres) = names(top2017_genre_count)

top_genres = dsi_top_genres[1:20,] %>% left_join(top2017_genre_count, by = "Genre") %>%
  mutate(`DSI Top Songs`=percent.x, `2017 Top Songs`=percent.y) %>% ungroup() %>%
  select(Genre, `DSI Top Songs`, `2017 Top Songs`)

top_genres$`2017 Top Songs`[is.na(top_genres$`2017 Top Songs`)] = 0

top_genres = top_genres %>%
  arrange(-`DSI Top Songs`) %>% 
  mutate(Genre = factor(Genre, levels = .$Genre)) %>%
  gather(key = "Name", value = "percent", -Genre)

theme_dotplot <- theme_bw(18) +
  theme(axis.text.y = element_text(size = rel(.75)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank())

ggplot(top_genres, aes(x=percent, 
                       y = fct_rev(Genre), color=Name)) +
  geom_point() + ylab("") + theme_dotplot +
  theme(legend.position="top") + 
  theme(legend.title=element_blank()) + 
  xlab("Percentage of Songs")