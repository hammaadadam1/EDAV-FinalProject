data = class_playlists %>% left_join(artist_genres, by = "artist_name")
data = subset(data, !grepl("Deepak",Name))

artist_freq_user <- class_playlists %>% group_by(Name, artist_name)  %>%
                      summarise(Freq = n()) %>% arrange(Name, desc(Freq))

user_names <- unique(artist_freq_user$Name)

user_top3_artists <- artist_freq_user[1:3,]
for(i in 2:length(user_names)){
  user_top = subset(artist_freq_user, Name == user_names[i])[1:3,]
  user_top3_artists = rbind(user_top3_artists, user_top)
}

user_top3_concentration <- user_top3_artists %>% group_by(Name)  %>%
                              summarise(Sum = sum(Freq)) %>% arrange(Sum)

ggplot(user_top3_concentration, aes(x = fct_reorder(Name, -Sum), y = Sum)) + 
  geom_bar(stat = "identity") + 
    theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1)) +
      xlab("") + ylab('Songs from Top 3 Artists')

user_top1_artists <- artist_freq_user[1,]
for(i in 2:length(user_names)){
  user_top = subset(artist_freq_user, Name == user_names[i])[1,]
  user_top1_artists = rbind(user_top1_artists, user_top)
}

user_top1_concentration <- user_top1_artists %>% group_by(Name)  %>%
  summarise(Sum = sum(Freq)) %>% arrange(Sum)

ggplot(user_top1_concentration, aes(x = fct_reorder(Name, -Sum), y = Sum)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  xlab("") + ylab('Songs from Top 1 Artist')

user_top10_artists <- artist_freq_user[1:10,]
for(i in 2:length(user_names)){
  user_top = subset(artist_freq_user, Name == user_names[i])[1:10,]
  user_top10_artists = rbind(user_top10_artists, user_top)
}

user_top10_concentration <- user_top10_artists %>% group_by(Name)  %>%
  summarise(Sum = sum(Freq)) %>% arrange(Sum)

ggplot(user_top10_concentration, aes(x = fct_reorder(Name, -Sum), y = Sum)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  xlab("") + ylab('Songs from Top 1 Artist')

user_total_artists <- artist_freq_user %>% summarise(nartists = n())

user_artist_concentration <- user_total_artists %>% 
                                left_join(user_top1_concentration, by="Name") %>%
                                  left_join(user_top3_concentration, by="Name") %>%
                                    left_join(user_top10_concentration, by="Name") %>%
                                      select(Name, nartists, top1 = "Sum.x", top3 = "Sum.y", top10 = "Sum")

ggplot(user_artist_concentration, aes(x=nartists, y=top1)) + geom_point()



