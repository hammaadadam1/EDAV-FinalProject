# Set data

data = class_playlists 

# Get list of all genres

genre_list <- as.vector(data$artist_genres)
all_genres <- unique(Reduce(c, genre_list))

# Calculate the number of songs that fall into each genre for a user

user_genre_count <- matrix(nrow=0, ncol = 3)

for(name in unique(data$Name)){
  data_user <- subset(data, Name == name)
  user_genres_vector <- as.vector(data_user$artist_genres)
  user_genres <- unique(Reduce(c, user_genres_vector))
  count = 0
  for(genre in user_genres){
    count = 0
    for(i in 1:100){
      if(genre %in% data_user$artist_genres[i][[1]]){
        count = count + 1
      }
    }
    user_genre_count = rbind(user_genre_count, c(name, genre, count))
  }
}

user_genre_count = data.frame(user_genre_count, stringsAsFactors=FALSE)
user_genre_count[,3] = as.numeric(user_genre_count[,3])
names(user_genre_count) = c("Name", "Genre", "Freq")

user_genre_count = user_genre_count %>% group_by(Name) %>% arrange(-Freq, .by_group = TRUE)

# Calculate the minimum spanning genres for each playlist

genre_span <- function(name){
  data_name = subset(data, Name == name)
  name_vector <- as.vector(data_name$artist_genres)
  name_genres <- unique(Reduce(c, name_vector))
  count = 0
  min_genres = matrix(nrow=0, ncol=3)
  nremoved = 0
  non_null_indices = c()
  
  for(i in 1:100){
    if(!is.null(data_name$artist_genres[i][[1]])){
      non_null_indices = c(non_null_indices, i)
    }
  }
  data_name = data_name[non_null_indices,]
  nrows = nrow(data_name)
  
  while(nrow(data_name)>0){
    name_genre_count = matrix(nrow = 0, ncol = 3)
    for(genre in name_genres){
      count = 0
      for(i in 1:100){
        if(genre %in% data_name$artist_genres[i][[1]]){
          count = count + 1
        }
      }
      name_genre_count = rbind(name_genre_count, c(name, genre, count))
    }
    name_genre_count = data.frame(name_genre_count, stringsAsFactors = FALSE)
    names(name_genre_count) = c("Name", "Genre", "Freq")
    name_genre_count$Freq = as.numeric(name_genre_count$Freq)
    most_genre = name_genre_count[name_genre_count$Freq == max(name_genre_count$Freq),][1,2]
    indices_to_keep = c()
    for(i in 1:100){
      if(!most_genre %in% data_name$artist_genres[i][[1]] & !is.null(data_name$artist_genres[i][[1]])){
        indices_to_keep = c(indices_to_keep, i)
      }
    }
    old_length = nrow(data_name)
    data_name = data_name[indices_to_keep,]
    new_length = nrow(data_name)
    min_genres = rbind(min_genres, c(name, most_genre, old_length - new_length))
  }
  min_genres = data.frame(min_genres, stringsAsFactors = FALSE)
  names(min_genres) = c("Name", "Genre", "Songs")
  return(min_genres)
}

genre_spans <- genre_span(as.character(unique(data$Name)[1]))

for(i in 2:length(unique(data$Name))){
  genre_spans <- rbind(genre_spans, genre_span(as.character(unique(data$Name)[i])))
}

# Top genre vs Number of genres

user_names <- unique(genre_spans$Name)
user_top_genre <- genre_spans[1,]

for(i in 2:length(user_names)){
  top_g = subset(genre_spans, Name == user_names[i])[1,]
  user_top_genre = rbind(user_top_genre, top_g)
}

user_n_genres <- genre_spans %>% group_by(Name) %>% 
                    summarise(ngenres=n()) %>% 
                      inner_join(user_top_genre, by = "Name") %>%
                        select(Name, ngenres, top_genre = Genre, top_genre_songs = Songs) %>%
                          mutate(top_genre_songs = as.numeric(top_genre_songs))

artist_genre_concentration <- user_artist_concentration %>% inner_join(user_n_genres, by = "Name")

ggplot(artist_genre_concentration, aes(x= nartists, y = ngenres)) + geom_point() + 
  theme_classic() + xlab("Number of Artists") + ylab("Number of Spanning Genres")

ggplot(artist_genre_concentration, aes(y= top_genre_songs, x = top10)) + geom_point() + 
  theme_classic() + ylab("% of Songs in Top Genre") + xlab("% of Songs By Top 10 Artists")

