library(e1071)

max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

predictRatings <- function(user_char, data, nmovies) {
  ratings = rep(0, nmovies)
  for (movie in c(1:nmovies)) {
    movie_filter <- data$item.id == movie
    filtered_data <- data[movie_filter,]
    nobs <- nrow(data[movie_filter,])
    
    if (nobs > 5) {
      hard_filter1 <- filtered_data$user.age == user_char[1]
      hard_filter2 <- filtered_data$user.gender == user_char[2] 
      hard_filter3 <- filtered_data$user.job == user_char[3]
    
      n1 <- nrow(filtered_data[hard_filter1,])
      n2 <- nrow(filtered_data[hard_filter2,])
      n3 <- nrow(filtered_data[hard_filter3,])

      if (n1 * n2 * n3 > 0) {
        m <- naiveBayes(rating ~ user.age + user.gender + user.job, data = filtered_data, laplace = 1)
        m$tables$user.age[is.na(m$tables$user.age)] = 0
        m$tables$user.gender[is.na(m$tables$user.gender)] = 0
        m$tables$user.job[is.na(m$tables$user.job)] = 0
    
        user <- data.frame(user.id = NA, item.id = NA, rating = NA, user.age = factor(user_char[1], levels = levels(data$user.age)), user.gender = factor(user_char[2], levels = levels(data$user.gender)), user.job = factor(user_char[3], levels = levels(data$user.job)))
        rating <- sum(predict(m, user, type="raw") * c(1:5), na.rm = TRUE)
        ratings[movie] = rating
        }
    }
  }
  return(ratings)
}



#Make age categorical
user.age.hist <- hist(u.user$user.age, breaks = c(0,16,20,25,30,37,45,55,100), freq=FALSE, plot=FALSE)
u.user.bis <- u.user
u.user.bis$user.age <- cut(u.user$user.age, breaks = user.age.hist$breaks, labels = c(1:8))

#Make global dataframe for computing models
nb_df <- merge(x=u.data, y=u.user.bis, by="user.id")
nb_df <- nb_df[,!names(nb_df) %in% c("user.zip", "timestamp")]
nb_df <- as.data.frame(lapply(nb_df, factor))

nmovies = length(levels(nb_df$item.id))
user.char = c(3,"F","healthcare")
predictedVotes <- predictRatings(user.char, nb_df, nmovies)

if (sum(predictedVotes == rep(0, nmovies)) > 0) {
  u.item[u.item$movie.id %in% max.nindex(predictedVotes, 10), "movie.title"]
} else {
    print("Error : no prediction could be made for this features combination")
}

predictedVotes
