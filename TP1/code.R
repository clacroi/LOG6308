#0.

# imported packages
library(Matrix)

# Utility functions
# Cosinus entre un vecteur v et chaque colonne dela matrice m
cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }

# Trouve les indexes des premières 'n' valeurs maximales d'une matrice
max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

# Compute RMSR between two vector excluding NAs from computation
rmse <- function(a, b) {
  c <- a
  d <- b
  c[which(is.na(a))] = 0
  c[which(is.na(b))] = 0
  d[which(is.na(b))] = 0
  d[which(is.na(a))] = 0
  
  sqrt(mean((c - d)^2))
}

# Integrating data
u.user <- read.csv(file='/home/corentin/Documents/Polytechnique/Systèmes de recommandation/TP1/data/u.user.csv', sep='|', header=T)
u.item <- read.csv(file='/home/corentin/Documents/Polytechnique/Systèmes de recommandation/TP1/data/u.item.csv', sep='|', header=T)
u.data <- read.csv(file='/home/corentin/Documents/Polytechnique/Systèmes de recommandation/TP1/data/u.data.csv', sep='|', header=T)

#1. Compute average rating by job and age

# Change u.user columns names before join operations 
colnames(u.user) <- c("user.id", "user.age", "user.gender", "user.job", "user.zip")

# Make dataframe combining u.data and u.user by Join operation on user.id column
fullData <- merge(x=u.data, y=u.user, by="user.id")

# Job
jobMergedDf <- aggregate(x=fullData[,"rating"], by=list(job = fullData$user.job), FUN=mean)
print(jobMergedDf)
plot(jobMergedDf$x, main="Average ratings by job", ylab="Average Rating", xlab="Job", xaxt="n")
axis(1, at=c(1:length(jobMergedDf$job)), labels=jobMergedDf$job, las=2, cex.axis=0.7, tck=-.01)

# Age
ageMergedDf <- aggregate(x=fullData[,"rating"], by=list(age = fullData$user.age), FUN=mean)
print(ageMergedDf)
plot(ageMergedDf$x, main="Average rating by age", ylab="Average Rating", xlab="Age", xaxt="n")
text(c(1:length(ageMergedDf$age)), ageMergedDf$x, ageMergedDf$age, cex=0.6, pos=4, col="blue")

#2. Compute 10 more similar movies to Star Trek using cosinus distance coeffcients and correlation coefficients between movies

# Make User-Item Matrix : a sparse matrix containing 0s and a dense matrix containing NAs
UIM.sparse <- sparseMatrix(i = u.data$user.id,
                           j = u.data$item.id,
                           x = u.data$rating)
rownames(UIM.sparse) <- paste('u', 1:nrow(UIM.sparse), sep='')
colnames(UIM.sparse) <- paste('i', 1:ncol(UIM.sparse), sep='')

# Make also a dense matrix from this UI sparse matrix
UIM <- as.matrix(UIM.sparse)
UIM[UIM == 0] <- NA

# Construct vector with cosinus distance between interest movie and all others
cosItem <- as.vector(cosinus.vm(UIM.sparse[,u.item[u.item$movie.title == "Star Trek V: The Final Frontier (1989)","movie.id"]], UIM.sparse))
corItem <- as.vector(cor(x=UIM.sparse[,450], y=as.matrix(UIM.sparse)))

# Take the 11 closest movies (including itself in fisrt position with a 1 distance)
# Cosinus distance
u.item[u.item$movie.id %in% sort(max.nindex(cosItem,11)),"movie.title"]
# Correlation coefficient "distance"
u.item[u.item$movie.id %in% sort(max.nindex(corItem,11)),"movie.title"]

#3. Use an item-item approach to predict for users who haven't rated Star Trek
#a. Compute vectors containing Euclidean Distance between Star Trek 
#and all ohters movies and compute sorted (by index number) list of 21 NN

n.voisins = 21

# With no votes being 0s in sparse UI matrix
distance.450 <- sqrt(colSums((UIM.sparse[,450] - UIM.sparse)^2))
i.distance.450.NN <- sort(min.nindex(distance.450, n.voisins))

# With no votes being NAs in dense UI Matrix
distance.na.450 <- sqrt(colSums((UIM[,450] - UIM)^2, na.rm=T))
i.distance.na.450.NN <- sort(min.nindex(distance.na.450, n.voisins))

# With distances being computed by R dist() function on sparse UI Matrix
#distance.dist.na.450 <- as.matrix(dist(t(UIM)))[450,]
#i.distance.dist.na.450.NN <- sort(min.nindex(distance.dist.na.450, n.voisins))

#with distances being computed by R dist() function on dense UI Matrix
#distance.dist.450 <- as.matrix(dist(t(UIM.sparse)))[450,]
#i.distance.dist.450.NN <- sort(min.nindex(distance.dist.450, n.voisins))

#b.Compute vectors of cosinus distance between ST and its 20 NN using i.distance.450 as the NN vector
# Compute cosinus coeffcients between Star Trek and its 20 Nearest Neighbors
cos.450.21N <- as.vector(cosinus.vm(UIM.sparse[,450], UIM.sparse[,i.distance.450.NN]))

# Extract the full votes vector for Star Trek
votes.450 <- UIM[,450]

# Make temporary matrix by filtering in UIM.sparse lines corresponding to users who haven't voted for ST and columns corresponding to the NN of Star Trek
tmp.mat <- as.matrix(UIM.sparse[which(is.na(votes.450)),i.distance.450.NN])

# Compute vector of cosinus weights sum for final predicted votes (cosinus coefficient for neighbors items that have not been rated by a user will not be taken into account in this sum)
weights.sum <- ifelse(tmp.mat == 0,0,1) %*% cos.450.21N

# Replace missing votes in votes.450 by predicted vote (as stated before the user must have rated at least 1 of the 20NN for its vote to be predicted : this is the case when the weight sum associated to a user is > 0)
predictedVotes.450 <- votes.450
predictedVotes.450[which(is.na(votes.450))][weights.sum > 0] <- as.vector(tmp.mat[weights.sum > 0,] %*% cos.450.21N) / weights.sum[weights.sum > 0]


#4.Compute quadratic error of the previous item-item approach

# We take the real 20 NN neighbors of Star Trek (excluding himself)
i.distance.450.NN <- tail(i.distance.450.NN, 20)

# We get a slice of the sparse UI Matrix containing only votes for the 20NN of ST
tmp.mat <- as.matrix(UIM.sparse[,i.distance.450.NN])

# We compute the vector of cosinus weights between ST and the other movies
cos.450.20N <- as.vector(cosinus.vm(UIM.sparse[,450], UIM.sparse[,i.distance.450.NN]))

# We compute for all users the sum of cosinus weights corresponding to movies that the users have rated
weights.sum <- ifelse(tmp.mat == 0,0,1) %*% cos.450.20N

# We compute the predicted votes for ST for users that have rated at least one of the Star Trek 20NN (i.e the cos weights sum is >0) 
predictedVotes.450 <- rep(0, nrow(UIM.sparse))
predictedVotes.450[weights.sum > 0] <- as.vector(tmp.mat[weights.sum > 0,] %*% cos.450.20N) / weights.sum[weights.sum > 0]

# We compute the RMSE of the prediction process for all users for which we predicted a vote and that have actually voted for Star Trek (i.e all users who haven't voted for ST OR didn't get a rating prediction for ST WON'T CONTRIBUTE to the rmse score thanks to my custom implementation of rmse between 2 vectors)
rmseScore <- rmse(predictedVotes.450, votes.450)
rmseScore

#5. Compute 10 best movies suggestions for a manually created user using a user-user and NN approach

# Construct a vector containing indexes of target movies
indices.starTrek <- grep("trek", as.character(u.item$movie.title), ignore.case=T)
indices.starWars <- c(172,181)
indices <- c(indices.starTrek, indices.starWars)

# Construct the vote vector for the user u
user.votes <- rep(0, ncol(UIM.sparse))
user.votes[indices.starTrek] = 5
user.votes[indices.starWars] = 1

# Compute vector containing common votes with database users
user.votes.communs <- ((UIM.sparse > 0) + 0) %*% ((user.votes > 0) + 0) # nombre de votes communs

# Compute distances between the constructed user and Database users and compute its 20 NN : for prediction efficiency only users that have 3 common votes with the constructed User can be in its nearest neighbors
distance.user <- sqrt(colSums((user.votes - t(UIM.sparse))^2))
i.distance.user.NN <- names(sort(distance.user[which(user.votes.communs >= 3)])[1:20])

# Compute inter-users cosinus coefficients (between the constructed User and its 20NN only)
cos.user.20NN <- as.vector(cosinus.vm(user.votes, t(UIM.sparse[i.distance.user.NN,])))

# Compute vector of cosinus weights sum between user and database users
tmp.mat <- as.matrix(UIM.sparse[i.distance.user.NN, ])
weights.sum <- ifelse(t(tmp.mat) == 0,0,1) %*% cos.user.20NN

# Make predictions for movies rated by at least 1 NN of the constructed User (weights.sum > 0) and not already rated by the construted user (all but indices)
user.predictedVotes <- rep(0, ncol(UIM.sparse))
user.predictedVotes[-c(which(weights.sum == 0), indices)] <- t(tmp.mat)[-c(which(weights.sum == 0), indices),] %*% cos.user.20NN / weights.sum[-c(which(weights.sum == 0), indices)]

# Get titles of the 10 best rated movies
u.item[u.item$movie.id %in% max.nindex(user.predictedVotes, 10), "movie.title"]

#6. Make 10 movies suggestions for new users based on users information (age, gender, job)

library(e1071)

# Make age categorical (8 different age classes)
user.age.hist <- hist(u.user$user.age, breaks = c(0,16,20,25,30,37,45,55,100), freq=FALSE, plot=FALSE)
u.user.bis <- u.user
u.user.bis$user.age <- cut(u.user$user.age, breaks = user.age.hist$breaks, labels = c(1:8))

# Make global dataframe for computing models
nb_df <- merge(x=u.data, y=u.user.bis, by="user.id")
nb_df <- nb_df[,!names(nb_df) %in% c("user.zip", "timestamp")]
nb_df <- as.data.frame(lapply(nb_df, factor))

# Function that returns predict votes for all movies given the characteristics of the input user. 0 votes in the returned vector means a vote could'nt be made

predictRatings <- function(user_char, data, nmovies) {
  ratings = rep(0, nmovies)
  
  # We try to predict ratings for all movies
  for (movie in c(1:nmovies)) {
    movie_filter <- data$item.id == movie
    filtered_data <- data[movie_filter,]
    nobs <- nrow(data[movie_filter,])
    
    # If there is not enough observations, we chose to not make predictions for the movies
    if (nobs > 5) {
      # The following integers are the number of observations (= ratings) whose user share the same age, gender and job with our target user. If one of this number is 0, we chose to not make predictions for this movie.
      n1 <- nrow(filtered_data[filtered_data$user.age == user_char[1],])
      n2 <- nrow(filtered_data[filtered_data$user.gender == user_char[2],])
      n3 <- nrow(filtered_data[filtered_data$user.job == user_char[3],])
      
      if (n1 * n2 * n3 > 0) {
        # We build the model with alpha in Laplace approximation being 1
        m <- naiveBayes(rating ~ user.age + user.gender + user.job, data = filtered_data, laplace = 1)
        
        # We have to set NaN values in conditional tables of the models by 0s, otherwise the rating computation will give NaN
        m$tables$user.age[is.na(m$tables$user.age)] = 0
        m$tables$user.gender[is.na(m$tables$user.gender)] = 0
        m$tables$user.job[is.na(m$tables$user.job)] = 0
        
        # We make a single row Dataframe for our target user
        user <- data.frame(user.id = NA, item.id = NA, rating = NA, user.age = factor(user_char[1], levels = levels(data$user.age)), user.gender = factor(user_char[2], levels = levels(data$user.gender)), user.job = factor(user_char[3], levels = levels(data$user.job)))
        
        # The predicted rating is not the most probable rating but the probability-weighted average of all possible ratings 
        rating <- sum(predict(m, user, type="raw") * c(1:5), na.rm = TRUE)
        ratings[movie] = rating
      }
    }
  }
  return(ratings)
}

# Test code
nmovies = length(levels(nb_df$item.id))
user.char = c(3,"F","healthcare")
predictedVotes <- predictRatings(user.char, nb_df, nmovies)

# If some predictions have been made, print 10 best suggestions otherwise print an error message
if (sum(predictedVotes == rep(0, nmovies)) > 0) {
  u.item[u.item$movie.id %in% max.nindex(predictedVotes, 10), "movie.title"]
} else {
  print("Error : no prediction could be made for this features combination")
}
