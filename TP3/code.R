library(Matrix)
library(RCurl)

# 0.a Utiliy functions

# This function returns a matrix containing cosinus distance between columns of m
cosinus.m <- function(m) {
  ans <- t(m) %*% m
  m.norms <- sqrt(colSums(m^2, na.rm=TRUE))
  ans <- ans/m.norms
  ans <- ans %*% diag(1/m.norms)
  return(ans)
}

cosinus.vv <- function(v1,v2) { 
  n1 <- sqrt(sum(v1^2))
  n2 <- sqrt(sum(v2^2))
  return(sum(v1 * v2)/(n1*n2))
}

# b. Data Integration

u.data <- read.csv(file='/home/corentin/Documents/Polytechnique/Systèmes de recommandation/TP1/data/u.data.csv', sep='|', header=T)
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')
m <- as.matrix(m)
p <- dim(m)[1] # Number of users
n <- dim(m)[2] # Number of items

## Construct matrices equivalent to m with NAs
m.na <- m
m.na[m==0] <- NA

# 1. Implement a naïve vote prediction methods and estimate rmse mae by cross validation :
# a. (cv_av_colmean) Set predicted rating for (user, item) to item average given rating
# b. (cv_av_rowmean) Set predicted rating for (user, item) to user average rating

indices <- which(m >= 1, arr.ind=T)
indices <- indices[sample(nrow(indices)),]

cv_av_colmean <- function(m, n_fold, observations) {
  
  n_obs <- nrow(observations)
  fold_size = n_obs %/% n_fold
  mae <- 0
  rmse <- 0
  
  # Compute columns means vector and na indices
  na_obs <- which(is.na(m), arr.ind=TRUE)
  
  for (k in seq(0, n_fold-1)) {
    
    print(paste("Computing Error for test set ", toString(k+1)))
    test_obs <- observations[seq(k*fold_size, ((k + 1) * fold_size - 1)),]
    train_obs <- observations[-seq(k*fold_size, ((k + 1) * fold_size - 1)),]
    
    # Compute items means with train observations only 
    # (ratings for test observations are set to NAN and not taken into account in average computation)
    m_train <- m
    m_train[test_obs] <- NA
    cM <- colMeans(m_train, na.rm=TRUE)
    
    # Compute matrix cotaining predicted vote (if possible) for test observations
    # and NAN values for all others (train observations, unobserved ratings, test observations
    # that couldn't be predicted because absence of corresponding item average rating)
    m_test.pred <- m
    m_test.pred[test_obs] <- cM[test_obs[,2]] # Set all test ratings to items means in the train matrix
    m_test.pred[train_obs] <- NA # Set all train ratings to NA in this prediction matrix
    
    # Compute number of effective predictions, mae and rmse  
    n_pred_votes <- length(m) - sum(colSums(is.na(m-m_test.pred), na.rm=TRUE))
    mae <- mae + sum(colSums(abs(m - m_test.pred), na.rm = TRUE))/n_pred_votes
    rmse <- rmse + sqrt(sum(colSums((m - m_test.pred)^2, na.rm = TRUE))/n_pred_votes)
  }
  mae <- mae/n_fold
  rmse <- rmse/n_fold
  return(list(mae_er=mae, rmse_er=rmse))
}

naive_errors_colmean <- cv_av_colmean(m.na, 5, indices)

# b.
cv_av_rowmean <- function(m, n_fold, observations) {
  
  n_obs <- nrow(observations)
  fold_size = n_obs %/% n_fold
  mae <- 0
  rmse <- 0
  
  # Compute columns means vector and na indices
  na_obs <- which(is.na(m), arr.ind=TRUE)
  
  for (k in seq(0, n_fold-1)) {
    
    print(paste("Computing Error for test set ", toString(k+1)))
    test_obs <- observations[seq(k*fold_size, ((k + 1) * fold_size - 1)),]
    train_obs <- observations[-seq(k*fold_size, ((k + 1) * fold_size - 1)),]
    
    # Compute items means with train observations only 
    # (ratings for test observations are set to NAN and not taken into account in average computation)
    m_train <- m
    m_train[test_obs] <- NA
    cR <- rowMeans(m_train, na.rm=TRUE)
    
    # Compute matrix cotaining predicted vote (if possible) for test observations
    # and NAN values for all others (train observations, unobserved ratings, test observations
    # that couldn't be predicted because absence of corresponding item average rating)
    m_test.pred <- m
    m_test.pred[test_obs] <- cR[test_obs[,1]] # Set all test ratings to items means in the train matrix
    m_test.pred[train_obs] <- NA # Set all train ratings to NA in this prediction matrix
    
    # Compute number of effective predictions, mae and rmse  
    n_pred_votes <- length(m) - sum(colSums(is.na(m-m_test.pred), na.rm=TRUE))
    mae <- mae + sum(colSums(abs(m - m_test.pred), na.rm = TRUE))/n_pred_votes
    rmse <- rmse + sqrt(sum(colSums((m - m_test.pred)^2, na.rm = TRUE))/n_pred_votes)
  }
  mae <- mae/n_fold
  rmse <- rmse/n_fold
  return(list(mae_er=mae, rmse_er=rmse))
}

naive_errors_rowmean <- cv_av_rowmean(m.na, 5, indices)

# 2. Apply SVD decomposition

# Svd decomposition with k = min(n,p)
m.svd <- svd(m)

# 3. Apply SVD decoposition with 10 latent factors

# We compute NA observations in m.na matrix and set its unobserved ratings to 
# the columns average (item average observed ratings)
na_obs <- which(is.na(m.na), arr.ind=TRUE)
cM <- colMeans(m.na, na.rm=TRUE)
m.normalized <- m.na
m.normalized[na_obs] <-cM[na_obs[,2]] 

# SVD decomposition with k = 10
m.svd.10 <- svd(m.normalized, nu=10, nv=10)

# Compute reconstruction of m
m.reconstructed <- m.svd.10$u %*% diag(m.svd.10$d[1:10]) %*% t(m.svd.10$v)

# Scale result to [0,5]
m.reconstructed[m.reconstructed > 5] <- 5
m.reconstructed[m.reconstructed < 0] <- 0

# 4. Compute Mean Absolute Error and Mean Square Error of the prediction process

n_votes <- sum(colSums(ifelse(m == 0,0,1)))
mae <- sum(colSums(abs(m.na - m.reconstructed), na.rm = TRUE))/n_votes
rmse <- sqrt(sum(colSums((m.na - m.reconstructed)^2, na.rm = TRUE))/n_votes)
print(paste("RMSE is ", rmse))
print(paste("MAE is ", mae))

#5. Compute optimal number of dimension

dims <- append(seq(2,min(n,p),5), c(min(n,p)))
m.svd <- svd(m.normalized)

rmse_vec <- rep(0,length(dims))
abser_vec <- rep(0,length(dims))

for (k in c(1:length(dims))) {
  dim = dims[k]
  m.reconstructed <- m.svd$u[,1:dim] %*% diag(m.svd$d[1:dim]) %*% t(m.svd$v[,1:dim]) # reconstruct original matrix
  
  # Scale votes to [1,5]
  m.reconstructed[m.reconstructed > 5] <- 5
  m.reconstructed[(m.reconstructed < 1) & (m.reconstructed > 0)] <- 1
  
  # Compute MAE and RMSE
  abser_vec[k] <- sum(colSums(abs(m.na - m.reconstructed), na.rm = TRUE))/n_votes
  rmse_vec[k] <- sqrt(sum(colSums((m.na - m.reconstructed)^2, na.rm = TRUE))/n_votes)
}

plot(dims, rmse_vec, main="RMSE of SVD Prediction method vs reconstruction dimension", ylab="RMSE", xlab="Dimension")
plot(dims, abser_vec, main="MAE of SVD Prediction method vs reconstruction dimension", ylab="MAE", xlab="Dimension")

#6. Compute optimal number of dimension by Cross Validation

cv_svd_colmean <- function(m, n_fold, observations, dims) {
  
  n_obs <- nrow(observations)
  fold_size = n_obs %/% n_fold
  rmse_vec <- rep(0, length(dims))
  abser_vec <- rep(0, length(dims))
  
  # Compute columns means vector and na indices
  na_obs <- which(is.na(m), arr.ind=TRUE)
  
  for (k in seq(0, n_fold-1)) {
    
    print(paste("Computing Error for test set ", toString(k+1)))
    test_obs <- observations[seq(k*fold_size, ((k + 1) * fold_size - 1)),]
    train_obs <- observations[-seq(k*fold_size, ((k + 1) * fold_size - 1)),]
    m_train <- m
    
    # Compute items means with train observations only
    tmp_m <- m
    tmp_m[test_obs] <- NA
    cM <- colMeans(tmp_m, na.rm=TRUE)
    rm(tmp_m)
    
    # Set all test ratings to items means in the train matrix
    m_train[test_obs] <- cM[test_obs[,2]]
    m_train[na_obs] <- cM[na_obs[,2]]
    
    # Set all restant NA values to 0 (ratings for movies unobserved in train observations)
    m_train[is.na(m_train)] <- 0
    
    # Compute SVD decomposition for the train matrix
    m_train.svd <- svd(m_train)
    
    # Predict test votes using SVD recompoistion with different number of dimensions
    for (j in c(1:length(dims))) {
      dim = dims[j]
      
      # Construct SVD recomposition based on dim dimensions
      m_train.rec <- m_train.svd$u[,1:dim] %*% diag(m_train.svd$d[1:dim]) %*% t(m_train.svd$v[,1:dim])
      
      #Construct matrix containing predicted vote for test observations 
      # and NA for all others (train observations or no observation)
      m_test.pred <- m_train.rec
      m_test.pred[train_obs] <- NA # set all train ratings to NA
      m_test.pred[na_obs] <- NA # set all unobserved ratings to NA
      m_test.pred[, is.na(cM)] <- NA # set all ratings corresponding to movies unobserved in train set to NA
      m_test.pred[m_test.pred > 5] <- 5 # scale all values > 5 to 5
      m_test.pred[m_test.pred < 1] <- 1 # scale all values < 1 to 1
      
      # Compute number of predicted votes, MAE and RMSE
      n_pred_votes <- length(m) - sum(colSums(is.na(m-m_test.pred), na.rm=TRUE))
      abser_vec[j] <- abser_vec[j] + sum(colSums(abs(m - m_test.pred), na.rm = TRUE))/n_pred_votes
      rmse_vec[j] <- rmse_vec[j] + sqrt(sum(colSums((m - m_test.pred)^2, na.rm = TRUE))/n_pred_votes)
    }
  }
  abser_vec <- abser_vec/n_fold
  rmse_vec <- rmse_vec/n_fold
  return(list(rmse=rmse_vec, abs=abser_vec))
}

# Test function with n_fold=5

n_fold <- 5
dims <- append(seq(2,30,1), seq(30, min(n,p), 30))
errors <- cv_svd_colmean(m.na, 5, indices, dims)
plot(dims, errors$rmse, main="RMSE of SVD Prediction method vs reconstruction dimension", ylab="RMSE", xlab="Dimension")
plot(dims, errors$abs, main="MAE of SVD Prediction method vs reconstruction dimension", ylab="MAE", xlab="Dimension")  

#7. Compare SVD methods with Collaborative item-item approach

cv_item_nn <- function(m, n_fold, observations, n_neighbors) {
  
  n_obs <- nrow(observations)
  fold_size = n_obs %/% n_fold
  mae <- 0.0
  rmse <- 0.0
  
  # Compute and na indices a
  na_obs <- which(is.na(m), arr.ind=TRUE)
  
  m.0s <- m
  m.0s[is.na(m)] <- 0.0
  
  # Compute matrix of common votes
  print("Computing common votes matrix...")
  movies.commonvotes <- ((t(m.0s) >= 1) + 0) %*% ((m.0s >= 1) + 0)
  diag(movies.commonvotes) <- rep(0,ncol(m))
  
  for (k in seq(0, n_fold-1)) {
    
    print(paste("Computing Error for test set ", toString(k+1)))
    test_obs <- observations[seq(k*fold_size, ((k + 1) * fold_size - 1)),]
    train_obs <- observations[-seq(k*fold_size, ((k + 1) * fold_size - 1)),]
    
    # Construct train matrix (filled with NAs or 0s)
    m_train <- m
    m_train[test_obs] <- NA
    m_train.0s <- m.0s
    m_train.0s[test_obs] <- 0.0
    
    # Compute columns average
    cM <- colMeans(m_train, na.rm=TRUE)
    
    # Compute distance matrix and commonvotes matrix with 0s diagonal
    print("Computing distance matrix...")
    dist_mat <- as.matrix(dist(t(m_train))) # on NAs matrix to get exact distance
    
    # Construct cosinus weights matrix with 0s on diagonal
    print("Computing cosinus weights matrix...")
    m_train.cos <- cosinus.m(m_train.0s) #on 0s matrix to avoid propagating NAs
    diag(m_train.cos) <- 0.0 # 
    
    # Set all weights m_train.cos[i1,i2] to 0 if i2 doesn't belong to i1 NN
    print("Reducing non neighbors cosinus weights matrix to 0...")
    for (movie in seq(1,ncol(m))) {
      dist_vector <- dist_mat[, movie]
      dist_vector[which(movies.commonvotes[, movie] < 3)] <- 10000 # set all distances between movie and i to 10000 if less than 4 common votes
      dist_vector <- sort(dist_vector)
      m_train.cos[tail(names(dist_vector), n=-n_neighbors), movie] <- 0.0
    }
    m_train.cos[is.na(m_train.cos)] <- 0.0 # cosinus.mm fct could have introduced NAs values resulting from 0 division
    
    # Compute MAE and RMSE for all observations
    n_pred_vote <- 0
    for (j in c(1:nrow(test_obs))) {
      user <- test_obs[j,1]
      item <- test_obs[j,2]
      # Compute 0/1 vector with 1 for item NN
      item.nn_movies <- ifelse(m_train.cos[,item] > 0,1,0)
      # Compute 0/1 vector for item user has voted for in train set
      user.votes <- ifelse(m_train.0s[user,] > 0,1,0)
      user.nnmovies_rated <- user.votes %*% item.nn_movies # the number of movies in item nn our user has given rating
      if (user.nnmovies_rated >= 2) {
        n_pred_vote <- n_pred_vote + 1
        pred_vote <- (m_train.0s[user,] %*% m_train.cos[,item])[1,1]
        pred_vote <- pred_vote/(user.votes %*% m_train.cos[,item])
        mae <- mae + abs(m.na[user,item]-pred_vote)
        rmse <- rmse + ((m.na[user,item]-pred_vote)^2)
        
      } else if (!(is.na(cM[item]))) {
        n_pred_vote <- n_pred_vote + 1
        pred_vote <- cM[item]
        mae <- mae + abs(m.na[user,item]-pred_vote)
        rmse <- rmse + ((m.na[user,item]-pred_vote)^2)
      } 
    }
    print(n_pred_vote)
    mae <- mae/n_pred_vote
    rmse <- sqrt(rmse/n_pred_vote)
  }
  mae <- mae/n_fold
  rmse <- rmse/n_fold
  return(list(mae_er=mae, rmse_er=rmse))
}

colfilter_errors <- cv_item_nn(m.na, 5, indices, 20)
