##0. Utility functions and data integration

min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }

load('/home/corentin/Documents/Polytechnique/Systèmes de recommandation/TPs/TP2/data.R')

##1. The PageRank algorithm used to rank references and references' references

pageRank <- function(m, d, n_iter) {
  N <- nrow(m)
  s <- colSums(m) + 1
  p = rep(1,N)
  
  for (k in c(1:n_iter)) {
    p <- (1-d)/N * rep(1,N) + d *(m %*% (p/s))
  }
  return(p)
}

# Run the PageRank alogrithm on our data
d <- 0.85
p <- pageRank(m,d,100)

# References suggestions : rank the article references based on their PageRank scores
refs <- names(which(m[,'422908'] > 0))
names(sort(p[refs,], decreasing = TRUE))

# References and references references suggestions : rank article references and references' references 
# based on their pageRank algorithm
# Compute S1 and S1' matrices
m2 <- m %*% m
# m3_ij is 1 if i belong to j references or j references' references
m3 <- ((m + m2) > 0) + 0
refs.refs <- names(which(m3[,'422908'] > 0))
names(sort(p[refs.refs,], decreasing = TRUE))

##2. An item-item approach to produce recommendation for the target article

n.voisins <- 51

# Euclidean distance based approach

distance.422908 <- sqrt(colSums((m[,'422908'] - m)^2))
i.distance.422908.NN <- min.nindex(distance.422908, n.voisins)
names(m[1,i.distance.422908.NN])

# Cosinus distance based approach

# Column based distance
distance.422908.cos <- as.vector(cosinus.vm(m[,'422908'], m))
distance.422908.cos[is.nan(distance.422908.cos)] <- 0
i.distance.422908.cos.NN <- max.nindex(distance.422908.cos, n.voisins)
names(m[1,i.distance.422908.cos.NN])

#Column and line based distances
distance.422908.cos.2 <- as.vector(cosinus.vm(m[,'422908'], m))
distance.422908.cos.2[is.nan(distance.422908.cos.2)] <- 0
distance.422908.cos.2 <- distance.422908.cos.2 + as.vector(cosinus.vm(m['422908',], t(m)))
distance.422908.cos.2[is.nan(distance.422908.cos.2)] <- 0
i.distance.422908.cos.2.NN <- max.nindex(distance.422908.cos.2, n.voisins)
names(m[1,i.distance.422908.cos.2.NN])

##3. Comparisons between item-item recommendations and PageRank algorithm

# Euclidean distance item-item approach

# Which recommended articles based on the Item-Item approach are references of 422908 ?
intersect(refs, names(m[1,i.distance.422908.NN]))
l = length(intersect(refs, names(m[1,i.distance.422908.NN])))/50
print(paste(l*100,"% of the 50 item item approach (with euclidean distance) recommended articles are in 42908 references"))

# Which recommended articles based on the Item-Item approach are references of 422908 or references' references ?
intersect(refs.refs, names(m[1,i.distance.422908.NN]))
l = length(intersect(refs.refs, names(m[1,i.distance.422908.NN])))/50
print(paste(l*100,"% of the 50 item item approach (with euclidean distance) recommended articles are in 42908 references or references'references"))

# Cosinus distance item-item approach

# Which recommended articles based on the Item-Item approach are references of 422908 ?
intersect(refs, names(m[1,i.distance.422908.cos.NN]))
l = length(intersect(refs, names(m[1,i.distance.422908.cos.NN])))/50
print(paste(l*100,"% of the 50 item item approach (with cosinus distance on columns vectors) recommended articles are in 42908 references"))
l = length(intersect(refs, names(m[1,i.distance.422908.cos.2.NN])))/50
print(paste(l*100,"% of the 50 item item approach (with cosinus distance on columns and lines vectors) recommended articles are in 42908 references"))

# Which recommended articles based on the Item-Item approach are references of 422908 or references references ?
intersect(refs.refs, names(m[1,i.distance.422908.cos.NN]))
l = length(intersect(refs.refs, names(m[1,i.distance.422908.cos.NN])))/50
print(paste(l*100,"% of the 50 item item approach (with cosinus distance columns vectors) recommended articles are in 42908 references or references'references"))
l = length(intersect(refs.refs, names(m[1,i.distance.422908.cos.2.NN])))/50
print(paste(l*100,"% of the 50 item item approach (with cosinus distance on columns and lines vectors) recommended articles are in 42908 references or references'references"))


# average absolute ranks difference comparisons

averageAbsoluteRankDifferential <- function(smallSuggSet, largeSuggSet) {
  ns <- length(smallSuggSet)
  nl <- length(largeSuggSet)
  
  avRankDiff <- 0
  for (k in c(1:ns)) {
    item <- smallSuggSet[k]
    # print(abs(which(largeSuggSet == item)-k))
    avRankDiff <- avRankDiff + abs(which(largeSuggSet == item)-k)
  }
  return(avRankDiff/ns)
}

# Differences between euclidean distance item item approach ranking and PageRank based ranking (on references only)
largeSuggSet <- names(m[1,order(distance.422908, decreasing=FALSE)])
smallSuggSet <- names(sort(p[refs,], decreasing = TRUE))
averageAbsoluteRankDifferential(smallSuggSet, largeSuggSet)

# Differences between cosinus distance item item approach ranking and PageRank based ranking (on references only)
largeSuggSet <- names(m[1,order(distance.422908.cos, decreasing=TRUE)])
smallSuggSet <- names(sort(p[refs,], decreasing = TRUE))
averageAbsoluteRankDifferential(smallSuggSet, largeSuggSet)

# Differences between cosinus distance item item approach ranking and PageRank based ranking (on references only)
largeSuggSet <- names(m[1,order(distance.422908.cos.2, decreasing=TRUE)])
smallSuggSet <- names(sort(p[refs,], decreasing = TRUE))
averageAbsoluteRankDifferential(smallSuggSet, largeSuggSet)

# Differences between cosinus distance item item approach ranking and 
# PageRank based ranking (on references and references' references)
largeSuggSet <- names(m[1,order(distance.422908.cos, decreasing=TRUE)])
smallSuggSet <- names(sort(p[refs.refs,], decreasing = TRUE))
averageAbsoluteRankDifferential(smallSuggSet, largeSuggSet)

# Differences between cosinus distance (on coluns and lines vectors) item item approach ranking and 
# PageRank based ranking (on references and references' references)
largeSuggSet <- names(m[1,order(distance.422908.cos.2, decreasing=TRUE)])
smallSuggSet <- names(sort(p[refs.refs,], decreasing = TRUE))
averageAbsoluteRankDifferential(smallSuggSet, largeSuggSet)
