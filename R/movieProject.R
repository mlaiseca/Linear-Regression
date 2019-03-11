# Mario Laiseca-Ruiz
# Final Project

# installing rcompanion to transform data
if(!require(rcompanion)){install.packages("rcompanion")}

# importing the movie meta data data set
movie_metadata <- read.csv(file = "/Users/mario/Google Drive/MIS500/Final Project/current data set/the-movies-dataset/movies_metadata.csv", header = TRUE, stringsAsFactors = TRUE)

# cleans data by removing the entries that have revenue of 0

movie_metadata <- subset.data.frame(movie_metadata, revenue > 0 )


# creaing vectors of revenue and vote average
movie_revenue <- as.vector(movie_metadata$revenue)
plot(movie_revenue)

movie_ratings <- as.vector(movie_metadata$vote_average)
plot(movie_ratings)

movie_budget <- as.vector(movie_metadata$budget)
plot(movie_budget)

# cleans data and removes data that doesn't 


# getting samples from revenue and movie rating
X <- movie_ratings
Y <- movie_revenue

# historgram for movie ratings 
hist(X, prob=TRUE, col = "blue", border = "black", xlab = "movie ratings", main = "Histogram of Movie Ratings")
lines(density(X))

# histogram of revenue
hist(Y, prob=TRUE, col = "blue", border = "black", xlab = "revenue", main = "Histogram of Revenue")
lines(density(Y))

# returns skew
distribution_skewness <- function(my_sample){
  (mean(my_sample) - median(my_sample))/sd(my_sample)
}


# testing skew for X and Y
skew_X <- distribution_skewness(X)
skew_X
skew_Y <- distribution_skewness(Y)
skew_Y

# normalize data, this can be done manually of by using the rcompanion package
# the sample size must be between 3 and 3000
# raising a power to the data is our lambda needed to make the data more normalized
# this package recommneds we raise our data to: x ^ 0.125



Y_3000_sample <- sample(Y, 3000, replace=TRUE)
T_tuk = transformTukey(Y_3000_sample,plotit=FALSE)
plotNormalHistogram(T_tuk)
Y <- Y_3000_sample
# set the lambda
myLambda <- 0.15
# set the transformation
Y <- Y^myLambda
hist(Y)

X_3000_sample <- sample(X, 3000, replace=TRUE)
T_tuk = transformTukey(X_3000_sample,plotit=FALSE)
plotNormalHistogram(T_tuk)
X <- X_3000_sample
# set the lambda
myLambda <- 2.175
# set the transformation
X <- X^myLambda
hist(X)






# testing skew for X and Y
skew_X <- distribution_skewness(X)
skew_X
skew_Y <- distribution_skewness(Y)
skew_Y




# making a function that runs the linear regression and returns the correlation

sample_correlation <- function(vector_1, vector_2){
  X <- sample(vector_1, 100, replace=TRUE)
  Y <- sample(vector_2, 100, replace=TRUE)
  cor(X, Y, use = "complete.obs")
}

# calling the sample correlation
# correlation_result <- sample_correlation(X, Y)

# storing the fingins in a vector of size 100
sample_correlation_results <- rep(x=0, times = 100)

collecting_correlation_results <- function(){
  for (i in 1:100){
    print("correlation result for sample: ")
    sample_correlation_results[i] <- sample_correlation(X, Y)
    print(sample_correlation_results[i])
  }
  
  average_correlation = mean(sample_correlation_results)
  print("average correlation: ")
  average_correlation
  
  
}

# running the simulation
collecting_correlation_results()

# sample model 
sample_correlation(X, Y)
line_intercept<- lm(Y~X)
abline(line_intercept, lwd=2)


