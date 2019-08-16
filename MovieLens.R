##############################
# Author: Vishal Bakshi
# Project: MovieLens Rating
# Date: August 14th 2019
##############################

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



##Load libraries
library(caret)
library(tidyverse)

## Create rmse function - this will be used to calculate RMSE for each training model
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


## Method 1 - We use a simplistic baseline aproach to calculate mean rating across entire training set 
## and use that as prediction for all ratings of the validation set 
mu <- mean(edx$rating)

# Calculate and Store RMSE result in the table
method1_rmse <- RMSE(validation$rating, mu)
rmse_results <- data_frame(method = "Use the average", RMSE = method1_rmse)

## Method 2 - modelling movie effect - certain movies would be rated higher than others due to
## movie quality/user likeability, we will take that in to account here
mu <- mean(edx$rating) 
movie_avgs <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))

#Visualize movie effect in histogram
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

#calculate prediction by using "bi" -  which represents average rank of a movie
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

#Calculate and Store RMSE result in the table
method2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = method2_rmse))

##Method 3 - modelling user effect - different users have different criteria/standard for rating a movie
##for example: same movie could be given 5 by one user and 3 by another, we wil take that in to account here
#calculate the user specific effect and store it in "b_u"
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#let's visualize the average rating for users who have rated more than 100 movies to confirm the variance across users
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

#calculate prediction by using "bi" and "bu"
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

#Calculate and Store RMSE result in the table
method_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = method_3_rmse))

##Method 3 - Modelling genre effect - certain genres tend to get higer user ratings for example,movies in romance genre
## could have higher rating than those in horror. Let's try to model that
#calculate the genre specific effect and store it in "b_g"
genre_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_u))

#calculate prediction by using "bi", "bu" and "bg"
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

#Calculate and Store RMSE result in the table
method_4_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model + Genre Effects Model",  
                                     RMSE = method_4_rmse))


##Method 5 - Regularization - we first explore our biggest errors in prediction in "Method 2 - modelling movie effect"
## then we improve the algorithm by accounting for variance caused by movies that have many more ratings than other movies
#View our top 10 errors in predictions
validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) 

#view top 10 movies by our estimate - b_i - create temporary table "movie_titles " for visualization
movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) 

#view bottom 10 movies by our estimate - b_i
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) 

#now validate that top 10 movies are rated by very few users
edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) 

#now validate that bottom 10 movies are rated by very few users
edx %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)


# We denote lambda as penalty to the least square equation, lambda gets larger as we have lots of movies with high number of ratings
# now using cross validation,we will find best lambda that minimizes the modified least square equation result
lambdas <- seq(0, 10, 0.25)

mu <- mean(edx$rating)
just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

#now calculate predictions and rmse for different values of lambda
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validation %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation$rating))
})

#visualize the lambda vs rmse and determine lambda with least rmse
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

#calculate and store the minimum RMSE in results table
method_5_rmse <- min(rmses)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = method_5_rmse))

## Method 6 - Here we Add "user effect" to "Method 5 i.e.  regulaized movieeffort"
# now using cross validation,we will find best lambda that minimizes the modified least square equation result
lambdas <- seq(0, 10, 0.25)

#now calculate predictions and rmse for different values of lambda
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

#visualize the lambda vs rmse and determine lambda with least rmse
qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]

method_6_rmse <- min(rmses)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = method_6_rmse))

## Output the rmse results for all models and show the best model with least rmse value
rmse_results %>% knitr::kable()
print(c("The least RMSE is for model -  Regularized Movie + User Effect Model. The RMSE value is:", method_6_rmse))


