##########
## knn ###
##########


setwd("D:/econ695_data/project") # Set the work dictionary

library(knitr)
library(dplyr)
library(stargazer)
library(data.table)
library(ggplot2)
library(lubridate)
library(caret)
library(tidyverse)
library(h2o)
library(tidyr)
library(class)

movies<- read.csv("movies.csv")
ratings<- read.csv("ratings.csv")

MovieLens_0 <- left_join(ratings, movies, by = "movieId")
MovieLens_0 <- na.omit(MovieLens_0)

MovieLens <- MovieLens_0 %>% mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "GMT"))
MovieLens$timestamp <- format(MovieLens$timestamp, "%Y")
names(MovieLens)[4] <- "year_rated"
MovieLens<- MovieLens %>% mutate(year = as.numeric(str_sub(title,-5,-2)))

set.seed(1)

n = dim( MovieLens )[1]
ti= sample( 1:n, n/10 )
#<- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

train_m <- MovieLens[-ti,]
test_m <- MovieLens[ti,]

# To make sure we don¡¯t include users and movies in the test set that do not appear in the training set, we remove these entries using the semi_join function:

validation <- test_m %>% 
  semi_join(train_m, by = "movieId") %>% 
  semi_join(train_m, by = "userId")

# Add rows removed from 'validation' set back into train set

removed_m <- anti_join(test_m, validation)
train_m <- rbind(train_m, removed_m)

n2 = dim(train_m)[1]
ti2= sample( 1:n2, n2/2 )
train.x <- train_m[-ti2,]
test_n <- train_m[ti2,]

# To make sure we don¡¯t include users and movies in the test set that do not appear in the training set, we remove these entries using the semi_join function:

test.x <- test_n %>% 
  semi_join(train.x, by = "movieId") %>%
  semi_join(train.x, by = "userId")

# Add rows removed from test set back into train set

removed_n <- anti_join(test_n, test.x)
train.x <- rbind(train.x, removed_n)

n3 = dim(split_train)[1]
ti3= sample( 1:n3, n3/2 )
split_train.x <- split_train[-ti3,]
split_test_n <- split_train[ti3,]

# To make sure we don¡¯t include users and movies in the test set that do not appear in the training set, we remove these entries using the semi_join function:

split_test.x <- split_test_n %>% 
  semi_join(split_train.x, by = "movieId") %>%
  semi_join(split_train.x, by = "userId")

# Add rows removed from test set back into train set

removed_n2 <- anti_join(split_test_n, split_test.x)
split_train.x <- rbind(split_train.x, removed_n2)

train.x <- na.omit(train.x)
test.x <- na.omit(test.x)
validation <- na.omit(validation)

# Define Root Mean Squared Error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2,na.rm= TRUE))
}





train.x <- select(train.x,-c(title,genres,year,year_rated))
test.x <- select(test.x,-c(title,genres,year,year_rated))

movie.x <- tidyr::spread(train.x ,movieId, rating)
movie.test <-tidyr::spread(test.x ,movieId, rating)

recommender <- movie.x
recommender[is.na(recommender)]<-0

movie.test2 <-movie.test
movie.test2[is.na(movie.test2)]<-0


library(FNN)

KNN.Pred = knn( train = movie.x, test = movie.test2, k = 3)
table(KNN.Pred, Direction2005)