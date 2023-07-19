#Question1
#a,b,c,d,e,f,g

library(dplyr)
library(recommenderlab)
data("MovieLense")
MovieLense
View(MovieLense)
head(MovieLense)

# 1. Implementing User-Based Recommender System using SVD (Singular Value Decomposition) method:

# a. Load the 'ratings' and 'movies' datasets which is a part of 'MovieLense'
read.csv("D:/data science/intellipat/datasets_r_programming/movies.csv")->movies
View(movies)

read.csv("D:/data science/intellipat/datasets_r_programming/ratings.csv")->ratings
View(ratings)

# b. From the 'ratings' dataset, extract only those observations where a user has
# rated atleast 131 movies & each movie has been rated more than 30 times.
df_movie_summary = ratings %>% group_by(movieId) %>% summarise(count = n(),mean(rating))
View(df_movie_summary)
movie_benchmark = round(quantile(df_movie_summary$count,0.70, names = FALSE),0)
movie_benchmark
drop_movie_list = df_movie_summary[df_movie_summary$count < movie_benchmark,1]

#To create a dataframe of all the movies rated less often
drop_movie_list = as.data.frame(drop_movie_list)
View(drop_movie_list)
#To create a list of all the inactive users(users who rate less often)

df_movie_summary1 = ratings %>% group_by(userId) %>% summarise(count = n(),mean(userId))


cust_benchmark = round(quantile(df_movie_summary1$count,0.70, names = FALSE),0)

cust_benchmark
drop_cust_list = df_movie_summary1[df_movie_summary1$count < cust_benchmark,1]

#To create a dataframe of the users who rate less often
drop_cust_list = as.data.frame(drop_cust_list)

#To remove all the movies rated less often and users who rate less often
ratings = anti_join(ratings, drop_movie_list, by = "movieId")
ratings = anti_join(ratings, drop_cust_list, by = "userId")
View(ratings)
# c. Create a rating matrix for the 'ratings' dataset and convert it to a
# 'realRatingMatrix' object and store it in 'ratingmat'
library(reshape2)
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds



#Convert rating matrix into a recommenderlab sparse matrix
library(recommenderlab)
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)


# d. Build the User-Based Collaborative Filtering model on the 'ratingmat' using
# SVD method & store the result in 'recommender_model'
recommender_model <- Recommender(ratingmat_norm, method = "SVD")

# e. Predict the ratings for the 5th user and store the result as a list in 'recom_list'.
recom <- predict(recommender_model, ratingmat[5], n=10) #Obtain top 10 recommendations for 10th user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

# f. Print the top 10 recommended movies for 5th user
recom_result <- matrix(0,10)

for (i in c(1:10)){
  recom_result[i] <- movies[as.integer(recom_list[[1]][i]),2]
}

recom_result
# g. Print evaluation metrics for the SVD model using the 5-fold cross-validation
# for top 1,3,5, and 10 recommendations
evaluation_scheme <- evaluationScheme(ratingmat, method="cross-validation", k=5, given=3, goodRating = 5) #k=3 meaning a 3-fold cross validation.

#To calculate evaluation metrics for top 1,3,5, and 10 recommendations
evaluation_results <- evaluate(evaluation_scheme, method="SVD", n=c(1,3,5,10))

eval_results <- getConfusionMatrix(evaluation_results)[[1]]

#To print the evaluation results

eval_results


