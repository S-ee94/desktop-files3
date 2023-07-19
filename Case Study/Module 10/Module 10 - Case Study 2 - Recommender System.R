#Question1
#a, b, c, d, e, f
library(recommenderlab)
data("MovieLense")
MovieLense
MovieLense[rowCounts(MovieLense)>100,colCounts(MovieLense)>120]-> sample_movie
sample(x=c(T,F),size=nrow(sample_movie), replace = T,prob = c(0.7,0.3))-> split_movie
sample_movie[split_movie,]-> train_set
sample_movie[!split_movie]-> test_set
Recommender(data = train_set, method="UBCF")-> ubcf_model
n_recommended_ubcf<- 10
predict(object = ubcf_model, newdata=test_set, n=n_recommended_ubcf)-> recc_predicted_ubcf
recc_predicted_ubcf
recc_predicted_ubcf@items[[3]]-> user3_movie_numbers
user3_movie_numbers
recc_predicted_ubcf@itemLabels[user3_movie_numbers]
recc_predicted_ubcf@items[[5]]-> user5_movie_numbers
user5_movie_numbers
recc_predicted_ubcf@itemLabels[user5_movie_numbers]



#Question2
#a,b,c
Recommender(data = train_set, method="IBCF")-> ibcf_model
n_recommended_ibcf<- 12
predict(object = ibcf_model, newdata=test_set, n=n_recommended_ibcf)-> recc_predicted_ibcf
recc_predicted_ibcf
recc_predicted_ibcf@items[[2]]-> user2_movie_numbers
user2_movie_numbers
recc_predicted_ibcf@itemLabels[user2_movie_numbers]
recc_predicted_ibcf@items[[4]]-> user4_movie_numbers
user4_movie_numbers
recc_predicted_ibcf@itemLabels[user4_movie_numbers]






