#Example of subsetting and adding a new column to a dataframe using base R.

library(tidyverse)

movies <- na.omit(get(data(movies)))

genre <- rep(NA, nrow(movies)) 
#create an empty vector based on number of rows

count <- rowSums(movies[, 18:24])
genre[which(count > 1)] = "Mixed Genre"
genre[which(count < 1)] = "None"
genre[which(count == 1 & movies$Action == 1)] = "Action"
genre[which(count == 1 & movies$Animation == 1)] = "Animation"
genre[which(count == 1 & movies$Comedy == 1)] = "Comedy"
genre[which(count == 1 & movies$Drama == 1)] = "Drama"
genre[which(count == 1 & movies$Documentary == 1)] = "Documentary"
genre[which(count == 1 & movies$Romance == 1)] = "Romance"
genre[which(count == 1 & movies$Short == 1)] = "Short"
movies$genre<-as.factor(genre)