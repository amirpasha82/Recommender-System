############################ Part 0 #####################################
###################### Reading and storing data #########################
#########################################################################

rm(list = ls())
setwd("/Users/amish913/Documents/Data Science project - Movies")
movies <- read.csv("movies.csv", header = T, sep = ",")
ratings <- read.csv("ratings.csv", header = T, sep = ",")
#tags <- read.csv("tags.csv", header = T, sep = ",")
links <- read.csv("links.csv", header = T, sep = ",")
links_stars <- read.csv("links_stars.csv", header = T, sep = ",")

############################# Part 1 ####################################
##### Data cleaning & preliminary analysis & extracting useful data #####
#########################################################################

library(tidyr)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(data.table)



### Here, we see that movieIDs in dataframes movies and ratings have different lengths.
### So we need to trim the one which is longer To do so, we do the following. 

a <- sort(unique(ratings$movieId))
b <- sort(unique(movies$movieId))
if (length(a) > length(b)){
  ind.diff <- setdiff(a,b)
} else {
  ind.diff <- setdiff(b,a)
  ind.diff.original <- which(movies$movieId %in% ind.diff)
  movies <- movies[-ind.diff.original,]
}

ratings[,4] = NULL # Not used 


#ratings = ratings[1:200,] # A portion of rating (for a quick test)
ratings_spread = as.matrix(spread(ratings, key = movieId, value = rating)) 
ratings_spread[,-1] = ratings_spread[,-1] - rowMeans(ratings_spread[,-1], na.rm = TRUE) # create user-item matrix (and deduct average ratings of users)
ratings_spread[is.na(ratings_spread)] = 0 # Set values corresponding to those movies that a user has not yet seen (rated) to zero
users = ratings_spread[,1]
ratings_spread = ratings_spread[,2:ncol(ratings_spread)] # remove user_id column


## Extracting geners --  Item (Genre) profile (movie~genre)
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
genres[genres == "(no genres listed)"] = NA

genres_separate <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres_separate) <- c(1:10)

decade_list <- c("1900s","1910s","1920s","1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","2010s")
genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western","IMAX")
genre_decade_list <- c(genre_list, decade_list)


## Extracting decades based on year of production
toMatch = as.character(1900:2016)
toMatch.year <- grep(paste(toMatch,collapse="|"), movies$title, value=TRUE)

year <- str_sub(movies$title,-5,-2)
year[which(grepl(paste(toMatch,collapse="|"), year) == F)] = NA
#year2 <- movies[which(is.na(movies$year) == F & is.na(movies$genres) == F),]

# cut years into decades
decade = cut(as.numeric(year), breaks = seq(1900,2020,10), labels = c("1900s","1910s","1920s","1930s","1940s","1950s","1960s","1970s","1980s","1990s","2000s","2010s"))

genre_decade_matrix <- matrix(0,nrow(genres_separate),length(genre_decade_list)) #empty matrix
for (r in 1:nrow(genre_decade_matrix)){
  ind.genre <- which(genre_list %in% genres_separate[r,])
  ind.decade <- which(genre_decade_list %in% decade[r])
  genre_decade_matrix[r,c(ind.genre,ind.decade)] <- 1
  #genre_decade_matrix[r,ind.decade] <- 1
}
colnames(genre_decade_matrix) <- genre_decade_list #set column names to genre list
genre_decade_matrix <- as.data.frame(genre_decade_matrix, stringsAsFactors=FALSE)

#tag = tolower(tags$tag)

## Word-cloud analysis 
genre_matrix = genre_decade_matrix[,1:19]
decade_matrix = genre_decade_matrix[,20:31]

freq_genre = colSums(genre_matrix)/sum(genre_matrix)
freq_decade = colSums(decade_matrix)/sum(decade_matrix)

wordcloud(genre_list, freq_genre*1000, colors = brewer.pal(5, "Dark2") ,  max.words = 1000, random.order = FALSE)
wordcloud(decade_list, freq_decade*100000, colors = brewer.pal(6, "Dark2") ,  max.words = 100000, random.order = T)



######### Imdb links ##########
#library(rvest) # read more on https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
#lego_movie <- read_html("http://www.imdb.com/title/tt0113277/")
#description_cast_html = html_nodes(lego_movie , '#titleCast .itemprop span') 
#description_cast <- html_text(description_cast_html)

#description_cast = list(NA,nrow(links))

#description_cast = matrix(NA, nrow(links), no_stars) # we consider only first 4 stars
#for (i in 1:nrow(links)){
#  movie_page = read_html(paste("http://www.imdb.com/title/tt", links[i,2], sep=""))
#  description_cast_html = html_nodes(movie_page , '#titleCast .itemprop span') 
#  description_cast[i,] <- html_text(description_cast_html)[1:no_stars]
#}
#links = cbind(links, star = description_cast)
#write.csv(links, file = "links_stars.csv", row.names=FALSE)


no_stars = 4; # Number of stars (aribitrary but should be in sync with tha of the webScrap_WriteCSV.R)
links_stars = read.csv("links_stars.csv", header = T, sep = ",")
links_stars = links_stars[movies$movieId,] # Trim links_stars s.t. it corresponds to the moviesID in the movies data frame

links_stars[,8:11] = NULL  # Corrected a bug (Not sure why columns 8:11 are repeated)

# Stack all stars into stars list 
stars = NULL 
for (i in 4: (4 + no_stars-1)){
  stars = c(stars, as.character(links_stars[,i]))
}
stars = unique(stars)


star_matrix <- matrix(0,nrow(links_stars), length(stars)) #empty matrix
tmp = as.matrix(links_stars[,4:7])
for (r in 1:nrow(star_matrix)){
  ind.stars <- which(stars %in% tmp[r,])
  star_matrix[r,ind.stars] <- 1
}

star_matrix <- as.data.frame(star_matrix, stringsAsFactors=FALSE)
names(star_matrix) = stars



# Limit the star list to the first top ones 
stars_list = sort(colSums(star_matrix), decreasing = T)[1:51] 
stars_list = stars_list[-which(is.na(names(stars_list)))] # remove NAs in the star list (This is due to the fact that IMDb )

# Use the data only for the top 50 actors 
star_matrix = star_matrix[,names(stars_list)]

# Wordcloud analysis of stars
freq_stars = colSums(star_matrix)/sum(star_matrix)
wordcloud(names(stars_list), freq_stars*10000, colors = brewer.pal(5, "Dark2") ,  max.words = 50, random.order = T)



genre_decade_star_matrix = cbind(genre_decade_matrix, star_matrix)


# Define a function that determines cosine similarity (in cosine or correlation sense) between two arrays (for further use)
cosine.sim <- function(x, y) {
  sim = sum(x*y)/(sqrt(sum(x^2)*sum(y^2)))
  return(sim)
}

############################### Part 2 ##################################
####### Item-based recommender system (Collaborative filtering) #########
#########################################################################

# Determine item similarity matrix
sim_matrix <- matrix(0,ncol(ratings_spread),ncol(ratings_spread)) # pre-allocate sim matrix
for (i in 1:ncol(ratings_spread)){
  for (j in i:ncol(ratings_spread)) {
     sim_matrix[i,j] = cosine.sim(ratings_spread[,i],ratings_spread[,j])
  }
}
# Copy the upper-triangular part of the matrix to its lower-triangular counterpart
sim_matrix = sim_matrix + t(sim_matrix)
diag(sim_matrix) = 1

# Recommandations for user 1 
user = 1
predicted_ratings = ratings_spread
ind_rated_movies = which(ratings_spread[user,] != 0)
ind_non_rated_movies = which(ratings_spread[user,] == 0)
for (i in 1:ncol(ratings_spread)){
  if (ratings_spread[user,i] == 0){
    predicted_ratings[user,i] = sum(ratings_spread[user,ind_rated_movies]*sim_matrix[i,ind_rated_movies])/sum(sim_matrix[i,ind_rated_movies])
  }
}
ind_rec1 = sort(predicted_ratings[user,ind_non_rated_movies],decreasing = T)[1:10]
rec1 = movies[movies$movieId %in% as.integer(names(ind_rec1)),]



############################### Part 3 ##################################
####### User-based recommender system (Collaborative filtering) #########
#########################################################################

# Determine user similarity matrix
sim_matrix_user <- matrix(0,nrow(ratings_spread),nrow(ratings_spread)) # pre-allocate sim matrix
for (i in 1:nrow(ratings_spread)){
  for (j in i:nrow(ratings_spread)) {
    sim_matrix_user[i,j] = cosine.sim(ratings_spread[i,],ratings_spread[j,])
  }
}
# Copy the upper-triangular part of the matrix to its lower-triangular counterpart
sim_matrix_user = sim_matrix_user + t(sim_matrix_user)
diag(sim_matrix_user) = 1

# Recommandations for user 1 
user = 1
predicted_ratings_user = ratings_spread
ind_rated_movies = which(ratings_spread[user,] != 0)
ind_non_rated_movies = which(ratings_spread[user,] == 0)
for (i in 1:ncol(ratings_spread)){
  if (ratings_spread[user,i] == 0){
    predicted_ratings_user[user,i] = sim_matrix_user[user,-user] %*% ratings_spread[-user,i]/sum(sim_matrix_user[user,-user])
  }
}

ind_rec2 = sort(predicted_ratings_user[user,ind_non_rated_movies],decreasing = T)[1:10]
rec2 = movies[movies$movieId %in% as.integer(names(ind_rec2)),]



############################### Part 4 ##################################
####### Content-based recommender system (by creatig user profile) ######
#########################################################################

ratings2 <- dcast(ratings, movieId~userId, value.var = "rating", na.rm=FALSE)
ratings2[,-1] = ratings2[,-1] - colMeans(ratings2[,-1], na.rm = TRUE) # reduce user average rating
ratings2[is.na(ratings2)] = 0 
ratings2 = ratings2[,-1] #remove movieIds col. Rows are movies, cols are userIds

## Creating user profile and calculate dot product for the user profile
user_profile = matrix(0,ncol(genre_decade_star_matrix),ncol(ratings2))
for (c in 1:ncol(ratings2)){
  for (i in 1:ncol(genre_decade_star_matrix)){
    # user_profile: how much a user rate for a specific genre in total.
    user_profile[i,c] <- sum((genre_decade_star_matrix[,i]) * (ratings2[,c])) # meaning: (Aggregated) user's opinion on a specific genre or a decade the movie is made, etc.
  }
}
rownames(user_profile) = genre_decade_list
colnames(user_profile) = paste("user", as.character(1:ncol(ratings2)), sep = " ")

## User similarities based on the user profile
sim_matrix_user_profile <- matrix(0,ncol(user_profile),ncol(user_profile)) # pre-allocate sim matrix
for (i in 1:ncol(user_profile)){
  for (j in i:ncol(user_profile)) {
    sim_matrix_user_profile[i,j] = cosine.sim(user_profile[,i],user_profile[,j])
  }
}
# Copy the upper-triangular part of the matrix to its lower-triangular counterpart
sim_matrix_user_profile = sim_matrix_user_profile + t(sim_matrix_user_profile)
diag(sim_matrix_user_profile) = 1 # set the diagonal entries to 1

# Recommandations for user 1 
user = 1
predicted_ratings_user_profile = ratings_spread
ind_rated_movies = which(ratings_spread[user,] != 0)
ind_non_rated_movies = which(ratings_spread[user,] == 0)
for (i in 1:ncol(ratings_spread)){
  if (ratings_spread[user,i] == 0){
    predicted_ratings_user_profile[user,i] = sim_matrix_user_profile[user,-user] %*% ratings_spread[-user,i]/sum(sim_matrix_user_profile[user,-user])
  }
}

ind_rec3 = sort(predicted_ratings_user_profile[user,ind_non_rated_movies],decreasing = T)[1:10]
rec3 = movies[movies$movieId %in% as.integer(names(ind_rec3)),]



