
######### Imdb links ##########
library(rvest) # read more on https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
lego_movie <- read_html("http://www.imdb.com/title/tt0113277/")
description_cast_html = html_nodes(lego_movie , '#titleCast .itemprop span') 
description_cast <- html_text(description_cast_html)

description_cast = list(NA,nrow(links))
no_stars = 4;
description_cast = matrix(NA, nrow(links), no_stars) # we consider only first 4 stars
for (i in 1:nrow(links)){
  movie_page = read_html(paste("http://www.imdb.com/title/tt", links[i,2], sep=""))
  description_cast_html = html_nodes(movie_page , '#titleCast .itemprop span') 
  description_cast[i,] <- html_text(description_cast_html)[1:no_stars]
}
links = cbind(links, star = description_cast)
write.csv(links, file = "links_stars.csv", row.names=FALSE)