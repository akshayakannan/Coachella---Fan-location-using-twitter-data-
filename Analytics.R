install.packages("rtweet")
library(rtweet)
library(ggplot2)
library(dplyr)

#Trends
city <- get_trends("Boston")
trend <- city %>%
  group_by(trend) %>%
  summarize(vol = mean(tweet_volume))
trend <- arrange(trend, desc(vol)) 

# Extract tweets  
t <- search_tweets("Coachella", n = 10000, include_rts = F) 
t <- lat_lng(t)  
t <- na.omit(t[, c("lat", "lng", "country")])
colnames(t) <- c('lat', 'long', 'country')

#World map
w <- map_data("world")
world <- ggplot() +
  geom_polygon(data = w, aes(x = long,y = lat, group = group),
               fill = "grey",
               color = "white") 
world + coord_quickmap() +
  geom_point(data = t, aes(x = long, y = lat),
             size = 5, 
             color = "darkgreen",
             alpha = 0.6) +
  labs(title = 'World Map of Tweet Locations - Coachella') 

# US Map
u <- map_data("usa")
usa <- ggplot() +
  geom_polygon(data = u, aes(x = long, y = lat,  group = group),
               fill = "grey",
               color = "white") 

# Filter US tweets
t <- t %>% filter(country == "United States")
coach <- t
usa + coord_quickmap() +
  geom_point(data = coach,aes(x = long, y = lat),
             size = 5, 
             color = "darkgreen",
             alpha = 0.6) +
  labs(title = 'Map of Tweet Locations - Coachella') 
