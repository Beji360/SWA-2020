# SWA-2020
if(require("pacman")){
  library(pacman)
}else{
  install.packages("pacman")
  library(pacman)
}
pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
               parallel, dplyr, plotly, tidyverse, reticulate, UsingR, Rmpfr,
               swirl, corrplot, gridExtra, mise, latex2exp, tree, rpart, lattice,
               coin, primes, epitools, maps, clipr, ggmap, twitteR, ROAuth,
               tm, rtweet, base64enc, httpuv, SnowballC, RColorBrewer, wordcloud, ggwordcloud)

#1/Use rtweet library to search and download 1000 tweets written in English mention your Company’s name. Ignore retweets while searching.
n <- 1000
tweets.company <- search_tweets(q = 'Bethesda', n = n, token = tk,
                                include_rts = FALSE)
save(tweets.company, file = "~/Documents/WSU2020/Social web/Download_1.Rdata")

#2/Find the follower count (save in variable x) and friend count (save in variable y) of each twitter user in (1)
class(tweets.company)

names(tweets.company)
class(tweets.company$followers_count)
library(tidyverse)
name  <- tweets.company$name
users <- unique(name)
x <- tweets.company$followers_count
(x     <- x[!duplicated(name)]) %>% length()


#3/Calculate the average followers_count ( save as xbar) and the average friends_count (save as. ybar) of your sample

#8.3 Building networks
user = lookup_users(c("bethesda")) #examine all of the details for Bethesda
names(user)
user$screen_name

user$friends_count
user$followers_count

t <- get_friends("bethesda") #gets user id of friends of Bethesda
names(t)
#Question24: Find the 10 most popular friends of Bethesda
friends = lookup_users(t$user_id)

friendPosition = order(friends$followers_count, decreasing = TRUE)[1:10] 
friendPosition

topFriends = friends[friendPosition,] #ids of top 10 friends
topFriends
topFriends$screen_name
#Result ques24: "jimmyfallon"     "PlayStation"     "SHAQ"            "Xbox"            "markiplier"      "RockstarGames"  "NintendoAmerica" "IGN"             "Twitch"          "EA"  
