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

--8.3 Building networks--
names(tweets.company)
class(tweets.company$followers_count)
library(tidyverse)
name  <- tweets.company$name
users <- unique(name)
x <- tweets.company$followers_count
(x     <- x[!duplicated(name)]) %>% length()

#load(file = "bethesda.RData")
##examine all of the details for Bethesda
user = lookup_users(c("bethesda"), token = tk)
names(user)
user$screen_name

user$friends_count
user$followers_count

t <- get_friends("bethesda", token = tk)
names(t)
#Question24: Find the 10 most popular friends of Bethesda
friends = lookup_users(t$user_id, token = tk)

friendPosition = order(friends$followers_count, decreasing = TRUE)[1:10] 
friendPosition

topFriends = friends[friendPosition,] #ids of top 10 friends
topFriends
topFriends$screen_name

#Question 25: The egocentric graph 
library("igraph")
more.friends = list() #a place to store the friends of friends
#n = length(topFriends)
n= nrow(topFriends)
t = get_friends(topFriends$user_id[1]) #get friends of each friend 
more.friends[[1]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[2]) #get friends of each friend 
more.friends[[2]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[3]) #get friends of each friend 
more.friends[[3]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[4]) #get friends of each friend 
more.friends[[4]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[5]) #get friends of each friend 
more.friends[[5]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[6]) #get friends of each friend 
more.friends[[6]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[7]) #get friends of each friend 
more.friends[[7]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[8]) #get friends of each friend 
more.friends[[8]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[9]) #get friends of each friend 
more.friends[[9]]=lookup_users(t$user_id)
t = get_friends(topFriends$user_id[10]) #get friends of each friend 
more.friends[[10]]=lookup_users(t$user_id)
more.friends
class(more.friends[[1]])
dim(more.friends[[1]])
nrow(more.friends[[1]])


bethesda = rep(user$screen_name, 10) 
el = cbind(bethesda, topFriends$screen_name[1:10])  # bind the columns to create a matrix
el
#-----------------Reduce size of data----------------
for(a in 1:10){
  if(nrow(more.friends[[a]])>5){
    more.friends[[a]]=more.friends[[a]][1:13,]
  }
}
more.friends[[1]]$screen_name[1]
more.friends[[1]]$screen_name[2]
more.friends[[2]]$screen_name[2]
#save(user, friends, more.friends, file="bethesda.RData")

#write the function
user.to.edgelist <- function(user, friends) { 
  # create the list of friend screen names    
  user.name = rep(user$screen_name, nrow(friends))  # repeat user's name    
  el = cbind(user.name, friends$screen_name)  # bind the columns to create a matrix
  return(el) 
}
#Creating function user.to.edgelist to create the edge list for Bethesda:
el.bethesda = user.to.edgelist(user, friends)
el.bethesda
#Build the edge list for the top 10 friends using a loop:
for (a in c(1:length(more.friends))) {     
  el.friend = user.to.edgelist(topFriends[a,], more.friends[[a]])     
  el.bethesda = rbind(el.bethesda, el.friend)  # append the new edge list to the old one. 
}
el.bethesda
#After we have the edge list, we can create the graph:
g = graph.edgelist(el.bethesda)
g
#Reduce the vertex size and use a special plot layout:
plot(g, layout = layout.fruchterman.reingold, vertex.size = 15)

#This graph contains many vertices. To remove these, 
#let's only keep the vertices with degree (in or out) greater than 1.
g2=induced_subgraph(g, which(degree(g, mode = "all") >1))
#This graph is now easier to visualise:
plot(g2, layout = layout.fruchterman.reingold, vertex.size = 10)


