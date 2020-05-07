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

#Build the edge list using:
bethesda = rep(user$screen_name, 10) 
el = cbind(bethesda, topFriends$screen_name[1:10])  # bind the columns to create a matrix
el
#-----------------Reduce size of data----------------
for(a in 1:10){
  if(nrow(more.friends[[a]])>12){
    more.friends[[a]]=more.friends[[a]][1:12,]
  }
}
more.friends[[1]]$screen_name[1]
more.friends[[1]]$screen_name[2]
more.friends[[2]]$screen_name[2]
#save(user, friends, more.friends, file="bethesda.RData")

#Write the function
user.to.edgelist <- function(user, friends) { 
#Create the list of friend screen names    
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

#Make graph easier to visualise: 
g1=induced_subgraph(g, which(degree(g, mode = "all") >1))
plot(g1, layout = layout.fruchterman.reingold, vertex.size = 10)

#Question 26: Compute the closeness centrality score
g1.centres=order(closeness(g1), decreasing=TRUE)
g1.centres
length(g1.centres)
#--Result 26--
#[1]   1   7   8   3   6   9  10  11  13  14  15   2   4   5  12  16  17  18  19  20  21  22  23  24  25  26  27  28  29
#[30]  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58
#[59]  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87
#[88]  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
#[117] 117 118 119 120 121 122 123 124 125 126 127 128

#Question 27: Top 3 most central people
g1[g1.centres][,1]#names of the centres
cl_vt<- c(names(g1[g1.centres][,1]),closeness(g1)[g1.centres])
cl_vt[1:4]
#--Result 27-- "bethesda"   "Xbox"   "EA"   "markiplier" 


