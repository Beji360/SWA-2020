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








#Question 13 :create a term document matrix. Use TFIDF weights in your analysis and find how many documents were empty following the TFIDF process.
#Create the term-document matrix



##------------1.1--- Preliminaries-------------------------


library("rtweet")
library("tm")
library("SnowballC")
library('stopwords')





# Load thee files

load("~/Social Web Analytic/Social web 2020/Assignment/SWA-2020/project/bethesda.RData")

load("~/Social Web Analytic/Social web 2020/Assignment/SWA-2020/project/Download_1.Rdata")




tweets <- rbind(tweets.company)


corpus = Corpus(VectorSource(tweets$text)) # create a corpus from tweet text
corpus = tm_map(corpus, function(x) iconv(x, to='ASCII')) # convert characters to ASCII
corpus = tm_map(corpus,
                content_transformer(function(x) gsub("@\\w+",
                                                     "", x))) #remove character '@'
corpus = tm_map(corpus, removeWords, stopwords('english')) # remove common stopwords
corpus = tm_map(corpus,
                content_transformer(function(x) gsub("
(f|ht)tp(s?)://\\S+", "", x))) #remove URL's
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, tolower) # convert all letters to lower case

#Create the term-document matrix

tdm = TermDocumentMatrix(corpus)

tweet.dtm = DocumentTermMatrix(corpus)
tweet.wdtm = weightTfIdf(tweet.dtm)
tweet.matrix = as.matrix(tweet.wdtm)

#-----------------weightTfIdf  empty document Results  ----------------



Warning message:
In weightTfIdf(tweet.dtm) :
  empty document(s): 1 2 3 4 6 12 13 16 19 20 21 22 26 28 31 35 41 45 49 52 55 56 58 62 63 71 81 83 84 88 89 90 91 92 93 94 100 103 110 111 112 117 127 128 144 145 153 156 158 162 171 173 175 180 181 184 208 209 212 213 216 225 226 227 228 231 258 261 262 263 266 267 269 273 275 277 285 286 287 289 293 298 302 308 311 312 314 315 319 325 326 327 328 329 330 336 337 338 342 343 344 347 353 354 356 360 362 365 371 372 374 375 376 386 388 390 404 408 418 419 422 423 425 431 433 435 436 438 441 442 444 445 456 459 460 466 468 470 473 475 480 485 496 498 500 501 505 507 510 515 516 517 521 525 526 528 529 530 531 533 535 536 538 540 545 546 549 550 553 554 557 559 564 566 567 568 570 571 573 575 576 579 580 583 584 587 588 590 591 593 594 595 596 599 602 603 606 607 608 610 611 621 627 628 631 637 641 642 643 644 645 648 649 652 653 658 659 660 661 662 664 665 666 667 669 670 672 673 674 675 676 679 680 681 684 685 686 690 691 699 704 706 707 708 715 720 721 722 724 726 728 729 730 731 732 73 [... truncated]


  #observe the dim of tweet.matrix
  dim(tweet.matrix)

  #----------------- Results  ----------------  
  [1] 1000 2651

  tweet.matrix[1,1:10] #observe a column in tweet.matrix represents a term in tweets
                       #so each row resprsents a tweet


  #-----------------observe Results  ----------------                   

   bethesda              dear              give httpstcoomgbvjasb              just         morrowind
          0.1999615         1.4236835         0.8397602         1.4236835         0.6739795         1.1972603
              remak             along        betalaunch              come
          1.4236835         0.0000000         0.0000000         0.0000000


  ## remove empty tweets
  empties = which(rowSums(abs(tweet.matrix)) == 0)
  length(empties)

  #-----------------length Results  ----------------
  [1] 370

  if(length(empties)!=0){
    tweet.matrix = tweet.matrix[-empties,]
  }
  dim(tweet.matrix)

  #----------------- Results  ----------------
  [1]  630 2651




  #Question 14 :use the elbow method to find the appropriate number of clusters of themes among the combined tweets using cosine distance. Assume the range of clusters can be from 1 to 15.

  norm.tweet.matrix = diag(1/sqrt(rowSums(tweet.matrix^2))) %*% tweet.matrix
  ## then create the distance matrix
  D =dist(norm.tweet.matrix, method = "euclidean")^2/2
  #To visualise the clustering, we will use multidimensional
  #scaling to project the data into a 2d space
  ## perform MDS using 100 dimensions
  mds.tweet.matrix <- cmdscale(D, k=100)
  n = 15   #we assume elbow bends at 15 clusters  
  SSW = rep(0, n)
  for (a in 1:n) {
    ## use nstart to reduce the effect of the random initialisation
    set.seed(40)#seed for random number generator to ensure consistency in our results
    K = kmeans(mds.tweet.matrix, a, nstart = 20)
    SSW[a] = K$tot.withinss
  }


  ## plot the results
  plot(1:n, SSW, type = "b")





  #15. Find the number of tweets in each cluster assuming there are 2 to 6 clusters.

  #Examine the words associated with each cluster for cluster 2.
  cluster.number = 2
  ## find position of tweets in cluster
  clusterTweetsId = which(K$cluster == cluster.number)
  ## extract tweets vectors for cluster
  clusterTweets = tweet.matrix[clusterTweetsId,]
  ## combine the tweets into a mean tweet
  clusterTermWeight = colMeans(clusterTweets)
  ## show the top 10 weighted words
  sort(clusterTermWeight, decreasing = TRUE)[1:10]

  #----------------- Results  ----------------

   now       twitchstream             stream               game         livestream               live
          0.39267578         0.22941895         0.20118712         0.11706938         0.11262764         0.11097809
                play             twitch                pro httpstcoaagjlwesqz
          0.10736741         0.10663997         0.10331118         0.09965784



  #Examine the words associated with each cluster for cluster 3.
  cluster.number = 3
  ## find position of tweets in cluster
  clusterTweetsId = which(K$cluster == cluster.number)
  ## extract tweets vectors for cluster
  clusterTweets = tweet.matrix[clusterTweetsId,]
  ## combine the tweets into a mean tweet
  clusterTermWeight = colMeans(clusterTweets)
  ## show the top 10 weighted words
  sort(clusterTermWeight, decreasing = TRUE)[1:10]

  #----------------- Results  ----------------


        don      fait      pour     contr       dun    lutter    dollar   million     covid  gamergen
  0.6613061 0.6413803 0.6413803 0.5256265 0.5256265 0.5256265 0.4166372 0.3872357 0.3012453 0.2298919





  #Examine the words associated with each cluster for cluster 4.
  cluster.number = 4
  ## find position of tweets in cluster
  clusterTweetsId = which(K$cluster == cluster.number)
  ## extract tweets vectors for cluster
  clusterTweets = tweet.matrix[clusterTweetsId,]
  ## combine the tweets into a mean tweet
  clusterTermWeight = colMeans(clusterTweets)
  ## show the top 10 weighted words
  sort(clusterTermWeight, decreasing = TRUE)[1:10]


  #----------------- Results  ----------------


          job         bio    kellyjob kellyservic        hire        link       kelli     norwood     teacher         nih
    0.4450684   0.2709652   0.2504476   0.2504476   0.2338859   0.2183086   0.1918656   0.1735800   0.1735800   0.1699851



  #Examine the words associated with each cluster for cluster 5.
  cluster.number = 5
  ## find position of tweets in cluster
  clusterTweetsId = which(K$cluster == cluster.number)
  ## extract tweets vectors for cluster
  clusterTweets = tweet.matrix[clusterTweetsId,]
  ## combine the tweets into a mean tweet
  clusterTermWeight = colMeans(clusterTweets)
  ## show the top 10 weighted words
  sort(clusterTermWeight, decreasing = TRUE)[1:10]


  #----------------- Results  ----------------



       need       bad      know     thing      like   compani      feel      that    glitch      game
  0.3658184 0.3280644 0.2190968 0.1856761 0.1503963 0.1296856 0.1255923 0.1211779 0.1127601 0.1027036



  #Examine the words associated with each cluster for cluster 6.
  cluster.number = 6
  ## find position of tweets in cluster
  clusterTweetsId = which(K$cluster == cluster.number)
  ## extract tweets vectors for cluster
  clusterTweets = tweet.matrix[clusterTweetsId,]
  ## combine the tweets into a mean tweet
  clusterTermWeight = colMeans(clusterTweets)
  ## show the top 10 weighted words
  sort(clusterTermWeight, decreasing = TRUE)[1:10]

  #----------------- Results  ----------------

             money            right httpstcobwemvbsn             mayb              fix            final            stole
         0.5228681        0.2445982        0.1660964        0.1634508        0.1625350        0.1383904        0.1307510
              game           wonder          compani
         0.1277921        0.1047603        0.1032391  





   16. Visualize your clustering in 2-dimensional vector space. Show each cluster in a different colour. Use different symbols for "the tweets that are greater than average friend count" and for "the tweets that are less than the friends count".



   mds2.tweet.matrix <- cmdscale(D, k=2)
   plot(mds2.tweet.matrix, col = K$cluster)

   mds2.tweet.matrix <- cmdscale(D, k=2)
   plot(mds2.tweet.matrix, col = K$cluster,pch =
          as.numeric(user$friends_count ==
                       "bethesda"))
   legend("topleft", c( "bethesda Tweets"), pch
      =c(1,0))
   legend("bottomright",
   legend = c("Cluster 1- Red", "Cluster 2- Green", "Cluster 3- Black" , "Cluster 4- Yellow" , "Cluster 5- Blue" , "Cluster 6- Pink" , "Cluster 7- Light Blue" , "Cluster 8- Grey"),
   col = c("red", "green", "black" , "Yellow", "Blue", "Pink", " Light Blue" ,"Grey"),
   lty= c(1,1),
   cex=1
   )


   #Question 17 :Comment on your visualization.


   The cluster visualization shows that most of the tweets are at the far bottom centre on the x-axis  between -0.1 and 0.1 with  few of the yellow ,black  and    light blue at the outliers clustered separately .

















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
