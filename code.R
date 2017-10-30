

############### 1. Text Retrival ###############

# Install and Activate Packages
#install.packages("twitteR", "RCurl", "RJSONIO", "stringr")
library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)

# Declare Twitter API Credentials
api_key <- "jt8JvXvvhDm0VlrfSGuKFsnn9" # From dev.twitter.com
api_secret <- "tzfFUZr5esq1zeStzDuzYf5u7kyoK41Jto9dEEtFRh9muCMv2T" # From dev.twitter.com
token <- "164629846-7molrihvswyFG0eY7YQoanU8fNZxV51DLL39lMUK" # From dev.twitter.com
token_secret <- "Qio7a95S1zgotg6AcirbumyyrHTAgezYn0o6FBex9hT77" # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

# Search Twitter 
tweets <- searchTwitter("Indian Railway", n=2500,lang="en")

# Transform tweets list into a data frame
tweets.df <- twListToDF(tweets)



############### 2. Text Analysis ###############
library(tm)

neg = scan("negative-words.txt", what="character", comment.char=";")
pos = scan("positive-words.txt", what="character", comment.char=";")




score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    tweet = tolower(tweet) # makes all letters lowercase
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching values for words from list 
    neg.matches = match(words, neg.words)  ## returns matching values for words from list 
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches) ## converts matching values to true of false
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}

## Analysis Resule ####
analysis = score.sentiment(tweets.df$text, pos, neg)

hist(analysis$score,main = "Tweet Analysis",xlab = "Tweet Score")
