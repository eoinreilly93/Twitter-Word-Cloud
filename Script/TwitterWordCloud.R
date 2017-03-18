#############################
# SET UP TWITTER STUFF
#############################

#NEED DEVTOOLS TO INSTALL GITHUB
library(devtools)
library(tm)
library(httr)
#install_github("twitteR", username = "geoffjentry")
library(twitteR)
setup_twitter_oauth("vPWn7f3xgbZ1PxMikYnDDFgxl", "tQgOyOiVT6fIAQ2M5WCqYLZzC2TK0D5vGR3yme4BA78PTZq6KR", "543935337-AJRKzxra1AmLFAIYS8rxKQ4lLj0Iy9cq0p5BAjL2", 	"VSxyNFh594xefqy0WZCXhScMSxEvq05plQZ0I9I0iTele")

#########################
# ANALYSIS
########################

mht=userTimeline('realDonaldTrump',n=3200, includeRts=TRUE)
mht = strip_retweets(mht, strip_manual = TRUE, strip_mt = TRUE)
tweetsdf <- twListToDF(mht)


mach_text = sapply(mht, function(x) x$getText())


clean_text = clean.text(mach_text)
tweet_corpus = Corpus(VectorSource(clean_text))


tdm = TermDocumentMatrix(tweet_corpus,control = list(removePunctuation = TRUE,stopwords = c("machine", "learning", "the", "and", stopwords("english")),removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

library(wordcloud)
# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), max.words = 150)



clean.text <- function(some_txt)
{
  some_txt = gsub("&amp", "", some_txt)
  
  some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", some_txt)
  
  some_txt = gsub("@\\w+", "", some_txt)
  
  some_txt = gsub("[[:punct:]]", "", some_txt)
  
  some_txt = gsub("[[:digit:]]", "", some_txt)
  
  some_txt = gsub("http\\w+", "", some_txt)
  
  some_txt = gsub("[t]{2,}", "", some_txt)
  
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  # define "tolower error handling" function
  
  try.tolower = function(x)
    
  {
    
    y = NA
    
    try_error = tryCatch(tolower(x), error=function(e) e)
    
    if (!inherits(try_error, "error"))
      
      y = tolower(x)
    
    return(y)
    
  }
  
  some_txt = sapply(some_txt, try.tolower)
  
  some_txt = some_txt[some_txt != ""]
  
  names(some_txt) = NULL
  
  return(some_txt)
  
}


#FILTER BY DATE
tweetsdf$created <- format(as.Date(tweetsdf$created,format="%Y-%m-%d"))
tweetsdf <- subset(tweetsdf, created >= as.Date("2016-11-09"))


